library(dplyr)
library(tidyr)
library(openxlsx)
library(httr2)
source("../Keeper/extras/KeeperEvaluation/ConvertOldToNewFormat.R")

cacheFolder <- "cacheKeeper"
dir.create(cacheFolder)

keeperFile <- "../keeperllmeval/KEEPER_results_all_redux.xlsx"
keeper <- read.xlsx(keeperFile) |>
  tibble() |>
  SqlRender::snakeCaseToCamelCaseNames() |>
  rename(cohortId = "cohortid", 
         cohortName = "cohortname",
         goldStandard = "adjudicated.case.status.(yes.=.true.positive,.no.=.false.positive,.i.don't.know.=.uncertain)")
keeper <- keeper |>
  select(-"personid") |>
  mutate(personId = as.character(row_number())) 
groups <- keeper |>
  group_by(cohortName) |>
  group_split()

settings <- Keeper::createPromptSettings()

instructions <- paste(readLines("InstructionsKeeper.txt"), collapse = "\n")

startTime <- Sys.time()
allResults <- list()
# group = groups[[1]]
for (group in groups) {
  message("Evaluating disease ", group$cohortName[1])
  newFormat <- convertKeeperTableToKeeper(group)
  
  instantiatedInstructions <- gsub("<disease>", group$cohortName[1], gsub("<definition>", "", instructions))
  instantiatedCriteria = list(
    "true" = gsub("<disease>", group$cohortName[1], 
                  "The preponderance of evidence suggests the patient likely had <disease> (e.g., specific treatments, consistent symptoms, or repeated diagnoses over time)."),
    "false" = gsub("<disease>", group$cohortName[1], 
                   "The preponderance of evidence suggests the patient likely did not have <disease> (e.g., it is more likely they were only being tested, the diagnosis was ruled out, or the codes were purely administrative).")
  )
  
  profiles <- newFormat |>
    group_by(generatedId) |>
    group_split()
  # profile = profiles[[1]]
  for (profile in profiles) {
    fileName <- file.path(cacheFolder, sprintf("person_%s.rds", profile$generatedId[1]))
    if (file.exists(fileName)) {
      parsed <- readRDS(fileName)
    } else {
      message("- Reviewing person ", profile$generatedId[1])
      payload <- list(
        state = Keeper:::createPrompt(settings, profile), 
        questions = list(
          has_disease = list( 
            type = "noul",
            instructions = instantiatedInstructions,
            criteria = instantiatedCriteria
          )
        )
      )
      payload$model <- "clef:27b"
      req <- request("http://localhost:11434/v1/systemone") |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(payload, auto_unbox = TRUE)
      resp <- req_perform(req)
      parsed <- resp_body_json(resp)
      saveRDS(parsed, fileName)
    }
    allResults[[length(allResults) + 1]] <- tibble(
      generatedId = profile$generatedId[1],
      diseaseName = group$cohortName[1],
      probability = parsed$answers$has_disease$noul
    )
  }
}
delta <- Sys.time() - startTime
message("Reviewing cases took ", round(delta, 1), " ", attr(delta, "units"))
# Reviewing cases took 36 mins

allResults <- bind_rows(allResults)

results <- allResults |>
  rename(personId = "generatedId") |>
  inner_join(keeper |>
               select("personId", "goldStandard"),
             by = join_by(personId)) |>
  mutate(goldStandard = tolower(goldStandard)) |>
  filter(!grepl("know", goldStandard)) |>
  mutate(goldStandard = goldStandard == "yes")

hist(results$probability)

# Plot calibration -------------------
library(ggplot2)

plotCalibration <- function(joined) {
  calibration_data <- joined %>%
    mutate(correct_num = as.numeric(goldStandard)) %>%
    mutate(bin = cut(probability, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarize(
      mean_predicted = mean(probability, na.rm = TRUE),
      observed_accuracy = mean(correct_num, na.rm = TRUE),
      sample_size = n(),
      .groups = "drop"
    )
  
  ggplot(calibration_data, aes(x = mean_predicted, y = observed_accuracy)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray") +
    geom_line(color = "#2c3e50", size = 1) +
    geom_point(aes(size = sample_size), color = "#3498db", alpha = 0.8) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    labs(
      title = "Model Calibration Curve",
      subtitle = "Points closer to the dashed line indicate better calibration",
      x = "Predicted Probability",
      y = "Observed Accuracy",
      size = "N (Bin Size)"
    ) +
    theme_minimal()
}

plotCalibration(results)
ggsave("CalibrationKeeperCef27b.png", width = 5, height = 4)


results |>
  mutate(range = case_when(probability < 0.1 ~ "No",
                           probability > 0.9 ~ "Yes",
                           TRUE ~ "Maybe")) |>
  group_by(range) |>
  summarise(p = mean(goldStandard),
            fraction = n() / nrow(results))
# range     p fraction
# Maybe 0.372   0.558 
# No    0.429   0.0213
# Yes   0.964   0.421 
