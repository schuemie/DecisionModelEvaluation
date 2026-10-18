library(dplyr)
library(httr2)
library(jsonlite)

cacheFolder <- "cachePhenelopeLocal"
dir.create(cacheFolder)
useLocal <- TRUE

rules <- paste(readLines("RulesPhenelope.txt"), collapse = "\n")

# group = groups[[3]]
findMatch <- function(group) {
  cacheFileName <- file.path(cacheFolder, sprintf("%s.rds", gsub("[^[:alnum:]]", "_", group$targetName[1])))
  if (file.exists(cacheFileName)) {
    rows <- readRDS(cacheFileName)
    return(rows)
  } else {
    message(sprintf("Processing '%s'", group$targetName[1]))
    questionsList <- list()
    answersList <- list()
    for (i in 1:nrow(group)) {
      key <- paste0("candidate_", i)
      value <- list(
        type = "noul",
        instructions = sprintf("Evaluate the candidate '%s'. Should it be included in the concept set based on the definition and rules in the state?",
                               group$conceptName[i]),
        criteria = list(
          "true" = "Meets the rules and >95% of instances fall under the concept set.",
          "false" = "Violates the rules, requires assuming unstated details, or is too broad."
        )
      )
      questionsList[[key]] <- value
      answersList[[key]] <- tibble(conceptId = group$conceptId[i],
                                   conceptName = group$conceptName[i])
    }

    payload <- list(
      state = list(
        concept_set_name = group$targetName[1],
        clinical_definition = group$targetDefinition[1],
        rules = rules
      ),
      questions = questionsList
    )
    if (useLocal) {
      payload$model <- "clef:27b"
      req <- request("http://localhost:11434/v1/systemone") |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(payload, auto_unbox = TRUE)
    } else {
      payload$model <- "jev-1.13.0"
      req <- request("https://api.typesafe.ai/v1/systemone") |>
        req_headers("Content-Type" = "application/json") |>
        req_auth_bearer_token(Sys.getenv("TYPESAFE_KEY")) |>
        req_body_json(payload, auto_unbox = TRUE)
    }
    resp <- req_perform(req)
    parsed <- resp_body_json(resp)

    answerToRow <- function(key, answers) {
      row <- answersList[[key]] |>
        mutate(probability = answers[[key]]$noul)
      return(row)
    }
    rows <- lapply(names(parsed$answers), answerToRow, answers = parsed$answers)
    rows <- bind_rows(rows) |>
      mutate(targetName = group$targetName[1])
    saveRDS(rows, cacheFileName)
    return(rows)
  }
}

concepts <- ConceptSetConstructionEvaluation::getConceptsForAdjudication()
groups <- concepts |>
  group_by(targetName) |>
  group_split()
rows <- lapply(groups, findMatch)
rows <- bind_rows(rows)



# rows <- lapply(list.files(cacheFolder, full.names = TRUE), readRDS)
# rows <- bind_rows(rows)

hist(rows$probability)
goldStandard <- readr::read_csv(system.file("goldStandard",
                                            "ConceptAdjudicationGoldStandard.csv",
                                            package = "ConceptSetConstructionEvaluation"),
                                show_col_types = FALSE) |>
  mutate(goldStandard = goldStandard == "TRUE")
jebVsGs <- rows |>
  inner_join(goldStandard |>
               select("targetName", "conceptId", "goldStandard"),
             by = join_by(conceptId, targetName))



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

plotCalibration(jebVsGs)
ggsave("CalibrationPhenelopeClef27b.png", width = 5, height = 4)

# Plot AUC -------
roc_obj <- pROC::roc(jebVsGs$goldStandard, jebVsGs$probability)
pROC::ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve using ggroc", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()
pROC::auc(roc_obj)
# Area under the curve: 0.9782

# Evaluate -----------------
rows <- rows |>
  mutate(adjudication = if_else(probability > 0.5, "YES", "NO"))
ConceptSetConstructionEvaluation::evaluateConceptAdjudication(rows)
# Short rules:
# targetDomain    tp    fp    tn    fn   ppv sensitivity specificity
# CONDITION       75     7   485    33 0.915       0.694       0.986
# MEASUREMENT     42     0   233    25 1           0.627       1
# PROCEDURE       56     5   310    29 0.918       0.659       0.984
# All            173    12  1028    87 0.935       0.665       0.988

# Original rules:
# targetDomain    tp    fp    tn    fn   ppv sensitivity specificity
# CONDITION       82    13   479    26 0.863       0.759       0.974
# MEASUREMENT     45     3   230    22 0.938       0.672       0.987
# PROCEDURE       62     7   308    23 0.899       0.729       0.978
# All            189    23  1017    71 0.892       0.727       0.978

rows <- rows |>
  mutate(adjudication = if_else(probability > 0.25, "YES", "NO"))
ConceptSetConstructionEvaluation::evaluateConceptAdjudication(rows)

# targetDomain    tp    fp    tn    fn   ppv sensitivity specificity
# CONDITION       89    25   467    19 0.781       0.824       0.949
# MEASUREMENT     54     9   224    13 0.857       0.806       0.961
# PROCEDURE       73    12   303    12 0.859       0.859       0.962
# All            216    46   994    44 0.824       0.831       0.956

jebVsGs |>
  mutate(range = case_when(probability < 0.1 ~ "No",
                           probability > 0.9 ~ "Yes",
                           TRUE ~ "Maybe")) |>
  group_by(range) |>
  summarise(p = mean(goldStandard),
            fraction = n() / nrow(jebVsGs))
# range       p fraction
# Maybe 0.397     0.306 
# No    0.00365   0.632 
# Yes   1         0.0615
