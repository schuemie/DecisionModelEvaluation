library(httr2)
library(jsonlite)
library(dplyr)

termToCandidates <- readr::read_csv("exact_matching_vector_search_context.csv", show_col_types = FALSE) |>
  SqlRender::snakeCaseToCamelCaseNames()
# instructions <- paste(readLines("InstructionsAriadneShort.txt"), collapse = "\n")
instructions <- paste(readLines("InstructionsAriadne.txt"), collapse = "\n")

useLocal <- FALSE

if (useLocal) {
  cacheFolder <- "cacheAriadneLocal"
} else {
  cacheFolder <- "cacheAriadne"
}
dir.create(cacheFolder)

# group = groups[[20]]
# group <- termToCandidates |>
#   filter(sourceCode == 45422256)
findMatch <- function(group) {
  cacheFileName <- file.path(cacheFolder, sprintf("%s.rds", group$sourceCode[1]))
  if (file.exists(cacheFileName)) {
    rows <- readRDS(cacheFileName)
    return(rows)
  } else {
    
    message(sprintf("Processing '%s'", group$sourceTerm[1]))
    contextState <- sprintf("Source term: %s", group$cleanedTerm[1])
    
    optionsList <- list()
    rows <- list()
    for (i in seq_len(nrow(group))) {
      key <- paste0("candidate_", i)
      if (useLocal) {
        value <- sprintf("Name: %s\nParents: %s\nSynonyms: %s\nDomain: %s\nClass: %s\nVocabulary: %s",
                         group$matchedConceptName[i],
                         group$matchedParents[i],
                         group$matchedSynonyms[i],
                         group$matchedDomainId[i],
                         group$matchedConceptClassId[i],
                         group$matchedVocabularyId[i])
      } else {
        value <- sprintf("Name: %s\nParents: %s\nChildren: %s\nSynonyms: %s\nDomain: %s\nClass: %s\nVocabulary: %s",
                         group$matchedConceptName[i],
                         group$matchedParents[i],
                         group$matchedChildren[i],
                         group$matchedSynonyms[i],
                         group$matchedDomainId[i],
                         group$matchedConceptClassId[i],
                         group$matchedVocabularyId[i])
      }
      optionsList[[key]] <- value
      rows[[i]] <- tibble(key = key, 
                          mappedConceptId = group$matchedConceptId[i],
                          mappedConceptName = group$matchedConceptName[i])
    }
    optionsList[["none"]] <- "None of the provided candidates are an exact match."
    rows[[length(rows) + 1]] <- tibble(key = "none",
                                       mappedConceptId = -1,
                                       mappedConceptName = "None")
    rows <- bind_rows(rows)
    
    payload <- list(
      state = contextState,
      questions = list(
        best_match = list(
          type = "choice",
          instructions = instructions,
          criteria = optionsList
        )
      )
    )
    if (useLocal) {
      req <- request("http://localhost:8100/v1/systemone") |>
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
    if ("warnings" %in% names(parsed)) {
      warnings <- paste(unlist(parsed$warnings), collapse = ";")
    } else {
      warnings <- ""
    }
    
    rows <- rows |>
      inner_join(tibble(key = names(parsed$answers$best_match$probabilities),
                        probability = unlist(parsed$answers$best_match$probabilities)),
                 by = join_by(key)) |>
      mutate(choice = key ==  parsed$answers$best_match$choice,
             warnings = warnings,
             sourceCode = group$sourceCode[1],
             sourceTerm = group$sourceTerm[1],
             cleanedTerm = group$cleanedTerm[1]) 
    saveRDS(rows, cacheFileName)
    return(row)
  }
}

groups <- termToCandidates |>
  group_by(sourceTerm) |>
  group_split()
rows <- lapply(groups, findMatch)
# rows <- lapply(list.files(cacheFolder, full.names = TRUE), readRDS)
rows <- bind_rows(rows)

if ("probility" %in% names(rows)) {
  rows <- rows |> rename(probability = "probility")
}

gs <- termToCandidates |>
  distinct(sourceCode,
           targetConceptId,
           targetConceptName,
           predicate,
           targetConceptIdB,
           targetConceptNameB,
           predicateB) |>
  mutate(targetConceptId = if_else(predicate == "broadMatch", -1, targetConceptId),
         targetConceptIdB = if_else(predicateB == "broadMatch", -1, targetConceptIdB))

jevVsGs <- rows |>
  inner_join(gs |>
               select(sourceCode, targetConceptId, targetConceptIdB),
             by = join_by(sourceCode)) |>
  mutate(correct = (targetConceptId == mappedConceptId) | (!is.na(targetConceptIdB) & targetConceptIdB == mappedConceptId))

jevChoicesVsGs <- jevVsGs |>
  filter(choice) 

jevChoicesVsGs |>
  filter(choice) |>
  summarise(accuracy = mean(correct))
# Jev accuracy (vs gold standard)
# 0.817

hist(jevVsGs$probability)
jevVsGs |>
  mutate(range = case_when(probability < 0.1 ~ "No",
                           probability > 0.9 ~ "Yes",
                           TRUE ~ "Maybe")) |>
  group_by(range) |>
  summarise(p = mean(correct),
            fraction = n() / nrow(jevVsGs))
# range       p fraction
# Maybe 0.469     0.0542
# No    0.00232   0.932 
# Yes   0.947     0.0141
jevVsGs |>
  group_by(sourceCode) |>
  summarise(total = sum(probability)) |>
  summarise(min(total))

jevChoicesVsGs |>
  summarise(fractionConfident = mean(probility > 0.95))
# Fraction p > 0.95
# 0.274
jevChoicesVsGs |>
  filter(probility > 0.95) |>
  summarise(accuracy = mean(correct))
# Accuracy in subset having p > 0.95
# 0.952

fullLlm <- readr::read_csv("exact_matching_llm_mapped_terms.csv", show_col_types = FALSE) |>
  SqlRender::snakeCaseToCamelCaseNames()
fullLlm <- fullLlm |>
  inner_join(gs, by = join_by(sourceCode)) |>
  mutate(correct = (targetConceptId == mappedConceptId) | (!is.na(targetConceptIdB) & targetConceptIdB == mappedConceptId))
fullLlm |>
  summarise(accuracy = mean(correct))
# o3 accuracy (against gold standard)
# 0.851




fullLlm |>
  inner_join(rows |>
               filter(confidence > 0.95) |>
               select("sourceCode")) |>
  summarise(accuracy = mean(correct))
# o3 accuracy in subset having p > 0.95
# 0.932

jebVsFullLlm <- rows |>
  inner_join(fullLlm |>
               select(sourceCode, fullLlmId = mappedConceptId), by = join_by(sourceCode)) |>
  mutate(correct = mappedConceptId == fullLlmId)
jebVsFullLlm |>
  summarise(accuracy = mean(correct))
# 0.780

readr::write_csv(jebVsGs, "JebVsGsTypeSafe.csv")
# Plot calibration -------------------
library(ggplot2)

plotCalibration <- function(joined) {
  calibration_data <- joined %>%
    mutate(correct_num = as.numeric(correct)) %>%
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
      x = "Predicted Probability (Confidence)",
      y = "Observed Accuracy",
      size = "N (Bin Size)"
    ) +
    theme_minimal()
}

plotCalibration(jevVsGs)
ggsave("CalibrationAriadneItemLevel.png", width = 5, height = 4)

plotCalibration(jevChoicesVsGs)
ggsave("CalibrationAriadneChoiceLevel.png", width = 5, height = 4)


hist(jebVsGs$confidence)
# row <- findMatch(groups[[200]])
