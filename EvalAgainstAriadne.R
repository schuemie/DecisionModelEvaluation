library(httr2)
library(jsonlite)
library(dplyr)

termToCandidates <- readr::read_csv("exact_matching_vector_search_context.csv", show_col_types = FALSE) |>
  SqlRender::snakeCaseToCamelCaseNames()
instructions <- paste(readLines("InstructionsAriadneShort.txt"), collapse = "\n")
useLocal <- FALSE

if (useLocal) {
  cacheFolder <- "cacheAriadneLocal"
} else {
  cacheFolder <- "cacheAriadne"
}
dir.create(cacheFolder)

# group = groups[[20]]
group <- termToCandidates |>
  filter(sourceCode == 45422256)
findMatch <- function(group) {
  cacheFileName <- file.path(cacheFolder, sprintf("%s.rds", group$sourceCode[1]))
  if (file.exists(cacheFileName)) {
    row <- readRDS(cacheFileName)
    return(row)
  } else {

    message(sprintf("Processing '%s'", group$sourceTerm[1]))
    contextState <- sprintf("Source term: %s", group$cleanedTerm[1])

    optionsList <- list()
    answersList <- list()
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
      answersList[[key]] <- tibble(mappedConceptId = group$matchedConceptId[i],
                                   mappedConceptName = group$matchedConceptName[i])
    }
    optionsList[["none"]] <- "None of the provided candidates are an exact match."
    answersList[["none"]] <- tibble(mappedConceptId = -1,
                                    mappedConceptName = "None")

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

    answer <- parsed$answers$best_match
    row <- answersList[[answer$choice]] |>
      mutate(confidence = answer$confidence,
             warnings = warnings,
             sourceCode = group$sourceCode[1],
             sourceTerm = group$sourceTerm[1])
    saveRDS(row, cacheFileName)
    return(row)
  }
}

groups <- termToCandidates |>
  group_by(sourceTerm) |>
  group_split()
rows <- lapply(groups, findMatch)
# rows <- lapply(list.files(cacheFolder, full.names = TRUE), readRDS)
rows <- bind_rows(rows)

gs <- termToCandidates |>
  distinct(sourceCode,
           targetConceptId,
           targetConceptName,
           predicate,
           targetConceptIdB,
           predicateB) |>
  mutate(targetConceptId = if_else(predicate == "broadMatch", -1, targetConceptId),
         targetConceptIdB = if_else(predicateB == "broadMatch", -1, targetConceptIdB))
jebVsGs <- rows |>
  inner_join(gs, by = join_by(sourceCode)) |>
  mutate(correct = (targetConceptId == mappedConceptId) | (!is.na(targetConceptIdB) & targetConceptIdB == mappedConceptId))
jebVsGs |>
  summarise(accuracy = mean(correct))
# Jev accuracy (vs gold standard)
# Local: 0.804
# TypeSafe: 0.791
jebVsGs |>
  summarise(fractionConfident = mean(confidence > 0.95))
# Fraction p > 0.95
# Local: 0.0978
# TypeSafe: 0.257
jebVsGs |>
  filter(confidence > 0.95) |>
  summarise(accuracy = mean(correct))
# Accuracy in subset having p > 0.95
# TypeSafe: 0.958

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
    mutate(bin = cut(confidence, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarize(
      mean_predicted = mean(confidence, na.rm = TRUE),
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

plotCalibration(jebVsGs)
ggsave("CalibrationJebVsGsTypeSafe.png", width = 5, height = 5)
plotCalibration(filter(jebVsGs, warnings == ""))

plotCalibration(jebVsFullLlm)
plotCalibration(filter(jebVsFullLlm, warnings == ""))

hist(jebVsGs$confidence)
# row <- findMatch(groups[[200]])
