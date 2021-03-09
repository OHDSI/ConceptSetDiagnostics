# given a concept set table, perform design diagnostics
#' @export
performDesignDiagnosticsOnConceptTable <-
  function(connection,
           vocabularyDatabaseSchema = 'vocabulary',
           conceptSetExpressionTable,
           vocabularyIdForRecommender = c('SNOMED', 'ICD'),
           exportResults = TRUE,
           locationForResults,
           dbms = 'postgresql',
           blackList = c(0),
           iteration = 1) {
    result <- list()
    result$conceptSetExpressionTable <- conceptSetExpressionTable
    result$conceptSetExpression <-
      getConceptSetExpressionFromConceptTable(conceptTable = conceptSetExpressionTable)
    
    ### optimized
    result$conceptSetExpressionTableOptimized <-
      optimizeConceptSetExpression(
        connection = connection,
        dbms = dbms,
        conceptSetExpression = result$conceptSetExpression,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      ) %>%
      dplyr::filter(.data$removed == 0) %>%
      dplyr::select(.data$conceptId) %>%
      dplyr::distinct() %>%
      dplyr::left_join(y = result$conceptSetExpressionTable,
                       by = 'conceptId')
    
    result$conceptSetExpressionOptimized <-
      getConceptSetExpressionFromConceptTable(conceptTable = result$conceptSetExpressionTableOptimized)
    
    #################################
    recommendation <-
      ConceptSetDiagnostics::getRecommendationForConceptTable(
        connection = connection,
        dbms = dbms,
        conceptSetExpressionTable = result$conceptSetExpressionTable,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        vocabularyIdForRecommender = vocabularyIdForRecommender
      )
    result$recommendedStandard <-
      recommendation$recommendedStandard %>%
      dplyr::filter(!.data$conceptId %in% result$conceptSetExpressionTable$conceptId) %>% 
      dplyr::filter(!.data$conceptId %in% blackList)
    result$recommendedSource <- recommendation$recommendedSource %>%
      dplyr::filter(!.data$conceptId %in% result$conceptSetExpressionTable$conceptId) %>% 
      dplyr::filter(!.data$conceptId %in% blackList)
    
    
    if (exportResults) {
      if (nrow(result$recommendedStandard) > 0) {
        readr::write_excel_csv(
          x = result$recommendedStandard,
          file = file.path(
            locationForResults,
            paste0("recommendedStandard", iteration, ".csv")
          ),
          append = FALSE,
          na = ""
        )
        writeLines(text = paste0(
          "Wrote recommendedStandard",
          iteration,
          ".csv to ",
          locationForResults
        ))
      } else {
        writeLines(
          text = paste0(
            "No recommendation. recommendedStandard",
            iteration,
            ".csv is not written to ",
            locationForResults
          )
        )
        unlink(x = file.path(
          locationForResults,
          paste0("recommendedStandard", iteration, ".csv")
        ), recursive = TRUE, force = TRUE)
      }
      if (nrow(result$recommendedSource) > 0) {
        readr::write_excel_csv(
          x = result$recommendedSource,
          file = file.path(
            locationForResults,
            paste0("recommendedSource", iteration, ".csv")
          ),
          append = FALSE,
          na = ""
        )
        writeLines(text = paste0(
          "Wrote recommendedSource",
          iteration,
          ".csv to ",
          locationForResults
        ))
      } else {
        writeLines(
          text = paste0(
            "No recommendation. recommendedSource",
            iteration,
            ".csv is not written to ",
            locationForResults
          )
        )
        unlink(x = file.path(
          locationForResults,
          paste0("recommendedSource", iteration, ".csv")
        ), recursive = TRUE, force = TRUE)
      }
    }
    return(result)
  }