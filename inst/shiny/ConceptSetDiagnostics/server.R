# library(ConceptSetDiagnostics)
# library(purrr)

shiny::shinyServer(function(input, output, session) {
  numberOfKeywords <- reactiveVal(1)
  col_names <-
    shiny::reactive(paste0("col", seq_len(numberOfKeywords())))
  
  
  observeEvent(eventExpr = input$addKeyword, handlerExpr = {
    numberOfKeywords(numberOfKeywords() + 1)
  })
  
  
  observeEvent(eventExpr = input$removeKeyword,
               handlerExpr = {
                 if (numberOfKeywords() > 1) {
                   numberOfKeywords(numberOfKeywords() - 1)
                 }
               })
  
  output$col <- renderUI({
    purrr::map(col_names(), ~ shiny::textInput(.x, NULL, value = isolate(input[[.x]])))
  })
  
  conceptSetResultsExpression <- reactiveVal(NULL)
  conceptSetSearchResults <- reactiveVal(NULL)
  observeEvent(eventExpr = input$search, handlerExpr = {
    shiny::withProgress(expr = {
      keywords <- purrr::map_chr(col_names(), ~ input[[.x]] %||% "")
      if (length(keywords) > 0) {
        searchResult <- list()
        for (i in 1:length(keywords)) {
          
          vocabularyIdOfInterest <-
            c('SNOMED',
              'HCPCS',
              'ICD10CM',
              'ICD10',
              'ICD9CM',
              'ICD9',
              'Read')
          domainIdOfInterest <- c('Condition', 'Observation')
          
          
          
          designDiagnostics <-
            ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(
              searchString = keywords[[i]],
              exportResults = FALSE,
              vocabularyIdOfInterest = vocabularyIdOfInterest,
              domainIdOfInterest = domainIdOfInterest,
              connection = connection
            )
          searchResult[[i]] <- designDiagnostics
          json <-
            ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = designDiagnostics$conceptSetExpressionDataFrame) %>%
            RJSONIO::toJSON(digits = 23, pretty = TRUE)
        }
        
        if (length(searchResult) >=  1) {
          conceptSetExpressionAllTerms <- list()
          searchResultConceptIdsAllTerms <- list()
          for (i in (1:length(searchResult))) {
            conceptSetExpressionAllTerms[[i]] <-
              searchResult[[i]]$conceptSetExpressionDataFrame
            searchResultConceptIdsAllTerms[[i]] <-
              searchResult[[i]]$searchResultConceptIds
          }
          conceptSetExpressionAllTerms <-
            dplyr::bind_rows(conceptSetExpressionAllTerms) %>%
            ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame() %>%
            ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>%
            ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(connection = connection,
                                                                                            updateVocabularyFields = TRUE) %>%
            ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame()
          
          conceptSetResultsExpression(conceptSetExpressionAllTerms)
          conceptSetSearchResults(dplyr::bind_rows(searchResultConceptIdsAllTerms))
          
          json <- conceptSetExpressionAllTerms %>%
            RJSONIO::toJSON(digits = 23, pretty = TRUE)
          
        }
      }
    }, message = "Loading, Please Wait . .")
  })
  
  output$searchResultConceptIds <- DT::renderDT({
    if (is.null(conceptSetSearchResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetSearchResults())
    }
  })
  
  getConceptSetExpression <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      data <-
        ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetResultsExpression())
    })
    return(data)
  })
  
  output$conceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getConceptSetExpression())
    }
  })
  
  getResolved <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      data <-
        ConceptSetDiagnostics::resolveConceptSetExpression(conceptSetExpression = conceptSetResultsExpression(),
                                                           connection = connection)
    })
    return(data)
  })
  
  output$resolvedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getResolved()$resolvedConcepts)
    }
  })
  
  output$mappedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getResolved()$mappedConcepts)
    }
  })
  
  getRecommendation <- shiny::reactive({
    shiny::withProgress(message = "Loading. . .", {
      data <-
        ConceptSetDiagnostics::getRecommendationForConceptSetExpression(conceptSetResultsExpression(), connection = connection)
    })
    return(data)
  })
  
  output$recommendedStandardConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedStandard)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedSource)
    }
  })
  
  output$conceptSetExpressionJSON <- shiny::renderText({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      data <-
        conceptSetResultsExpression() %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }
  })
})