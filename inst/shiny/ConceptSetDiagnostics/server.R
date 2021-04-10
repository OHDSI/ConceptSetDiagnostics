# library(ConceptSetDiagnostics)
library(purrr)

shiny::shinyServer(function(input, output, session) {
  numberOfKeywords <- reactiveVal(value = 1)
  col_names <-
    shiny::reactive(x = paste0("col", seq_len(numberOfKeywords())))
  
  observeEvent(eventExpr = input$addKeyword, 
               handlerExpr = {
                 numberOfKeywords(numberOfKeywords() + 1)
  })
  
  observeEvent(eventExpr = input$removeKeyword,
               handlerExpr = {
                 if (numberOfKeywords() > 1) {
                   numberOfKeywords(numberOfKeywords() - 1)
                 }
               })
  
  output$col <- renderUI({
    purrr::map(.x = col_names(), 
               .f = ~ shiny::textInput(inputId = .x, 
                                       label = NULL, 
                                       value = isolate(expr = input[[.x]])))
  })
  
  conceptSetResultsExpression <- reactiveVal(value = NULL)
  conceptSetSearchResults <- reactiveVal(value = NULL)
  observeEvent(eventExpr = input$search, 
               handlerExpr = {
    shiny::withProgress(expr = {
      keywords <- purrr::map_chr(.x = col_names(), 
                                 .f = ~ input[[.x]] %||% "")
      if (length(keywords) > 0) {
        conceptSetExpressionAllTerms <- list()
        searchResultConceptIdsAllTerms <- list()
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
          
          # step perform string search
          searchResultConceptIds <- ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                            searchString =  keywords[[i]]) 
          if (length(vocabularyIdOfInterest) > 0) {
            searchResultConceptIds <- searchResultConceptIds %>% 
              dplyr::filter(.data$vocabularyId %in% vocabularyIdOfInterest)
          }
          if (length(domainIdOfInterest) > 0) {
            searchResultConceptIds <- searchResultConceptIds %>% 
              dplyr::filter(.data$domainId %in% domainIdOfInterest)
          }
          
          # develop a concept set expression based on string search
          conceptSetExpressionDataFrame <- 
            ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
              conceptSetExpressionDataFrame = searchResultConceptIds,
              selectAllDescendants = TRUE) %>% 
            ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>% 
            ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(updateVocabularyFields = TRUE, 
                                                                                            recordCount = TRUE, 
                                                                     connection = connection) 
          
          conceptSetExpressionAllTerms[[i]] <- conceptSetExpressionDataFrame
          searchResultConceptIdsAllTerms[[i]] <- searchResultConceptIds
        }
        
        searchResultConceptIdsAllTerms <- dplyr::bind_rows(searchResultConceptIdsAllTerms) %>% 
          dplyr::distinct()
        conceptSetSearchResults(searchResultConceptIdsAllTerms)
        
          conceptSetExpressionAllTerms <-
            dplyr::bind_rows(conceptSetExpressionAllTerms) %>%
            ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame() %>%
            ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>%
            ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(connection = connection,
                                                                                            recordCount = TRUE, 
                                                                                            updateVocabularyFields = TRUE)
          conceptSetResultsExpression(conceptSetExpressionAllTerms)
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
      data <- conceptSetResultsExpression()
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
      conceptSetExpression <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
        conceptSetExpressionDataFrame = conceptSetResultsExpression())
      data <-
        ConceptSetDiagnostics::resolveConceptSetExpression(conceptSetExpression = conceptSetExpression,
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
      conceptSetExpression <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
        conceptSetExpressionDataFrame = conceptSetResultsExpression())
      data <-
        ConceptSetDiagnostics::getRecommendationForConceptSetExpression(conceptSetExpression = conceptSetExpression, 
                                                                        connection = connection)
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
      data <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
        conceptSetExpressionDataFrame = conceptSetResultsExpression()) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }
  })
})