library(magrittr)
library(ConceptSetDiagnostics)

shiny::shinyServer(function(input, output, session) {
  conceptSetResults <- reactiveVal(NULL)
  observeEvent(eventExpr = input$search, handlerExpr = {
    shiny::withProgress(expr = {
      data <- ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = input$keyword,
                                                                          connection = connection)
    },message = "Loading, Please Wait . .")
    conceptSetResults(data)
  })
  
  output$searchResultConceptIds <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$searchResultConceptIds
      # standardDataTable(data = conceptSetResults()$searchResultConceptIds)
    }
  })
  
  output$conceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$conceptSetExpressionDataFrame
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$resolvedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$resolvedConceptIds$resolvedConcepts
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$mappedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$resolvedConceptIds$mappedConcepts
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$recommendedStandardConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$recommendedConceptIds$recommendedStandard
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$recommendedConceptIds$recommendedSource
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      conceptSetResults()$recommendedConceptIds$recommendedSource
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$conceptSetExpressionJSON <- shiny::renderText({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      data <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetResults()$conceptSetExpressionDataFrame)
      result <- jsonlite::toJSON(data,pretty = TRUE)
      # standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  }) 
  
})