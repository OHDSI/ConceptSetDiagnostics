library(magrittr)
library(ConceptSetDiagnostics)

shiny::shinyServer(function(input, output, session) {
  conceptSetResults <- reactiveVal(NULL)
  observeEvent(eventExpr = input$search, handlerExpr = {
    shiny::withProgress(expr = {
      data <-
        ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = input$keyword,
                                                                    connection = connection)
    }, message = "Loading, Please Wait . .")
    conceptSetResults(data)
  })
  
  output$searchResultConceptIds <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$searchResultConceptIds)
    }
  })
  
  output$conceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$conceptSetExpressionDataFrame)
    }
  })
  
  output$resolvedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$resolvedConceptIds$resolvedConcepts)
    }
  })
  
  output$mappedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$resolvedConceptIds$mappedConcepts)
    }
  })
  
  output$recommendedStandardConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$recommendedConceptIds$recommendedStandard)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$recommendedConceptIds$recommendedSource)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetResults()$recommendedConceptIds$recommendedSource)
    }
  })
  
  output$conceptSetExpressionJSON <- shiny::renderText({
    if (is.null(conceptSetResults())) {
      return(NULL)
    } else {
      data <-
        ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetResults()$conceptSetExpressionDataFrame)
      jsonlite::toJSON(data, pretty = TRUE)
    }
  })
  
})