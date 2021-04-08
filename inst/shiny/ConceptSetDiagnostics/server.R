library(magrittr)
library(ConceptSetDiagnostics)

shiny::shinyServer(function(input, output, session) {
  observeEvent(eventExpr = input$search, handlerExpr = {
    ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = input$keyword,
                                                                connection = connection)
  })
})