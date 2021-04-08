library(magrittr)

shinydashboard::dashboardPage(
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  header = shinydashboard::dashboardHeader(title = "Concept Set Diagnostric"),
  body = shinydashboard::dashboardBody(
    shinydashboard::box(
      title = "Concept Set Diagnostic",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::textInput(inputId = "keyword", label = "Keyword"),
      shiny::actionButton(inputId = "search", label = "Search")
    ),
    shiny::tabsetPanel(
      id = "cohortDetails",
      type = "tab",
      shiny::tabPanel(title = "Search Result",
                      value = "searchResult",
                      DT::DTOutput(outputId = "searchResultConceptIds")
      ),
      shiny::tabPanel(title = "Concept Set Expression",
                      value = "conceptSetExpression",
                      DT::DTOutput(outputId = "conceptSetExpression")
      ),
      shiny::tabPanel(title = "Resolved",
                      value = "resolved",
                        shiny::tabsetPanel(
                          id = "resolvedConceptsetExpressionTab",
                          type = "tab",
                          shiny::tabPanel(
                            value = "resolvedConceptsetExpressionTabPanel",
                            title = "Resolved",
                            DT::DTOutput(outputId = "resolvedConceptSetExpression"),
                          ),
                          shiny::tabPanel(
                            value = "mappedConceptsetExpressionTabPanel",
                            title = "Mapped",
                            DT::DTOutput(outputId = "mappedConceptSetExpression")
                          )
                        )
                      
      ),
      shiny::tabPanel(title = "Recommended",
                      value = "recommended",
                      shiny::tabsetPanel(
                        id = "recommendedConceptsetExpressionTab",
                        type = "tab",
                        shiny::tabPanel(
                          value = "recommendedStandardConceptSetExpressionTabPanel",
                          title = "Standard",
                          DT::DTOutput(outputId = "recommendedStandardConceptSetExpression"),
                        ),
                        shiny::tabPanel(
                          value = "recommendedSourceConceptSetExpressionTabPanel",
                          title = "Source",
                          DT::DTOutput(outputId = "recommendedSourceConceptSetExpression")
                        )
                      )
                      
      ),
      shiny::tabPanel(title = "JSON",
                      value = "json",
                      copyToClipboardButton(toCopyId = "conceptSetExpressionJSON", 
                                            style = "margin-top: 5px; margin-bottom: 5px;"),
                      shiny::verbatimTextOutput(outputId = "conceptSetExpressionJSON"),
                      tags$head(
                        tags$style(
                          "#conceptSetExpressionJSON { max-height:700px};"
                        )
                      )
      )
      ),
  )
)
