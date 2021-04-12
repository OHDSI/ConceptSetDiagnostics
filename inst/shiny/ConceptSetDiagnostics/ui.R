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
      shiny::h5("Enter Keyword(s) :"),
      column(4, shiny::uiOutput("col")),
      column(
        2,
        shiny::actionButton(
          inputId = "addKeyword",
          icon = icon("plus"),
          label = ""
        ),
        shiny::actionButton(
          inputId = "removeKeyword",
          icon = icon("minus"),
          label = ""
        )
      ),
      column(3,
             shiny::selectInput(inputId = "vocabularyId",
                                label = "vocabulary ID of Interest",
                                choices =  c('SNOMED','HCPCS','ICD10CM','ICD10','ICD9CM','ICD9','Read'),
                                selected = c('SNOMED','HCPCS','ICD10CM','ICD10','ICD9CM','ICD9','Read'),multiple = TRUE)
             ),
      column(3,
             shiny::selectInput(inputId = "domainId",
                                label = "domain ID of Interest",
                                choices =  c('Condition', 'Observation'),
                                selected = c('Condition', 'Observation'),multiple = TRUE)
      ),
      column(12,
             shiny::actionButton(inputId = "search", label = "Search"))
    ),
    shiny::tabsetPanel(
      id = "cohortDetails",
      type = "tab",
      shiny::tabPanel(
        title = "Search Result",
        value = "searchResult",
        DT::DTOutput(outputId = "searchResultConceptIds")
      ),
      shiny::tabPanel(
        title = "Concept Set Expression",
        value = "conceptSetExpression",
        DT::DTOutput(outputId = "conceptSetExpression")
      ),
      shiny::tabPanel(
        title = "Resolved",
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
      shiny::tabPanel(
        title = "Recommended",
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
      shiny::tabPanel(
        title = "JSON",
        value = "json",
        copyToClipboardButton(toCopyId = "conceptSetExpressionJSON",
                              style = "margin-top: 5px; margin-bottom: 5px;"),
        shiny::verbatimTextOutput(outputId = "conceptSetExpressionJSON"),
        tags$head(tags$style(
          "#conceptSetExpressionJSON { max-height:700px};"
        ))
      )
    ),
  )
)
