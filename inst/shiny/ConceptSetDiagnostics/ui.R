shinydashboard::dashboardPage(
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  header = shinydashboard::dashboardHeader(title = "Concept Set Diagnostric"),
  body = shinydashboard::dashboardBody(
    shinydashboard::box(
      title = "Vocabulary search",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::h5("Enter Keyword(s) :"),
      column(4, shiny::uiOutput(outputId = "col")),
      shinyjs::useShinyjs(),
      # tags$style("#col input {background-color:red !important}"),
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
      column(
        3,
        shiny::selectInput(
          inputId = "vocabularyIdToFilter",
          label = "Filter by Vocabulary",
          choices =  vocabulary$vocabularyId,
          selected = vocabulary$vocabularyId[stringr::str_detect(string = tolower(vocabulary$vocabularyId),
                                                                 pattern = 'icd|snomed|read')] %>% sort(),
          multiple = TRUE
        )
      ),
      column(
        3,
        shiny::selectInput(
          inputId = "domainIdToFilter",
          label = "Filter by Domain",
          choices =  domain$domainId,
          selected = domain$domainId[stringr::str_detect(string = tolower(domain$domainId),
                                                         pattern = 'condition|observation')] %>% sort(),
          multiple = TRUE
        )
      ),
      column(12,
             shiny::actionButton(inputId = "search", label = "Search"))
    ),
    shiny::conditionalPanel(
      condition = "!(output.isSearchResultFound)",
      shinydashboard::box(
        title = "Result",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::tabsetPanel(
          id = "cohortDetails",
          type = "tab",
          shiny::tabPanel(
            title = "Concept Set Expression",
            value = "conceptSetExpression",
            shiny::conditionalPanel(
              condition = "output.numberOfRowSelectedInConceptSetExpression >= 1",
              shiny::actionButton(
                inputId = "deleteConceptSetExpression",
                label = "Delete",
                style = "background-color:#c45;color:#fff"
              )
            ),
            DT::DTOutput(outputId = "conceptSetExpression"),
            tags$script(
              HTML(
                '
            $(document).on("click", ".selectConceptSetExpressionRow", function () {
                       var conceptSetExpressionRowCheckboxes = document.getElementsByName("selectConceptSetExpressionRow");
                       var conceptSetExpressionRowCheckboxesChecked = [];
                       for (var i=0; i<conceptSetExpressionRowCheckboxes.length; i++) {
                       if (conceptSetExpressionRowCheckboxes[i].checked) {
                       conceptSetExpressionRowCheckboxesChecked.push(conceptSetExpressionRowCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("conceptSetExpression_checkboxes_checked",conceptSetExpressionRowCheckboxesChecked);});

            $(document).on("click", ".selectDescendants", function () {
                       var descendantsCheckboxes = document.getElementsByName("selectDescendants");
                       var descendantsCheckboxesChecked = [];
                       for (var i=0; i<descendantsCheckboxes.length; i++) {
                       if (descendantsCheckboxes[i].checked) {
                       descendantsCheckboxesChecked.push(descendantsCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("descendants_checkboxes_checked",descendantsCheckboxesChecked);});

            $(document).on("click", ".selectMapped", function () {
                       var mappedCheckboxes = document.getElementsByName("selectMapped");
                       var mappedCheckboxesChecked = [];
                       for (var i=0; i<mappedCheckboxes.length; i++) {
                       if (mappedCheckboxes[i].checked) {
                       mappedCheckboxesChecked.push(mappedCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("mapped_checkboxes_checked",mappedCheckboxesChecked);});

            $(document).on("click", ".selectExcluded", function () {
                       var excludedCheckboxes = document.getElementsByName("selectExcluded");
                       var excludedCheckboxesChecked = [];
                       for (var i=0; i<excludedCheckboxes.length; i++) {
                       if (excludedCheckboxes[i].checked) {
                       excludedCheckboxesChecked.push(excludedCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("excluded_checkboxes_checked",excludedCheckboxesChecked);});
            '
              )
            ),
            tags$style(
              HTML(
                '.selectDescendants,.selectMapped,.selectExcluded,.selectConceptSetExpressionRow,
                .selectResolvedRow,.selectMappedRow,.selectRelationshipRow,.selectAncestorRelationshipRow{
              height : 30px;
              width : 30px;
                }
                '
                
              )
            ),
            tags$style(
              "
            #resolvedParameters .checkbox,#mappedParameters .checkbox,
            #relationshipParameters .checkbox, #ancestorRelationshipParameters .checkbox{ /* checkbox is a div class*/
              line-height: 30px;
              margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
            }
           #resolvedParameters input[type='checkbox'], #mappedParameters input[type='checkbox'],
           #relationshipParameters input[type='checkbox'], #ancestorRelationshipParameters input[type='checkbox']{ /* style for checkboxes */
              width: 30px; /*Desired width*/
              height: 30px; /*Desired height*/
              line-height: 30px;
            }
            #resolvedParameters span, #mappedParameters span,
            #relationshipParameters span, #ancestorRelationshipParameters span{
                margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
                line-height: 30px;
            }
          "
            ),
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
                tags$script(
                  HTML(
                    '
                    $(document).on("click", ".selectResolvedRow", function () {
                       var resolvedRowCheckboxes = document.getElementsByName("selectResolvedRow");
                       var resolvedCheckboxesChecked = [];
                       for (var i=0; i<resolvedRowCheckboxes.length; i++) {
                       if (resolvedRowCheckboxes[i].checked) {
                       resolvedCheckboxesChecked.push(resolvedRowCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("resolved_checkboxes_checked",resolvedCheckboxesChecked);});
                     '
                  )
                ),
                shiny::conditionalPanel(condition = "output.numberOfRowSelectedInResolved >= 1", {
                  column(
                    12,
                    style = "border:1px solid black;border-radius:5px;",
                    column(
                      3,
                      shiny::checkboxGroupInput(
                        inputId = "resolvedParameters",
                        label = "",
                        choices = c("descendent", "mapped", "exclude"),
                        inline = TRUE
                      )
                    ),
                    column(
                      9,
                      style = "margin-top:20px;",
                      shiny::actionButton(
                        inputId = "addResolved",
                        "Add to Concept Set Expression",
                        style = "background-color:#00A86B;color:white"
                      )
                    )
                  )
                }),
                
              ),
              shiny::tabPanel(
                value = "mappedConceptsetExpressionTabPanel",
                title = "Mapped",
                DT::DTOutput(outputId = "mappedConceptSetExpression"),
                tags$script(
                  HTML(
                    '
                    $(document).on("click", ".selectMappedRow", function () {
                       var mappedRowCheckboxes = document.getElementsByName("selectMappedRow");
                       var mappedCheckboxesChecked = [];
                       for (var i=0; i<mappedRowCheckboxes.length; i++) {
                       if (mappedRowCheckboxes[i].checked) {
                       mappedCheckboxesChecked.push(mappedRowCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("mappedRow_checkboxes_checked",mappedCheckboxesChecked);});
                     '
                  )
                ),
                shiny::conditionalPanel(condition = "output.numberOfRowSelectedInMapped >= 1", {
                  column(
                    12,
                    style = "border:1px solid black;border-radius:5px;",
                    column(
                      3,
                      shiny::checkboxGroupInput(
                        inputId = "mappedParameters",
                        label = "",
                        choices = c("Exclude", "Descendants", "Mapped"),
                        inline = TRUE
                      )
                    ),
                    column(
                      9,
                      style = "margin-top:20px;",
                      shiny::actionButton(
                        inputId = "addMapped",
                        "Add to Concept Set Expression",
                        style = "background-color:#00A86B;color:white"
                      )
                    )
                  )
                }),
                
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
          ),
          shiny::tabPanel(
            title = "Search Result",
            value = "searchResult",
            shiny::conditionalPanel(
              condition = "output.numberOfRowSelectedInSearchResult >= 1",
              shiny::actionButton(
                inputId = "deleteSearchResult",
                label = "Delete",
                style = "background-color:#c45;color:#fff"
              )
            ),
            DT::DTOutput(outputId = "searchResultConceptIds")
          )
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "output.isConceptIdSelected",
      shinydashboard::box(
        title = "Concept ID Details :",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::uiOutput(outputId = "conceptIdDetails"),
        column(
          4,
          shinydashboard::box(
            title = "Concept Synonym",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::DTOutput(outputId = "conceptSynonym")
          )
        ),
        column(
          8,
          shinydashboard::box(
            title = "Related Concepts",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::DTOutput(outputId = "conceptRelationship"),
            tags$script(
              HTML(
                '
                    $(document).on("click", ".selectRelationshipRow", function () {
                       var relationshipRowCheckboxes = document.getElementsByName("selectRelationshipRow");
                       var relationshipCheckboxesChecked = [];
                       for (var i=0; i<relationshipRowCheckboxes.length; i++) {
                       if (relationshipRowCheckboxes[i].checked) {
                       relationshipCheckboxesChecked.push(relationshipRowCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("relationship_checkboxes_checked",relationshipCheckboxesChecked);});
                     '
              )
            ),
            shiny::conditionalPanel(condition = "output.numberOfRowSelectedInRelationship >= 1", {
              column(
                12,
                style = "border:1px solid black;border-radius:5px;",
                column(
                  5,
                  shiny::checkboxGroupInput(
                    inputId = "relationshipParameters",
                    label = "",
                    choices = c("descendent", "mapped", "exclude"),
                    inline = TRUE
                  )
                ),
                column(
                  7,
                  style = "margin-top:20px;",
                  shiny::actionButton(
                    inputId = "addRelationship",
                    "Add to Concept Set Expression",
                    style = "background-color:#00A86B;color:white"
                  )
                )
              )
            }),
          )
        )
      )
    )
  )
)
