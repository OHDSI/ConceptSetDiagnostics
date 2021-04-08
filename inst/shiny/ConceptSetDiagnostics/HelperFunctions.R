standardDataTable <- function(data, 
                              selectionMode = "single",
                              selected = NULL,
                              searching = TRUE,
                              pageLength = 10) {
  
  dataTableFilter =
    list(position = 'top',
         clear = TRUE,
         plain = FALSE)
  
  dataTableOption =
    list(
      pageLength = pageLength,
      lengthMenu = list(c(5, 10, 20, -1), c("5", "10", "20", "All")),
      lengthChange = TRUE,
      searching = searching,
      ordering = TRUE,
      scrollX = TRUE,
      ordering = TRUE,
      paging = TRUE,
      info = TRUE,
      searchHighlight = TRUE,
      # search = list(regex = TRUE, caseInsensitive = FALSE),
      stateSave = TRUE,
      dom = 'lBfrtip',
      # B for buttons
      buttons = list(
        'copy',
        list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        ),
        'print',
        'colvis'
      ),
      colReorder = TRUE,
      realtime = FALSE,
      # for col reorder
      # fixedColumns = list(leftColumns = 1),
      # fixedHeader = TRUE,
      # processing = TRUE,
      autoWidth = TRUE
    )
  listOfVariablesThatAreAlwaysFactors <- c('domainId',
                                           'conceptClassId',
                                           'vocabularyId',
                                           'standardConcept',
                                           'conceptSetName',
                                           'conceptName',
                                           'cohortId',
                                           'cohortName',
                                           'phenotypeId',
                                           'phenotypeName',
                                           'analysisName',
                                           'startDay',
                                           'endDay',
                                           'analysisId',
                                           'temporalChoices',
                                           'covariateName',
                                           'conceptId',
                                           'databaseId',
                                           'standard', 
                                           'invalidReason',
                                           'invalid',
                                           'conceptCode', 
                                           'isExcluded',
                                           'excluded',
                                           'includeDescendants',
                                           'descendants',
                                           'includeMapped',
                                           'mapped',
                                           'conceptInSet'
  )
  
  convertVariableToFactor <- function(data, variables) {
    for (i in (1:length(variables))) {
      variable <- variables[i]
      if (variable %in% colnames(data)) {    
        data[[variable]] <- as.factor(data[[variable]])
      }
    }
    return(data %>% dplyr::tibble())
  }
  data <- convertVariableToFactor(data = data,
                                  variables = listOfVariablesThatAreAlwaysFactors)
  colNamesData <- colnames(data)
  if (exists("database")) {
    for (i in (1:length(colNamesData))) {
      if (!colNamesData[[i]] %in% database$databaseId) {
        if (exists("temporalTimeRef") &&
            colNamesData[[i]] %in% temporalTimeRef$temporalChoices) {
          # do nothing
        } else {
          colNamesData[[i]] <- camelCaseToTitleCase(colNamesData[[i]])
        }
      }
    }
  }
  
  dataTable <- DT::datatable(
    data = data,
    class = "stripe compact order-column hover",
    rownames = FALSE,
    options = dataTableOption,
    colnames = colNamesData,
    filter = dataTableFilter,
    # style = 'bootstrap4',
    escape = FALSE,
    selection = list(mode = selectionMode, target = "row", selected = selected),
    editable = FALSE,
    # container = sketch,
    extensions = c('Buttons', 'ColReorder', 'FixedColumns', 'FixedHeader'),
    plugins = c('natural') #'ellipsis'
    # escape = FALSE
  )
  
  colNames <- colnames(data)
  listRounds <-
    c(colNames[stringr::str_detect(string = tolower(colNames),
                                   pattern = 'entries|subjects|count|min|max|p10|p25|median|p75|p90|max|before|onvisitstart|after|duringvisit|dbc|drc|rc')]
      , colNames[stringr::str_detect(string = colNames,
                                     pattern = paste0(database$databaseId, collapse = "|"))])
  listDecimal <-
    colNames[stringr::str_detect(string = tolower(colNames),
                                 pattern = 'average|standarddeviation|mean|sd|personyears|incidencerate')]
  listPercent <-
    colNames[stringr::str_detect(string = tolower(colNames),
                                 pattern = 'percent|start')]
  if (length(listRounds) > 0) {
    dataTable <- DT::formatRound(table = dataTable,
                                 columns = listRounds,
                                 digits = 0)
  }
  if (length(listDecimal) > 0) {
    dataTable <- DT::formatRound(table = dataTable,
                                 columns = listDecimal,
                                 digits = 2)
  }
  if (length(listPercent) > 0) {
    dataTable <- DT::formatPercentage(table = dataTable,
                                      columns = listPercent,
                                      digits = 1)
  }
  return(dataTable)
}

copyToClipboardButton <- function(toCopyId, label = "Copy to clipboard", icon = shiny::icon("clipboard"), ...) {
  
  script <- sprintf("
  text = document.getElementById('%s').textContent;
  html = document.getElementById('%s').innerHTML;
  function listener(e) {
    e.clipboardData.setData('text/html', html);
    e.clipboardData.setData('text/plain', text);
    e.preventDefault();
  }
  document.addEventListener('copy', listener);
  document.execCommand('copy');
  document.removeEventListener('copy', listener);
  return false;",toCopyId, toCopyId)
  
  tags$button(type = "button", class = "btn btn-default action-button", onclick = script, icon, label, ...)
}
