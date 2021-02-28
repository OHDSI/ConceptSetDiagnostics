# Concept search using string

#' @export
getStringSearchConcepts <-
  function(connection = NULL,
           searchString,
           fullTextSearch = FALSE,
           vocabularyDatabaseSchema = 'vocabulary') {
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection
    # also making search string of lower case - to make search uniform.
    searchString <-
      tolower(gsub("[^a-zA-Z0-9 ,]", " ", searchString))
    searchStringReverse <- stringi::stri_reverse(str = searchString)
    
    if (length(unlist(strsplit(searchString, " "))) > 1) {
      searchStringTsv <-
        paste(paste0(unlist(x = strsplit(
          x = searchString, split = " "
        )), ':*'), collapse = " & ")
    } else {
      searchStringTsv <- paste0(searchString, ':*')
    }
    
    if (length(unlist(strsplit(searchStringReverse, " "))) > 1) {
      searchStringReverseTsv <-
        paste(paste0(unlist(x = strsplit(
          x = searchStringReverse, split = " "
        )), ':*'), collapse = " & ")
    } else {
      searchStringReverseTsv <- paste0(searchStringReverse, ':*')
    }
    
    pathToSql <- system.file("sql/sql_server", 
                             "SearchVocabularyForConcepts.sql", 
                             package = 'ConceptSetDiagnostics')
    sql <- SqlRender::readSql(sourceFile = pathToSql)
    
    if (!fullTextSearch) {
      searchString <- ""
    }
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        search_string_tsv = searchStringTsv, 
        search_string_reverse_tsv = searchStringReverseTsv, 
        search_string = searchString, 
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble()
    
    conceptSetExpressionFromConceptTable <-
      getConceptSetExpressionFromConceptTable(conceptTable = data,
                                              selectAllDescendants = TRUE)
    conceptSetExpressionTable <- 
      getConceptSetDataFrameFromExpression(conceptSetExpression = conceptSetExpressionFromConceptTable) %>% 
      dplyr::left_join(y = data, by = 'conceptId')
    return(conceptSetExpressionTable)
  }