# Concept search using string

#' @export
getStringSearchConcepts <-
  function(connection = NULL,
           searchString,
           fullTextSearch = FALSE,
           vocabularyDatabaseSchema = 'vocabulary') {
    # Note this function is designed for postgres with TSV enabled.
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection
    # also making search string of lower case - to make search uniform.
    searchString <-
      stringr::str_squish(tolower(gsub("[^a-zA-Z0-9 ,]", " ", searchString)))
    
    # reversing for reverse search in TSV
    searchStringReverse <- stringi::stri_reverse(str = searchString)
    ## if any string is shorter than 5 letters than it brings back
    ## non specific search result
    searchStringReverse <- stringr::str_split(string = searchStringReverse, pattern = " ") %>% unlist()
    for (i in (1:length(searchStringReverse))) {
      if (nchar(searchStringReverse[[i]]) < 5) {
        searchStringReverse[[i]] <- ''
      }
    }
    searchStringReverse <- stringr::str_squish(paste(searchStringReverse, collapse = " ")) 
    
    # function to create TSV string for post gres
    stringForTsvSearch <- function(string) {
      string <- stringr::str_squish(string)
      # split the string to vector
      stringSplit = strsplit(
        x = string, split = " "
      ) %>% unlist()
      # add wild card only if word is atleast three characters long
      for (i in (1:length(stringSplit))) {
        if (nchar(stringSplit[[i]]) > 2) {
          stringSplit[[i]] <- paste0(stringSplit[[i]], ':*')
        }
      }
      return(paste(stringSplit, collapse = " & "))
    }
    
    searchStringTsv <- stringForTsvSearch(searchString)
    searchStringReverseTsv <- stringForTsvSearch(searchStringReverse)
    
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

    return(data)
  }