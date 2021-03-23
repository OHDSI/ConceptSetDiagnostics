keyWords <- 'Thyroiditis'


outputLocation <- stringr::str_replace_all(string = keyWords[[1]], 
                                           pattern = " ",
                                           replacement = "")
vocabularyIdOfInterest <- c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read')
domainIdOfInterest <- c('Condition', 'Observation')
# Details for connecting to the server:
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(
      Sys.getenv("phenotypeLibraryDbServer"),
      Sys.getenv("phenotypeLibraryDbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUserGowtham"),
    password = Sys.getenv("shinyDbPasswordGowtham"),
    port = Sys.getenv("phenotypeLibraryDbPort")
  )

connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

# Load the package
library(ConceptSetDiagnostics)

## create output location
locationForResults <-
  file.path(rstudioapi::getActiveProject(), 'extras', 'example', outputLocation)

# get search results
searchResult <- list()

### iteration 1
for (i in (1:length(keyWords))) {
  locationForResults2 <- file.path(locationForResults,
                                   paste0('keyWord', keyWords[[i]]),
                                   'iteration1')
  designDiagnostics <- ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = keyWords[[i]], 
                                                                                   exportResults = FALSE,
                                                                                   locationForResults = locationForResults2,
                                                                                   vocabularyIdOfInterest = vocabularyIdOfInterest,
                                                                                   domainIdOfInterest = domainIdOfInterest, 
                                                                                   connection = connection)
  searchResult[[i]] <- designDiagnostics
}

saveRDS(object = searchResult, file = file.path(locationForResults, "searchResult.rds"))
if (length(searchResult) >  1) {
  conceptSetExpressionAllTerms <- list()
  for (i in (1:length(searchResult))) {
    conceptSetExpressionAllTerms[[i]] <- searchResult[[i]]$conceptSetExpressionDataFrame
  }
  conceptSetExpressionAllTerms <- dplyr::bind_rows(conceptSetExpressionAllTerms) %>% 
    ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame() %>% 
    ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>% 
    ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(connection = connection, 
                                                                                    updateVocabularyFields = TRUE) %>% 
    ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame()
  
  json <- conceptSetExpressionAllTerms %>% 
    RJSONIO::toJSON(digits = 23, pretty = TRUE)
  
  SqlRender::writeSql(sql = json,
                      targetFile = file.path(locationForResults, "conceptSetExpressionAllTerms.json"))
}


for (j in (1:length(searchResult))) {
  json <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(
    conceptSetExpressionDataFrame = designDiagnostics$conceptSetExpressionDataFrame) %>% 
    RJSONIO::toJSON(digits = 23, pretty = TRUE) 
  SqlRender::writeSql(sql = json,
                      targetFile = file.path(locationForResults, paste0("conceptSetExpression", j, ".json")))
}

DatabaseConnector::disconnect(connection = connection)

