# given key words
keyWords <- c('Colitis')
outputLocation <- c('Colitis')
vocabularyIdOfInterest <- c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read')
domainIdOfInterest <- c('Condition', 'Procedure', 'Observation')

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
dir.create(path = locationForResults,
           recursive = TRUE,
           showWarnings = FALSE)


# get search results
searchResult <- list()
for (i in (1:length(keyWords))) {
  locationForResults2 <- file.path(locationForResults,
                                   paste0('keyWord', keyWords[[i]]),
                                   'iteration1')
  designDiagnostics <- ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = keyWords[[i]], 
                                                                                   exportResults = TRUE,
                                                                                   locationForResults = locationForResults2,
                                                                                   vocabularyIdOfInterest = vocabularyIdOfInterest,
                                                                                   domainIdOfInterest = domainIdOfInterest, 
                                                                                   connection = connection)
  searchResult[[i]] <- designDiagnostics
}

json <- ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = designDiagnostics$conceptSetExpressionDataFrame) %>% 
  RJSONIO::toJSON(digits = 23, pretty = TRUE)

SqlRender::writeSql(sql = json,
                    targetFile = file.path(locationForResults, "conceptSetExpression.json"))

DatabaseConnector::disconnect(connection = connection)
