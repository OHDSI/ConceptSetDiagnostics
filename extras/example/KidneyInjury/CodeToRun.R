keyWords1 <- 'Kidney Injury'
keyWords2 <- 'Renal Failure'
keyWords3 <- 'Renal Disease'
keyWords4 <- 'Kidney Disease'


folder <- stringr::str_replace_all(string = keyWords1,
                                   pattern = " ",
                                   replacement = "")
# Load the package
library(ConceptSetDiagnostics)

locationForResults <-
  file.path(rstudioapi::getActiveProject(), 'example', folder)
dir.create(path = locationForResults,
           recursive = TRUE,
           showWarnings = FALSE)

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

# get search results
conceptSetExpressionTable1 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords1)
conceptSetExpressionTable2 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords2)
conceptSetExpressionTable3 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords3)
conceptSetExpressionTable4 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords4)
conceptSetExpressionTable <-
  dplyr::bind_rows(
    conceptSetExpressionTable1,
    conceptSetExpressionTable2,
    conceptSetExpressionTable3,
    conceptSetExpressionTable4
  )

##  do automated processing based on search results
designDiagnostic <-
  performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionTable = conceptSetExpressionTable,
    exportResults = TRUE,
    locationForResults = locationForResults,
    iteration = 1
  )


############### review only standard for round 1
discoveredThruRecommender <- c(4134922,
                               442102,
                               4321555,
                               194200,
                               194200,
                               444408,
                               196852,
                               196852,
                               199195,
                               199195,
                               442533,
                               200253,
                               4153992,
                               4126426,
                               36686421,
                               36716454,
                               3180947,
                               42537219,
                               36686420,
                               197711,
                               36686422,
                               36686419)

recommendedConceptSetExpression <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection,
                                                                              conceptIds = discoveredThruRecommender)
recommendedConceptSetExpression <- recommendedConceptSetExpression %>%
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE) %>%
  ConceptSetDiagnostics::getConceptSetDataFrameFromExpression() %>%
  dplyr::left_join(y = recommendedConceptSetExpression, by = 'conceptId')
conceptSetExpressionTable <- dplyr::union(conceptSetExpressionTable, recommendedConceptSetExpression)
blackList <- setdiff(designDiagnostic$recommendedStandard$conceptId, discoveredThruRecommender)
designDiagnostic <-
  performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionTable = conceptSetExpressionTable,
    exportResults = TRUE,
    locationForResults = locationForResults,
    blackList = blackList,
    iteration = 2
  )
############## complete standard

## no source codes recommended
############## complete non standard


readr::write_excel_csv(
  x = designDiagnostic$conceptSetExpressionTableOptimized,
  file = file.path(locationForResults, "finalConceptSetExpression.csv"),
  append = FALSE,
  na = ""
) 

DatabaseConnector::disconnect(connection = connection)
