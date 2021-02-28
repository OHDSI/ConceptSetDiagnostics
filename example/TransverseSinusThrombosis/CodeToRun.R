keyWords1 <- 'Transverse Sinus Thrombosis'


folder <- stringr::str_replace_all(string = keyWords1,
                                   pattern = " ",
                                   replacement = "")
# Load the package
library(ConceptSetDiagnostics)

locationForResults <- file.path(rstudioapi::getActiveProject(), 'example', folder)
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
conceptSetExpressionTable <- conceptSetExpressionTable1

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
discoveredThruRecommender <- c(4102202,
                               764710
)
# 4102202	Cerebral venous sinus thrombosis	SNOMED	Condition	S	Not included - parent	4166	3	135078	19
# 764710	Thrombophlebitis of transverse sinus	SNOMED	Condition	S	Not included - descendant	0	0	0	0
recommendedConceptSetExpression <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection, 
                                                                              conceptIds = discoveredThruRecommender)
recommendedConceptSetExpression <- recommendedConceptSetExpression %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptTable(selectAllDescendants = TRUE) %>% 
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
