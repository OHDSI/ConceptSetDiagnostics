keyWords <- 'urticaria'
keyWords2 <- 'eczema'


folder <- stringr::str_replace_all(string = keyWords,
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
                                                 searchString = keyWords)
conceptSetExpressionTable2 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords2)
conceptSetExpressionTable <-
  dplyr::bind_rows(conceptSetExpressionTable1,
                   conceptSetExpressionTable2)

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
discoveredThruRecommender <- c(4297831,
                               4224625,
                               4286491,
                               4297825
)
# conceptId	conceptName	vocabularyId	domainId	standardConcept	conceptInSet	rc	dc	drc	dbc
# 4297831	Serum sickness due to drug	SNOMED	Observation	S	Not included - descendant	472	2	472	2
# 4224625	Angioedema of eyelids	SNOMED	Condition	S	Not included - descendant	201	2	201	2
# 4286491	Cold erythema associated with cold agglutinins	SNOMED	Condition	S	Not included - descendant	200	2	200	2
# 4297825	Serum sickness type vasculitis	SNOMED	Condition	S	Not included - descendant	100	1	200	2


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
