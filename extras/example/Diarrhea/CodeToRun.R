keyWords1 <- 'Diarrhea'

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
discoveredThruRecommender <- c(4193870,4166768, 201780, 4302676,4085622,4032877,198671,196623)
# 4193870	Bacterial dysentery	SNOMED	Condition	S	Not included - descendant	200	2	171813	20
# 4166768	Bacillary dysentery	SNOMED	Condition	S	Not included - descendant	200	2	171613	20
# conceptId	conceptName	vocabularyId	domainId	standardConcept	conceptInSet	rc	dc	drc	dbc
# 4302676	Amebic dysentery	SNOMED	Condition	S	Not included - descendant	300	3	56975	18
# 4085622	Acute amebic dysentery	SNOMED	Condition	S	Not included - descendant	14850	17	31967	18
# 4032877	Chronic amebiasis	SNOMED	Condition	S	Not included - descendant	340	3	24708	14
# 198671	Chronic intestinal amebiasis without abscess	SNOMED	Condition	S	Not included - descendant	24368	13	24368	13
# 196623	Acute amebic dysentery without abscess	SNOMED	Condition	S	Not included - descendant	17117	13	17117	13




recommendedConceptSetExpression <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection,
                                                                              conceptIds = discoveredThruRecommender)
recommendedConceptSetExpression <- recommendedConceptSetExpression %>%
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE) %>%
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression() %>%
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
