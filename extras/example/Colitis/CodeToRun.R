keyWords1 <- 'Colitis'

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
  ConceptSetDiagnostics::performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionDataFrame = conceptSetExpressionTable,
    exportResults = TRUE,
    locationForResults = locationForResults,
    iteration = 1
  )




############### review only standard for round 1
discoveredThruRecommender <- c(77025, 4342751, 
                               4187875,	
                               4322739,	
                               4298577,
                               4326827,
                               46273469,
                               4201583,
                               4179201,
                               4093449,
                               4171367)
# conceptId	conceptName	vocabularyId	domainId	standardConcept	conceptInSet	rc	dc	drc	dbc
# 77025	Diverticulitis of colon	SNOMED	Condition	S	Not included - descendant	22135417	15	22155542	16
# 4342751	Sigmoiditis	SNOMED	Condition	S	Not included - descendant	252	2	667686	19
# 4187875	Proctosigmoiditis	SNOMED	Condition	S	Not included - descendant	3544	2	652187	18
# 4322739	Diverticulitis of sigmoid colon	SNOMED	Condition	S	Not included - descendant	14080	4	14847	4
# 4298577	Diverticulitis of colon with perforation	SNOMED	Condition	S	Not included - descendant	3545	3	3969	3
# 4326827	Diverticulitis of rectosigmoid	SNOMED	Condition	S	Not included - descendant	1733	2	1733	2
# 46273469	Perforation of sigmoid colon due to diverticulitis	SNOMED	Condition	S	Not included - descendant	424	1	424	1
# 4201583	Noninfectious sigmoiditis	SNOMED	Condition	S	Not included - descendant	200	2	400	2
# 4179201	Perisigmoiditis	SNOMED	Condition	S	Not included - descendant	100	1	100	1
# 4093449	Allergic sigmoiditis	SNOMED	Condition	S	Not included - descendant	100	1	100	1
# 4171367	Dietetic sigmoiditis	SNOMED	Condition	S	Not included - descendant	100	1	100	1





discoveredConceptIdExpression <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection, 
                                                                   conceptIds = discoveredThruRecommender) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE)
discoveredConceptIdsResolved1 <-  ConceptSetDiagnostics::resolveConceptSetExpression(conceptSetExpression = discoveredConceptIds1,
                                                                                     connection = connection)
blackList <- setdiff(discoveredConceptIds$resolvedConcepts$conceptId, discoveredConceptIds$mappedConcepts$conceptId)

discoveredConceptIds1 <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection, 
                                                                    conceptIds = discoveredThruRecommender) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE) 

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
