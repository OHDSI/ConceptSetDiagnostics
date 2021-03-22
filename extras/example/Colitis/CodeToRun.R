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
  file.path(rstudioapi::getActiveProject(), 'example', outputLocation)
dir.create(path = locationForResults,
           recursive = TRUE,
           showWarnings = FALSE)


# get search results
searchResult <- list()

for (i in (1:length(keyWords))) {
  
  designDiagnostics <- ConceptSetDiagnostics::performDesignDiagnosticsOnSearchTerm(searchString = keyWords[[i]], 
                                                                                   exportResults = FALSE,
                                                                                   locationForResults = '',
                                                                                   vocabularyIdOfInterest = vocabularyIdOfInterest,
                                                                                   domainIdOfInterest = domainIdOfInterest, 
                                                                                   connection = connection)
  
  searchResult[[i]] <- list(keyWord = keyWords[[i]],
                            searchResultConceptIds = searchResultConceptIds,
                            conceptSetExpressionDataFrame = conceptSetExpressionDataFrame,
                            resolvedConceptIds = resolvedConceptIds)
}



##  do automated processing based on search results
designDiagnostic1 <-
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
                               4057826,
                               4261072,
                               4234788,
                               4341644,
                               4340374,
                               4341645,
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
# 4261072	Irritable bowel syndrome characterized by constipation	SNOMED	Condition	S	Not included - descendant	840039	14	840039	14
# 4234788	Irritable bowel syndrome characterized by alternating bowel habit	SNOMED	Condition	S	Not included - recommended via source	600434	13	600434	13
# 4341644	Irritable bowel syndrome variant of childhood	SNOMED	Condition	S	Not included - descendant	100	1	300	1
# 4340374	Irritable bowel syndrome variant of childhood with constipation	SNOMED	Condition	S	Not included - descendant	100	1	100	1
# 4341645	Irritable bowel syndrome variant of childhood with diarrhea	SNOMED	Condition	S	Not included - descendant	100	1	100	1




discoveredExpression1 <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection, 
                                                                   conceptIds = discoveredThruRecommender) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE) %>% 
ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(updateVocabularyFields = TRUE, connection = connection) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame()

blackList1 <- ConceptSetDiagnostics::resolveConceptSetExpression(conceptSetExpression = discoveredExpression1, connection = connection)


conceptSetExpressionTable1 <- dplyr::bind_rows(conceptSetExpressionTable, discoveredConceptIdExpressionDataFrame1) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame() %>% 
  ConceptSetDiagnostics::optimizeConceptSetExpression(connection = connection) %>% 
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression()


designDiagnostic2 <-
  ConceptSetDiagnostics::performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionDataFrame = conceptSetExpressionTable,
    exportResults = TRUE,
    blackList = blackList,
    locationForResults = locationForResults,
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
