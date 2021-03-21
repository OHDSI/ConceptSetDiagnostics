keyWords1 <- 'Idiopathic Peripheral Neuropathy'

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
### decision - none selected

############## complete standard

############## review source code recommended
discoveredThruRecommender <- c(4175154 # in source code idiopathic peripheral neuropathy 
                               # is mapped to disorder of peripheral nervous system.
                               # we will have to learn the impact of this mapping, using 
                               # three conceptSet expressions - one without this code,
                               # two with this standard code, 
                               # with only the non standard code
# sourceConceptId	sourceConceptName	sourceVocabularyId	sourceConceptCode	sourceRecordCount	sourceDatabaseCount
# 44826563	Unspecified hereditary and idiopathic peripheral neuropathy	ICD9CM	356.9	29198270	14
# 44824170	Hereditary and idiopathic peripheral neuropathy	ICD9CM	356	15441	8
      
# conceptId	conceptName	domainId	vocabularyId	standardConcept	rc	dc	drc	dbc
# 4175154	Disorder of the peripheral nervous system	Condition	SNOMED	S	1953226	16	879780565	22

)
######### no more iteration because non standard code
############## complete non standard


readr::write_excel_csv(
  x = designDiagnostic$conceptSetExpressionTableOptimized,
  file = file.path(locationForResults, "finalConceptSetExpression.csv"),
  append = FALSE,
  na = ""
) 

DatabaseConnector::disconnect(connection = connection)
