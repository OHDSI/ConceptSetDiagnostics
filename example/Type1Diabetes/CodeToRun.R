keyWords1 <- 'Type 1 Diabetes'
keyWords2 <- 'Type I Diabetes'
keyWords3 <- 'Mellitus Type 1'
keyWords4 <- 'Mellitus Type I'


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
discoveredThruRecommender <- c()
#conceptId	conceptName	vocabularyId	domainId	standardConcept	conceptInSet	rc	dc	drc	dbc
# 201820	Diabetes mellitus	SNOMED	Condition	S	Not included - parent	23196665	18	1487191054	22
# 380097	Macular edema due to diabetes mellitus	SNOMED	Condition	S	Not included - recommended via source	8727283	14	13616831	14
# 376114	Severe nonproliferative retinopathy due to diabetes mellitus	SNOMED	Condition	S	Not included - recommended via source	602109	14	1921293	14
# 45757079	Pre-existing diabetes mellitus in mother complicating childbirth	SNOMED	Condition	S	Not included - recommended via source	179404	12	179404	12


## although the standard codes are not verbatin Type 1 Diabetes Mellitus, the non standard are
## this is a mapping gap that we report to the vocabulary team
## in the interim - should the type 1 diabetes source code be used as is - or should the standard codes
## be used.
## 
## but many of them are covered by proliferative retinopathy type 1

# manual process

readr::write_excel_csv(
  x = designDiagnostic$conceptSetExpressionTableOptimized,
  file = file.path(locationForResults, "finalConceptSetExpression.csv"),
  append = FALSE,
  na = ""
) 

DatabaseConnector::disconnect(connection = connection)
