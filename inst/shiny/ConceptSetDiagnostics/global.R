library(magrittr)
source("HelperFunctions.R")

# Used for Testing with old DB connection
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(Sys.getenv("phoebedbServer"),
                   Sys.getenv("phoebedb"),
                   sep = "/"),
    user = Sys.getenv("phoebedbUser"),
    password = Sys.getenv("phoebedbPw"),
    port = Sys.getenv("shinydbPort")
  )

# connectionDetails <-
#   DatabaseConnector::createConnectionDetails(
#     dbms = "postgresql",
#     server = paste(
#       Sys.getenv("phenotypeLibraryDbServer"),
#       Sys.getenv("phenotypeLibraryDbDatabase"),
#       sep = "/"
#     ),
#     user = Sys.getenv("shinyDbUserGowtham"),
#     password = Sys.getenv("shinyDbPasswordGowtham"),
#     port = Sys.getenv("phenotypeLibraryDbPort")
#   )
connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

defaultVocabularySchema <- "vocabulary"


sqlVocabulary <-
  "SELECT * FROM @vocabulary_database_schema.vocabulary;"
sqlDomains <- "SELECT * FROM @vocabulary_database_schema.domain;"


vocabulary <-
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sqlVocabulary,
    snakeCaseToCamelCase = TRUE,
    vocabulary_database_schema = defaultVocabularySchema
  ) %>%
  dplyr::tibble()

domain <-
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sqlDomains,
    snakeCaseToCamelCase = TRUE,
    vocabulary_database_schema = defaultVocabularySchema
  ) %>%
  dplyr::tibble()

vocabularyVersion <- vocabulary %>%
  dplyr::filter(.data$vocabularyId == 'None') %>%
  dplyr::pull(.data$vocabularyVersion)
