library(purrr)

source("HelperFunctions.R")

dbms <- "postgresql"
vocabularyDatabaseSchema <- "vocabulary_20220409"

connectionDetailsLocalPostgres <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    user = "postgres",
    password = "password",
    server = "localhost/postgres",
    port = 5432
  )

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = paste(
      Sys.getenv("phenotypeLibraryServer"),
      Sys.getenv("phenotypeLibrarydb"),
      sep = "/"
    ),
    user = Sys.getenv("phenotypeLibrarydbUser"),
    password = Sys.getenv("phenotypeLibrarydbPw"),
    port = Sys.getenv("phenotypeLibraryDbPort")
  )

if (connectionDetails$dbms == 'postgresql') {
  connectionRemote <- pool::dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = connectionDetails$dbms,
    server = connectionDetails$server(),
    port = connectionDetails$port(),
    user = connectionDetails$user(),
    password = connectionDetails$password(),
    connectionString = connectionDetails$connectionString()
  )
} else {
  connectionRemote <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
}

connectionLocal <-
  DatabaseConnector::connect(connectionDetails = connectionDetailsLocalPostgres)

vocabularyVersion <-
  ConceptSetDiagnostics::getVocabulary(connection = connectionLocal,
                                       vocabulary = vocabularyDatabaseSchema) %>%
  dplyr::filter(.data$vocabularyId == 'None') %>%
  dplyr::pull(.data$vocabularyVersion)

vocabulary <-
  ConceptSetDiagnostics::getVocabulary(
    connection = connectionLocal,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

domain <-
  ConceptSetDiagnostics::getDomain(
    connection = connectionLocal,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

relationship <-
  ConceptSetDiagnostics::getRelationship(
    connection = connectionLocal,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )
DatabaseConnector::disconnect(connection = connectionLocal)

onStop(function() {
  DatabaseConnector::disconnect(connection = connectionLocal)
  DatabaseConnector::disconnect(connection = connectionRemote)
})
