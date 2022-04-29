library(magrittr)
library(purrr)
source("HelperFunctions.R")

dbms <- "postgresql"
vocabularyDatabaseSchema <- "vocabulary"

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = paste(
      Sys.getenv("shinyDbServer"),
      Sys.getenv("shinydbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUser"),
    password = Sys.getenv("shinyDbPassword"),
    port = Sys.getenv("shinydbPort")
  )

if (connectionDetails$dbm == 'postgresql') {
  connection <- pool::dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = connectionDetails$dbms,
    server = connectionDetails$server(),
    port = connectionDetails$port(),
    user = connectionDetails$user(),
    password = connectionDetails$password(),
    connectionString = connectionDetails$connectionString()
  )
} else {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
}

vocabularyVersion <-
  ConceptSetDiagnostics::getVocabulary(connection = connection,
                                       vocabulary = vocabularyDatabaseSchema) %>%
  dplyr::filter(.data$vocabularyId == 'None') %>%
  dplyr::pull(.data$vocabularyVersion)

vocabulary <-
  ConceptSetDiagnostics::getVocabulary(
    connection = connection,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

domain <-
  ConceptSetDiagnostics::getDomain(
    connection = connection,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

relationship <-
  ConceptSetDiagnostics::getRelationship(
    connection = connection,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )

onStop(function() {
  if (DBI::dbIsValid(dbObj = connection)) {
    if (methods::is(object = connection,
                    class2 = "Pool")) {
      writeLines("Closing database pool")
      pool::poolClose(pool = connection)
    } else {
      DatabaseConnector::disconnect(connection = connection)
    }
  }
})
