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

vocabularyVersion <- ConceptSetDiagnostics::getVocabulary(connection = connection, 
                                                          vocabulary = defaultVocabularySchema) %>%
  dplyr::filter(.data$vocabularyId == 'None') %>%
  dplyr::pull(.data$vocabularyVersion)
