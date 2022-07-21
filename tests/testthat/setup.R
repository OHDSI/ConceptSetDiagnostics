library(testthat)
library(ConceptSetDiagnostics)
library(dplyr)

cohortsJson <-
  SqlRender::readSql(sourceFile = system.file(file.path("cohorts", "14906.json"),
    package = "ConceptSetDiagnostics"
  ))

cohortExpression <- cohortsJson %>%
  RJSONIO::fromJSON(digits = 23)

cohortsSql <-
  SqlRender::readSql(sourceFile = system.file(file.path("cohorts", "14906.sql"),
    package = "ConceptSetDiagnostics"
  ))

cohortDefinitionSet <-
  readr::read_csv(system.file(file.path("cohorts", "CohortDefinitionSet.csv"),
    package = "ConceptSetDiagnostics"
  ),
  col_types = readr::cols()
  ) %>%
  dplyr::mutate(
    json = cohortsJson,
    sql = cohortsSql
  )

generateRandomString <- function() {
  randomStringTableName <-
    tolower(paste0(
      "tmp_",
      paste0(sample(
        x = c(LETTERS, 0:9),
        size = 12,
        replace = TRUE
      ), collapse = "")
    ))
  return(randomStringTableName)
}

dbms <- getOption("dbms", default = "postgresql")
message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- tempfile("jdbcDrivers")
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)

  if (!dbms %in% c("postgresql")) {
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)
  }

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

folder <- tempfile()
dir.create(folder, recursive = TRUE)
skipCdmTests <- FALSE


if (dbms == "postgresql") {
  dbUser <- Sys.getenv("CDM5_POSTGRESQL_USER")
  dbPassword <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
  dbServer <- Sys.getenv("CDM5_POSTGRESQL_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
} else if (dbms == "oracle") {
  dbUser <- Sys.getenv("CDM5_ORACLE_USER")
  dbPassword <- Sys.getenv("CDM5_ORACLE_PASSWORD")
  dbServer <- Sys.getenv("CDM5_ORACLE_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  tempEmulationSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  cohortDatabaseSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  options(sqlRenderTempEmulationSchema = tempEmulationSchema)
} else if (dbms == "redshift") {
  dbUser <- Sys.getenv("CDM5_REDSHIFT_USER")
  dbPassword <- Sys.getenv("CDM5_REDSHIFT_PASSWORD")
  dbServer <- Sys.getenv("CDM5_REDSHIFT_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
} else if (dbms == "sql server") {
  dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
  dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
  dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
}

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = dbms,
  user = dbUser,
  password = URLdecode(dbPassword),
  server = dbServer,
  pathToDriver = jdbcDriverFolder
)

if (cdmDatabaseSchema == "" || dbServer == "") {
  skipCdmTests <- TRUE
}


withr::defer(
  {
  },
  testthat::teardown_env()
)
