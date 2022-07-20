testthat::test_that("Get Concept Prevalence", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  concepts <-
    DatabaseConnector::renderTranslateQuerySql(
      conn = connection,
      sql = "SELECT concept_id,
                    concept_name,
                    vocabulary_id,
                    domain_id,
                    standard_concept
              FROM
              (   select CONCEPT_ID,
                        concept_name,
                        vocabulary_id,
                        domain_id,
                        standard_concept,
                        ROW_NUMBER() OVER() rn
                  from @vocabulary_database_schema.CONCEPT
              ) RN
            WHERE RN <= 100;",
      vocabulary_database_schema = cdmDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    ) %>%
    dplyr::tibble()
  
  concepts$rc <-
    sample(
      x = 1:nrow(concepts) * 2,
      size = nrow(concepts),
      replace = TRUE
    )
  concepts$drc <- concepts$rc * 2
  concepts$dbc <-
    sample(x = 1:2,
           size = nrow(concepts),
           replace = TRUE)
  concepts$ddbc <- concepts$dbc * 2
  
  tableName <- generateRandomString()
  
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = tableName,
    databaseSchema = cdmDatabaseSchema,
    data = concepts,
    camelCaseToSnakeCase = TRUE,
    dropTableIfExists = TRUE,
    createTable = TRUE
  )
  
  conceptPrevalenceCountConnection <-
    ConceptSetDiagnostics::getConceptPrevalenceCounts(
      conceptIds = concepts$conceptId,
      connection = connection,
      conceptPrevalenceTable = paste0(cdmDatabaseSchema, ".", tableName)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$drc))
  
  DatabaseConnector::disconnect(connection = connection)
  
  conceptPrevalenceCountConnectionDetails <-
    ConceptSetDiagnostics::getConceptPrevalenceCounts(
      conceptIds = concepts$conceptId,
      connectionDetails = connectionDetails,
      conceptPrevalenceTable = paste0(cdmDatabaseSchema, ".", tableName)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$drc))
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = "DROP TABLE IF EXISTS @cdm_database_schema.@table_name;",
    cdm_database_schema = cdmDatabaseSchema,
    table_name = tableName
  )
  
  testthat::expect_gte(object = nrow(conceptPrevalenceCountConnection),
                       expected = 0)
  
  testthat::expect_gte(
    object = nrow(conceptPrevalenceCountConnectionDetails),
    expected = 0
  )
})
