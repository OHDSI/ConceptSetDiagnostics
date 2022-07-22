testthat::test_that("Orphan Concepts - connection", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  orphanConcepts <- findOrphanConcepts(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = -1
  )
  testthat::expect_gte(object = nrow(orphanConcepts),
                       expected = 0)
  
  DatabaseConnector::disconnect(connection = connection)
})


testthat::test_that("Orphan Concepts - connectionDetails", {
  orphanConcepts <- findOrphanConcepts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = -1
  )
  testthat::expect_gte(object = nrow(orphanConcepts),
                       expected = 0)
  
})
