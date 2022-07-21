testthat::test_that("Orphan Concepts - connection", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  orphanConcepts <- getOrphanConcepts(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = 320128
  )
  testthat::expect_gte(object = nrow(orphanConcepts),
                       expected = 0)
  
  DatabaseConnector::disconnect(connection = connection)
})


testthat::test_that("Orphan Concepts - connectionDetails", {
  orphanConcepts <- getOrphanConcepts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = 0
  )
  testthat::expect_gte(object = nrow(orphanConcepts),
                       expected = 0)
  
})
