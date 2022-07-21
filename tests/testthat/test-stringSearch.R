connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

testthat::test_that("String search - connection", {
  testthat::expect_error(
    getStringSearchConcepts(
      searchString = "d",
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = connection
    )
  )
  output <- getStringSearchConcepts(
    searchString = "diabetes",
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    connection = connection
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

DatabaseConnector::disconnect(connection = connection)

testthat::test_that("String search - connectionDetails", {
  output <- getStringSearchConcepts(
    searchString = "diabetes mellitus",
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    connectionDetails = connectionDetails
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})
