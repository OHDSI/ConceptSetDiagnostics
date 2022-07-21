connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

testthat::test_that("String search - connection", {
  testthat::expect_error(
    performStringSearchForConcepts(
      searchString = "d",
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = connection
    )
  )
  output <- performStringSearchForConcepts(
    searchString = "diabetes",
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    connection = connection
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

DatabaseConnector::disconnect(connection = connection)

testthat::test_that("String search - connectionDetails", {
  output <- performStringSearchForConcepts(
    searchString = "diabetes mellitus",
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    connectionDetails = connectionDetails
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})
