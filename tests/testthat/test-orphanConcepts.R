testthat::test_that("Orphan Concepts - connection", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)

  orphanConcepts <- findOrphanConcepts(
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = -1
  )
  testthat::expect_gte(
    object = nrow(orphanConcepts),
    expected = 0
  )

  orphanConceptDf <- findOrphanConceptsForConceptSetExpression(
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptSetExpression = convertConceptSetDataFrameToExpression(
      conceptSetExpressionDataFrame = dplyr::tibble(conceptId = -1),
      selectAllDescendants = TRUE
    )
  )
  testthat::expect_gte(
    object = nrow(orphanConceptDf),
    expected = 0
  )

  DatabaseConnector::disconnect(connection = connection)
})


testthat::test_that("Orphan Concepts - connectionDetails", {
  orphanConcepts <- findOrphanConcepts(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptIds = -1
  )
  testthat::expect_gte(
    object = nrow(orphanConcepts),
    expected = 0
  )

  orphanConceptDf <- findOrphanConceptsForConceptSetExpression(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema,
    conceptSetExpression = convertConceptSetDataFrameToExpression(
      conceptSetExpressionDataFrame = dplyr::tibble(conceptId = -1),
      selectAllDescendants = TRUE
    )
  )
  testthat::expect_gte(
    object = nrow(orphanConceptDf),
    expected = 0
  )
})
