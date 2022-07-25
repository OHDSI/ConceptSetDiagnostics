testthat::test_that("Concept Prevalence - connection", {
  if (dbms == "postgresql") {
    connection <-
      DatabaseConnector::connect(connectionDetails = connectionDetails)
    
    conceptPrevalence <- getConceptPrevalenceCounts(
      conceptIds = 0,
      connection = connection,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    
    testthat::expect_gte(object = nrow(conceptPrevalence),
                         expected = 0)
    
    recommendedStandard <- getRecommendedStandard(
      conceptIds = 0,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = connection,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    testthat::expect_gte(object = nrow(recommendedStandard),
                         expected = 0)
    
    recommendedSource <- getRecommendedSource(
      conceptIds = 0,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = connection,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    testthat::expect_gte(object = nrow(recommendedSource),
                         expected = 0)
    
    recommendedFromDataFrame <-
      getRecommendationForConceptSetExpression(
        conceptSetExpression = convertConceptSetDataFrameToExpression(
          conceptSetExpressionDataFrame = dplyr::tibble(conceptId = 0),
          selectAllDescendants = TRUE
        ),
        vocabularyDatabaseSchema = cdmDatabaseSchema,
        connection = connection,
        conceptPrevalenceSchema = "concept_prevalence"
      )
    testthat::expect_gte(object = nrow(recommendedStandard),
                         expected = 0)
    
    DatabaseConnector::disconnect(connection = connection)
  }
})


testthat::test_that("Concept Prevalence - connectionDetails", {
  if (dbms == "postgresql") {
    conceptPrevalence <- getConceptPrevalenceCounts(
      conceptIds = 0,
      connectionDetails = connectionDetails,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    
    testthat::expect_gte(object = nrow(conceptPrevalence),
                         expected = 0)
    
    recommendedStandard <- getRecommendedStandard(
      conceptIds = 0,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    testthat::expect_gte(object = nrow(recommendedStandard),
                         expected = 0)
    
    recommendedSource <- getRecommendedSource(
      conceptIds = 0,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails,
      conceptPrevalenceSchema = "concept_prevalence"
    )
    testthat::expect_gte(object = nrow(recommendedSource),
                         expected = 0)
    
    recommendedFromDataFrame <-
      getRecommendationForConceptSetExpression(
        conceptSetExpression = convertConceptSetDataFrameToExpression(
          conceptSetExpressionDataFrame = dplyr::tibble(conceptId = 0),
          selectAllDescendants = TRUE
        ),
        vocabularyDatabaseSchema = cdmDatabaseSchema,
        connectionDetails = connectionDetails,
        conceptPrevalenceSchema = "concept_prevalence"
      )
  }
})


testthat::test_that("Concept Prevalence - table does not exist", {
  if (dbms == "postgresql") {
    connection <-
      DatabaseConnector::connect(connectionDetails = connectionDetails)
    
    testthat::expect_error(
      getConceptPrevalenceCounts(
        conceptIds = 0,
        connection = connection,
        conceptPrevalenceSchema = cdmDatabaseSchema
      )
    )
    
    testthat::expect_error(
      getRecommendedStandard(
        conceptIds = 0,
        vocabularyDatabaseSchema = cdmDatabaseSchema,
        connection = connection,
        conceptPrevalenceSchema = cdmDatabaseSchema
      )
    )
    
    testthat::expect_error(
      getRecommendedSource(
        conceptIds = 0,
        vocabularyDatabaseSchema = cdmDatabaseSchema,
        connectionDetails = connectionDetails,
        conceptPrevalenceSchema = cdmDatabaseSchema
      )
    )
    
    DatabaseConnector::disconnect(connection = connection)
  }
})
