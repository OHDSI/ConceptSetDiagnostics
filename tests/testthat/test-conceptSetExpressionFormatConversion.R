testthat::test_that("Get Concept Set Expression Format conversion - connection", {
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  testthat::expect_error(
    conceptSetExpressionConnection <-
      convertConceptSetDataFrameToExpression(
        conceptSetExpressionDataFrame = dplyr::tibble(conceptId = 0),
        selectAllDescendants = TRUE,
        updateVocabularyFields = TRUE,
        connection = connection
      )
  )
  
  conceptSetExpressionConnection <-
    convertConceptSetDataFrameToExpression(
      conceptSetExpressionDataFrame = dplyr::tibble(conceptId = 0),
      selectAllDescendants = TRUE,
      updateVocabularyFields = TRUE,
      connection = connection,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_is(object = conceptSetExpressionConnection, class = "list")
  testthat::expect_gte(object = length(conceptSetExpressionConnection),
                       expected = 0)
  
  testthat::expect_error(
    conceptSetExpressionDataFrame <-
      convertConceptSetExpressionToDataFrame(
        conceptSetExpression = NULL,
        updateVocabularyFields = TRUE,
        connection = connection
      )
  )
  
  testthat::expect_error(
    conceptSetExpressionDataFrame <-
      convertConceptSetExpressionToDataFrame(
        conceptSetExpression = conceptSetExpressionConnection,
        updateVocabularyFields = TRUE,
        connection = connection
      )
  )
  
  conceptSetExpressionDataFrame <-
    convertConceptSetExpressionToDataFrame(
      conceptSetExpression = conceptSetExpressionConnection,
      updateVocabularyFields = TRUE,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  testthat::expect_is(object = conceptSetExpressionDataFrame, class = "data.frame")
  testthat::expect_gte(object = nrow(conceptSetExpressionDataFrame),
                       expected = 0)
  
  conceptSetExpressionDataFrameSkipItems <-
    convertConceptSetExpressionToDataFrame(
      conceptSetExpression = conceptSetExpressionConnection$items,
      updateVocabularyFields = TRUE,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  testthat::expect_is(object = conceptSetExpressionDataFrameSkipItems, class = "data.frame")
  testthat::expect_gte(
    object = nrow(conceptSetExpressionDataFrameSkipItems),
    expected = 0
  )
  
  conceptSetExpressModified <- conceptSetExpressionConnection$items
  conceptSetExpressModified[[1]]$isExcluded <- NULL
  conceptSetExpressModified[[1]]$includeDescendants <- NULL
  conceptSetExpressModified[[1]]$includeMapped <- NULL
  conceptSetExpressModified[[1]]$concept$STANDARD_CONCEPT_CAPTION <- NULL
  conceptSetExpressModified[[1]]$concept$INVALID_REASON_CAPTION <- NULL
  
  conceptSetExpressionDataFrameModified <-
    convertConceptSetExpressionToDataFrame(
      conceptSetExpression = conceptSetExpressModified,
      updateVocabularyFields = FALSE,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  conceptSetExpressModified[[1]]$concept$STANDARD_CONCEPT_CAPTION <- ""
  conceptSetExpressModified[[1]]$concept$STANDARD_CONCEPT <- NULL
  
  DatabaseConnector::disconnect(connection = connection)
})


testthat::test_that("Get Concept Set Expression Format conversion - connection details",
                    {
                      conceptSetExpressionConnection <-
                        convertConceptSetDataFrameToExpression(
                          conceptSetExpressionDataFrame = dplyr::tibble(conceptId = 0),
                          selectAllDescendants = TRUE,
                          updateVocabularyFields = TRUE,
                          connectionDetails = connectionDetails,
                          vocabularyDatabaseSchema = cdmDatabaseSchema
                        )
                      testthat::expect_is(object = conceptSetExpressionConnection, class = "list")
                      testthat::expect_gte(object = length(conceptSetExpressionConnection),
                                           expected = 0)
                      
                      
                      conceptSetExpressionDataFrame <-
                        convertConceptSetExpressionToDataFrame(
                          conceptSetExpression = conceptSetExpressionConnection,
                          updateVocabularyFields = TRUE,
                          connectionDetails = connectionDetails,
                          vocabularyDatabaseSchema = vocabularyDatabaseSchema
                        )
                      
                      testthat::expect_is(object = conceptSetExpressionDataFrame, class = "data.frame")
                      
                      testthat::expect_gte(object = nrow(conceptSetExpressionDataFrame),
                                           expected = 0)
                    })
