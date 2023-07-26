testthat::test_that("extractConceptSetsInCohortDefinition", {
  testthat::expect_warning(
    extractConceptSetsInCohortDefinition(cohortExpression = cohortsExpressionNoConceptSet)
  )
  testthat::expect_null(object = suppressWarnings(
    extractConceptSetsInCohortDefinition(cohortExpression = cohortsExpressionNoConceptSet)
  ))

  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinition(cohortExpression = cohortExpression)
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort),
    expected = 0
  )

  a <- list()
  a$expression <- cohortExpression
  conceptSetsInCohort2 <-
    extractConceptSetsInCohortDefinition(cohortExpression = a)
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort2),
    expected = 0
  )
})

testthat::test_that("Check if cohort definition set", {
  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinitionSet(cohortDefinitionSet = cohortDefinitionSet)
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort),
    expected = 0
  )
})

testthat::test_that("optimizeConceptSetExpression", {
  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinition(cohortExpression = cohortExpression)

  conceptSetExpression1 <-
    conceptSetsInCohort[1, ]$conceptSetExpression |>
    RJSONIO::fromJSON(digits = 23)

  conceptSetsInCohort1 <-
    optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression1,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = length(conceptSetsInCohort1),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort1$recommended),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort1$optimizedConceptSet),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort1$removed),
    expected = 0
  )

  conceptSetsInCohort2 <-
    optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression1,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = DatabaseConnector::connect(connectionDetails)
    )
  testthat::expect_gte(
    object = length(conceptSetsInCohort2),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort2$optimizedConceptSet),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort2$recommended),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort2$removed),
    expected = 0
  )


  conceptSetExpression3 <-
    conceptSetsInCohort[2, ]$conceptSetExpression |>
    RJSONIO::fromJSON(digits = 23)

  conceptSetsInCohort1 <-
    optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression3,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = length(conceptSetsInCohort1),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort1$recommended),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort1$removed),
    expected = 0
  )
  testthat::expect_gte(
    object = nrow(conceptSetsInCohort1$optimizedConceptSet),
    expected = 0
  )
})

testthat::test_that("resolveConceptSetExpression", {
  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinition(cohortExpression = cohortExpression)

  conceptSetExpression1 <-
    conceptSetsInCohort[1, ]$conceptSetExpression |>
    RJSONIO::fromJSON(digits = 23)

  resolvedConceptSet1 <-
    resolveConceptSetExpression(
      conceptSetExpression = conceptSetExpression1,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet1),
    expected = 0
  )

  conceptSetExpression2 <-
    conceptSetsInCohort[2, ]$conceptSetExpression |>
    RJSONIO::fromJSON(digits = 23)

  resolvedConceptSet3 <-
    resolveConceptSetExpression(
      conceptSetExpression = conceptSetExpression2,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet3),
    expected = 0
  )

  resolvedConceptSet2 <-
    resolveConceptSetExpression(
      conceptSetExpression = conceptSetExpression1,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = DatabaseConnector::connect(connectionDetails)
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet2),
    expected = 0
  )
})


testthat::test_that("resolveConceptSetExpression", {
  resolvedConceptSet <-
    resolveConceptSetsInCohortExpression(
      cohortExpression = cohortExpression,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet),
    expected = 0
  )

  resolvedConceptSet2 <-
    resolveConceptSetsInCohortExpression(
      cohortExpression = cohortExpression,
      connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet2),
    expected = 0
  )
})

testthat::test_that("getExcludedConceptsInConceptSetExpression", {
  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinition(cohortExpression = cohortExpression)

  conceptSetExpressionWithExclusion <-
    conceptSetsInCohort[3, ]$conceptSetExpression |>
    RJSONIO::fromJSON(digits = 23)

  excludedConcepts1 <-
    getExcludedConceptsInConceptSetExpression(
      conceptSetExpression = conceptSetExpressionWithExclusion,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(
    object = nrow(excludedConcepts1),
    expected = 0
  )

  excludedConcepts2 <-
    getExcludedConceptsInConceptSetExpression(
      conceptSetExpression = conceptSetExpressionWithExclusion,
      connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(
    object = nrow(excludedConcepts2),
    expected = 0
  )
})
