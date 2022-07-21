testthat::test_that("extractConceptSetsInCohortDefinition", {
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

  conceptSetExpression <-
    conceptSetsInCohort[1, ]$conceptSetExpression %>%
    RJSONIO::fromJSON(digits = 23)

  conceptSetsInCohort <-
    optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = length(conceptSetsInCohort),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort$recommended),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort$removed),
    expected = 0
  )

  conceptSetsInCohort2 <-
    optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = DatabaseConnector::connect(connectionDetails)
    )
  testthat::expect_gte(
    object = length(conceptSetsInCohort2),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort2$recommended),
    expected = 0
  )
  testthat::expect_gte(
    object = length(conceptSetsInCohort2$removed),
    expected = 0
  )
})

testthat::test_that("resolveConceptSetExpression", {
  conceptSetsInCohort <-
    extractConceptSetsInCohortDefinition(cohortExpression = cohortExpression)

  conceptSetExpression <-
    conceptSetsInCohort[1, ]$conceptSetExpression %>%
    RJSONIO::fromJSON(digits = 23)

  resolvedConceptSet <-
    resolveConceptSetExpression(
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connectionDetails = connectionDetails
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet),
    expected = 0
  )

  resolvedConceptSet2 <-
    resolveConceptSetExpression(
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = cdmDatabaseSchema,
      connection = DatabaseConnector::connect(connectionDetails)
    )
  testthat::expect_gte(
    object = nrow(resolvedConceptSet2),
    expected = 0
  )
})
