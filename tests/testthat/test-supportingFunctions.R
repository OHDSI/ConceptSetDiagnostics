testthat::test_that("Has Data", {
  testthat::expect_false(object = hasData(data = NULL))
  testthat::expect_false(object = hasData(data = dplyr::tibble()))
  testthat::expect_false(object = hasData(data = c()))
  testthat::expect_false(object = hasData(data = c("")))
  testthat::expect_false(object = hasData(data = c(NA)))
  testthat::expect_false(object = hasData(data = vector(mode = "numeric", length = 0)))
  testthat::expect_true(object = hasData(data = dplyr::tibble(a = 1)))
})

testthat::test_that("Check if cohort definition set", {
  cohortDefinitionSet <- dplyr::tibble(cohortId = 1)
  testthat::expect_null(checkIfCohortDefinitionSet(cohortDefinitionSet = cohortDefinitionSet))

  cohortDefinitionSet <- dplyr::tibble(cohort_id = 1)
  errorMessage <-
    checkIfCohortDefinitionSet(cohortDefinitionSet = cohortDefinitionSet)
  testthat::expect_false(object = errorMessage$isEmpty())
})
