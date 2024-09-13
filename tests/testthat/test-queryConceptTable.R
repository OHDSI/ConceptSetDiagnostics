connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

# getConceptAncestor 1 ----
testthat::test_that("Get Concept Ancestor - connection", {
  output <- getConceptAncestor(
    connection = connection,
    conceptIds = 0,
    # 381316,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptDescendant 1 ----
testthat::test_that("Get Concept Descendant - connection", {
  output <- getConceptDescendant(
    conceptIds = 0,
    # 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptIdDetails 1 ----
testthat::test_that("Get Concept Details - connection", {
  output <- getConceptIdDetails(
    conceptIds = 0,
    # 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  ) |>
    dplyr::arrange(.data$conceptId)
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptRelationship 1 ----
testthat::test_that("Get Concept Relationship - connection", {
  output <- getConceptRelationship(
    conceptIds = 0,
    # c(192671, 35208414, 1118088, 35208414),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptSynonym 1 ----
testthat::test_that("Get Concept Synonym - connection", {
  output <- getConceptSynonym(
    conceptIds = 0,
    # 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMappedSourceConcepts 1 ----
testthat::test_that("Get Mapped Source Concept - connection", {
  output <- getMappedSourceConcepts(
    conceptIds = 0,
    # c(35208414, 192671, 1118088, 35208414),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMappedStandardConcepts 1 ----
testthat::test_that("Get Mapped Standard Concept - connection", {
  output <- getMappedStandardConcepts(
    conceptIds = 0,
    # c(35208414, 44923712),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getDomain 1 ----
testthat::test_that("Get Domain - connection", {
  output <- getDomain(
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getRelationship 1 ----
testthat::test_that("Get Relationship - connection", {
  output <-
    getRelationship(
      connection = connection,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getVocabulary 1 ----
testthat::test_that("Get Vocabulary - connection", {
  output <-
    getVocabulary(
      connection = connection,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getVocabularyVersion 1 ----
testthat::test_that("Get Vocabulary Version - connection", {
  output <-
    getVocabularyVersion(
      connection = connection,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getDrugIngredients 1 ----
testthat::test_that("Get Drug Ingredients - connection", {
  output <- getDrugIngredients(
    connection = connection,
    conceptIds = 0,
    # c(1127078, 1127433),
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMedraRelationship 1 ----
testthat::test_that("Get MedRa Relationship - connection", {
  output <- getMedraRelationship(
    connection = connection,
    conceptIds = 0,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = length(output), expected = 0)
  testthat::expect_equal(object = class(output), expected = "list")
  testthat::expect_gte(
    object = length(output$givenConceptId),
    expected = 0
  )
  testthat::expect_gte(object = nrow(output$soc), expected = 0)
  testthat::expect_gte(object = nrow(output$hlgt), expected = 0)
  testthat::expect_gte(object = nrow(output$hlt), expected = 0)
  testthat::expect_gte(object = nrow(output$pt), expected = 0)
  testthat::expect_gte(object = nrow(output$llt), expected = 0)
})

# mapMedraToSnomedViaVocabulary 1 ----
testthat::test_that("Map MedDra to Snomed - connection", {
  output <- mapMedraToSnomedViaVocabulary(
    connection = connection,
    conceptIds = 0,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})


# getCountOfSourceCodesMappedToStandardConcept 1 ----
testthat::test_that("Source codes Mapped to Standard Concept - connection", {
  output <-
    getCountOfSourceCodesMappedToStandardConcept(
      connection = connection,
      conceptIds = 0,
      cdmDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptRecordCount 1 ----
testthat::test_that("Source codes Mapped to Standard Concept - connection", {
  output <-
    getConceptRecordCount(
      connection = connection,
      conceptIds = 0,
      cdmDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# Disconnection ----
DatabaseConnector::disconnect(connection = connection)




# getConceptAncestor 2 ----
testthat::test_that("Get Concept Ancestor - connectionDetails", {
  output <- getConceptAncestor(
    connectionDetails = connectionDetails,
    conceptIds = 0,
    # 381316,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptDescendant 2 ----
testthat::test_that("Get Concept Descendant - connectionDetails", {
  output <- getConceptDescendant(
    conceptIds = 0,
    # 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptIdDetails 2 ----
testthat::test_that("Get Concept Details - connectionDetails", {
  output <- getConceptIdDetails(
    conceptIds = 0,
    # 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  ) |>
    dplyr::arrange(.data$conceptId)
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptRelationship 2 ----
testthat::test_that("Get Concept Relationship - connectionDetails", {
  output <- getConceptRelationship(
    conceptIds = 0,
    # c(192671, 35208414, 1118088, 35208414),
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptSynonym 2 ----
testthat::test_that("Get Concept Synonym - connectionDetails", {
  output <- getConceptSynonym(
    conceptIds = 0,
    # 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMappedSourceConcepts 2 ----
testthat::test_that("Get Mapped Source Concept - connectionDetails", {
  output <- getMappedSourceConcepts(
    conceptIds = 0,
    # c(35208414, 192671, 1118088, 35208414),
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMappedStandardConcepts 2 ----
testthat::test_that("Get Mapped Standard Concept - connectionDetails", {
  output <- getMappedStandardConcepts(
    conceptIds = 0,
    # c(35208414, 44923712),
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getDomain 2 ----
testthat::test_that("Get Domain - connectionDetails", {
  output <-
    getDomain(
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getRelationship 2 ----
testthat::test_that("Get Relationship - connectionDetails", {
  output <-
    getRelationship(
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getVocabulary 2 ----
testthat::test_that("Get Vocabulary - connectionDetails", {
  output <-
    getVocabulary(
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getVocabularyVersion 2 ----
testthat::test_that("Get Vocabulary Version - connectionDetails", {
  output <-
    getVocabularyVersion(
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getDrugIngredients 2 ----
testthat::test_that("Get Drug Ingredients - connectionDetails", {
  output <- getDrugIngredients(
    connectionDetails = connectionDetails,
    conceptIds = 0,
    # c(1127078, 1127433),
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getMedraRelationship 2 ----
testthat::test_that("Get MedRa Relationship - connectionDetails", {
  output <- getMedraRelationship(
    connectionDetails = connectionDetails,
    conceptIds = 0,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = length(output), expected = 0)
  testthat::expect_equal(object = class(output), expected = "list")
  testthat::expect_gte(
    object = length(output$givenConceptId),
    expected = 0
  )
  testthat::expect_gte(object = nrow(output$soc), expected = 0)
  testthat::expect_gte(object = nrow(output$hlgt), expected = 0)
  testthat::expect_gte(object = nrow(output$hlt), expected = 0)
  testthat::expect_gte(object = nrow(output$pt), expected = 0)
  testthat::expect_gte(object = nrow(output$llt), expected = 0)
})

# mapMedraToSnomedViaVocabulary 2 ----
testthat::test_that("Map MedDra to Snomed - connectionDetails", {
  output <- mapMedraToSnomedViaVocabulary(
    connectionDetails = connectionDetails,
    conceptIds = 0,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getCountOfSourceCodesMappedToStandardConcept 2 ----
testthat::test_that("Source codes Mapped to Standard Concept - connectionDetails", {
  output <-
    getCountOfSourceCodesMappedToStandardConcept(
      connectionDetails = connectionDetails,
      conceptIds = c(19025280, 19077577),
      cdmDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})

# getConceptRecordCount 2 ----
testthat::test_that("Source codes Mapped to Standard Concept - connectionDetails", {
  output <-
    getConceptRecordCount(
      connectionDetails = connectionDetails,
      conceptIds = c(19025280, 19077577),
      cdmDatabaseSchema = cdmDatabaseSchema
    )
  testthat::expect_gte(object = nrow(output), expected = 0)
})
