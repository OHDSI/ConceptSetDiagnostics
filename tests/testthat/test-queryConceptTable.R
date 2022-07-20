connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

# getConceptAncestor 1 ----
testthat::test_that("Get Concept Ancestor - connection", {
  output <- ConceptSetDiagnostics::getConceptAncestor(
    connection = connection,
    conceptIds = 381316,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptDescendant 1 ----
testthat::test_that("Get Concept Descendant - connection", {
  output <- ConceptSetDiagnostics::getConceptDescendant(
    conceptIds = 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptIdDetails 1 ----
testthat::test_that("Get Concept Details - connection", {
  output <- ConceptSetDiagnostics::getConceptIdDetails(
    conceptIds = 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  ) %>%
    dplyr::arrange(.data$conceptId)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptRelationship 1 ----
testthat::test_that("Get Concept Relationship - connection", {
  output <- ConceptSetDiagnostics::getConceptRelationship(
    conceptIds = c(192671, 35208414, 1118088, 35208414),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptSynonym 1 ----
testthat::test_that("Get Concept Synonym - connection", {
  output <- ConceptSetDiagnostics::getConceptSynonym(
    conceptIds = 381316,
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getMappedSourceConcepts 1 ----
testthat::test_that("Get Mapped Source Concept - connection", {
  output <- ConceptSetDiagnostics::getMappedSourceConcepts(
    conceptIds = c(35208414, 192671, 1118088, 35208414),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getMappedStandardConcepts 1 ----
testthat::test_that("Get Mapped Standard Concept - connection", {
  output <- ConceptSetDiagnostics::getMappedStandardConcepts(
    conceptIds = c(35208414, 44923712),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getDomain 1 ----
testthat::test_that("Get Domain - connection", {
  output <- ConceptSetDiagnostics::getDomain(connection = connection,
                                             vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getRelationship 1 ----
testthat::test_that("Get Relationship - connection", {
  output <-
    ConceptSetDiagnostics::getRelationship(connection = connection,
                                           vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getVocabulary 1 ----
testthat::test_that("Get Vocabulary - connection", {
  output <-
    ConceptSetDiagnostics::getVocabulary(connection = connection,
                                         vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getVocabularyVersion 1 ----
testthat::test_that("Get Vocabulary Version - connection", {
  output <-
    ConceptSetDiagnostics::getVocabularyVersion(connection = connection,
                                                vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getDrugIngredients 1 ----
testthat::test_that("Get Drug Ingredients - connection", {
  output <- ConceptSetDiagnostics::getDrugIngredients(
    connection = connection,
    conceptIds = c(1127078, 1127433),
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# Disconnection ----
DatabaseConnector::disconnect(connection = connection)








# getConceptAncestor 2 ----
testthat::test_that("Get Concept Ancestor - connectionDetailsDetails", {
  output <- ConceptSetDiagnostics::getConceptAncestor(
    connectionDetailsDetails = connectionDetailsDetails,
    conceptIds = 381316,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptDescendant 2 ----
testthat::test_that("Get Concept Descendant - connectionDetails", {
  output <- ConceptSetDiagnostics::getConceptDescendant(
    conceptIds = 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptIdDetails 2 ----
testthat::test_that("Get Concept Details - connectionDetails", {
  output <- ConceptSetDiagnostics::getConceptIdDetails(
    conceptIds = 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  ) %>%
    dplyr::arrange(.data$conceptId)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptRelationship 2 ----
testthat::test_that("Get Concept Relationship - connectionDetails", {
  output <- ConceptSetDiagnostics::getConceptRelationship(
    conceptIds = c(192671, 35208414, 1118088, 35208414),
    connection = connection,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getConceptSynonym 2 ----
testthat::test_that("Get Concept Synonym - connectionDetails", {
  output <- ConceptSetDiagnostics::getConceptSynonym(
    conceptIds = 381316,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getMappedSourceConcepts 2 ----
testthat::test_that("Get Mapped Source Concept - connectionDetails", {
  output <- ConceptSetDiagnostics::getMappedSourceConcepts(
    conceptIds = c(35208414, 192671, 1118088, 35208414),
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getMappedStandardConcepts 2 ----
testthat::test_that("Get Mapped Standard Concept - connectionDetails", {
  output <- ConceptSetDiagnostics::getMappedStandardConcepts(
    conceptIds = c(35208414, 44923712),
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getDomain 2 ----
testthat::test_that("Get Domain - connectionDetails", {
  output <-
    ConceptSetDiagnostics::getDomain(connectionDetails = connectionDetails,
                                     vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getRelationship 2 ----
testthat::test_that("Get Relationship - connectionDetails", {
  output <-
    ConceptSetDiagnostics::getRelationship(connectionDetails = connectionDetails,
                                           vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getVocabulary 2 ----
testthat::test_that("Get Vocabulary - connectionDetails", {
  output <-
    ConceptSetDiagnostics::getVocabulary(connectionDetails = connectionDetails,
                                         vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getVocabularyVersion 2 ----
testthat::test_that("Get Vocabulary Version - connectionDetails", {
  output <-
    ConceptSetDiagnostics::getVocabularyVersion(connectionDetails = connectionDetails,
                                                vocabularyDatabaseSchema = cdmDatabaseSchema)
  testthat::expect_gte(object = nrow(output), expected = 1)
})

# getDrugIngredients 2 ----
testthat::test_that("Get Drug Ingredients - connectionDetails", {
  output <- ConceptSetDiagnostics::getDrugIngredients(
    connectionDetails = connectionDetails,
    conceptIds = c(1127078, 1127433),
    vocabularyDatabaseSchema = cdmDatabaseSchema
  )
  testthat::expect_gte(object = nrow(output), expected = 1)
})