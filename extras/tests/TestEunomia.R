library(magrittr)
databaseSchema <- "main"
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# DatabaseConnector::insertTable()
connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)
# dbReadTable(connection,CONCEPT)

cohortDefinitionSet <-
  CohortGenerator::getCohortDefinitionSet(
    settingsFileName = "cohorts/CohortsToCreate.csv",
    jsonFolder = "cohorts",
    sqlFolder = "cohorts",
    packageName = "ConceptSetDiagnostics"
  ) %>%
  dplyr::tibble()


#-------------------------------------------------------------Pre-Requisites ----------------------------
# Get conceptIds to filter from concept_prevalence scheme (Filter Eunomia conceptIds fro Main Database)
concepts <-
  DatabaseConnector::renderTranslateQuerySql(
    conn = connection,
    sql = "select CONCEPT_ID,
                  concept_name,
                  vocabulary_id,
                  domain_id,
                  standard_concept
            from CONCEPT",
    snakeCaseToCamelCase = TRUE
  ) %>%
  dplyr::tibble()

concepts$rc <-
  sample(
    x = 1:nrow(concepts) * 2,
    size = nrow(concepts),
    replace = TRUE
  )
concepts$drc <- concepts$rc * 2
concepts$dbc <-
  sample(x = 1:2,
         size = nrow(concepts),
         replace = TRUE)
concepts$ddbc <- concepts$dbc * 2

DatabaseConnector::insertTable(
  connection = connection,
  tableName = "universe",
  data = concepts,
  camelCaseToSnakeCase = TRUE,
  dropTableIfExists = TRUE,
  createTable = TRUE
)



#----1. getConceptAncestor----
conceptAncestor <- ConceptSetDiagnostics::getConceptAncestor(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)
conceptAncestor

#----2. getConceptDescendant----
conceptDescendants <- ConceptSetDiagnostics::getConceptDescendant(
  conceptIds = conceptAncestor %>%
    dplyr::arrange(dplyr::desc(.data$maxLevelsOfSeparation)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$ancestorConceptId),
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)
conceptDescendants

#----3. getConceptIdDetails----
allConceptIds <- ConceptSetDiagnostics::getConceptIdDetails(
  conceptIds = c(
    conceptDescendants$ancestorConceptId,
    conceptDescendants$descendantConceptId,
    conceptAncestor$ancestorConceptId,
    conceptAncestor$descendantConceptId
  ) %>% unique(),
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
) %>%
  dplyr::arrange(.data$conceptId)
allConceptIds

#----4. getConceptPrevalenceCountsForConceptIds----
conceptPrevalenceCount <-
  ConceptSetDiagnostics::getConceptPrevalenceCounts(
    conceptIds = allConceptIds$conceptId,
    connection = connection,
    conceptPrevalenceTable = "main.universe"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$drc))
conceptPrevalenceCount


#----5. getConceptRelationship----
conceptIdforRelationship <- 40162359
ConceptSetDiagnostics::getConceptRelationship(
  conceptIds = conceptIdforRelationship,
  connectionDetails = connectionDetails,
  vocabularyDatabaseSchema = databaseSchema
)

#----6. getConceptSynonym----
ConceptSetDiagnostics::getConceptSynonym(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----7. getDomain----
ConceptSetDiagnostics::getDomain(connection = connection,
                                 vocabularyDatabaseSchema = databaseSchema)

#----8. getDrugIngredients
ConceptSetDiagnostics::getDrugIngredients(
  connection = connection,
  conceptIds = c(1127078, 1127433),
  vocabularyDatabaseSchema = databaseSchema
)

#----9. getExcludedConceptsInConceptSetExpression
conceptSetExpression <-
  dplyr::bind_rows(
    dplyr::tibble(conceptId = 4274025,
                  includeDescendants = TRUE),
    dplyr::tibble(
      conceptId = 4101796,
      includeDescendants = TRUE,
      isExcluded = TRUE
    )
  ) %>%
  convertConceptSetDataFrameToExpression()
excludedConcepts <- getExcludedConceptsInConceptSetExpression(
  conceptSetExpression = conceptSetExpression,
  connectionDetails = connectionDetails,
  vocabularyDatabaseSchema = "main"
)

#----10. getMappedSourceConcepts----
ConceptSetDiagnostics::getMappedSourceConcepts(
  conceptIds = 192671,
  connectionDetails = connectionDetails,
  vocabularyDatabaseSchema = databaseSchema
)

#----11.getMappedStandardConcepts----
ConceptSetDiagnostics::getMappedStandardConcepts(
  conceptIds = 35208414,
  connectionDetails = connectionDetails,
  vocabularyDatabaseSchema = databaseSchema
)

#----12.getMedraRelationship----

#----13. getRelationship----
ConceptSetDiagnostics::getRelationship(connection = connection, vocabularyDatabaseSchema = databaseSchema)

#----14. performStringSearchForConcepts----
searchResultDataFrame <-
  ConceptSetDiagnostics::performStringSearchForConcepts(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = databaseSchema,
    searchString =  searchKeyword
  )

searchResultDataFrame$includeDescendants <- TRUE
searchResultDataFrame$includeMapped <- TRUE
searchResultDataFrame$isExcluded <- FALSE

#----15. performStringSearchForConceptsUsingTsv----

#----16. getVocabulary----
getVocabulary(connectionDetails = connectionDetails,
              vocabularyDatabaseSchema = databaseSchema)

#----17. resolveConceptSetExpression----


#----18. resolveConceptSetsInCohortExpression----

#----19. convertConceptSetDataFrameToExpression----
conceptSetExpression <-
  ConceptSetDiagnostics::convertConceptSetDataFrameToExpression(conceptSetExpressionDataFrame = searchResultDataFrame)


#----20. convertConceptSetExpressionToDataFrame----
conceptSetDataFrame <-
  ConceptSetDiagnostics::convertConceptSetExpressionToDataFrame(
    connectionDetails = connectionDetails,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = databaseSchema,
    updateVocabularyFields = TRUE
  )

#----21. resolveConceptSetExpression----
resolvedConceptsIds <-
  ConceptSetDiagnostics::resolveConceptSetExpression(
    conceptSetExpression = conceptSetExpression,
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = databaseSchema
  )

#----22. ResolveConceptSetsInCohortExpression----


#----21. convertConceptSetDataFrameToExpression----


#----22. extractConceptSetsInCohortDefinition----
ConceptSetDiagnostics::extractConceptSetsInCohortDefinition(cohortExpression = cohortDefinitionSet[1, ]$json %>%
                                                              RJSONIO::fromJSON(digits = 23))

#----23. extractConceptSetsInCohortDefinitionSet----


#----24. optimizeConceptSetExpression
ConceptSetDiagnostics::optimizeConceptSetExpression(
  conceptSetExpression = conceptSetExpression,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)
