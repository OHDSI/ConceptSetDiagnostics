library(magrittr)
conceptId <- 381316
searchKeyword <- "Diabetes"
databaseSchema <- "main"
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# DatabaseConnector::insertTable()
connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)
# dbReadTable(connection,CONCEPT)


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
ConceptSetDiagnostics::getConceptAncestor(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----2. getConceptIdDetails----
ConceptSetDiagnostics::getConceptIdDetails(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----3. getConceptPrevalenceCountsForConceptIds----
ConceptSetDiagnostics::getConceptPrevalenceCounts(
  conceptIds = conceptId,
  connection = connection,
  conceptPrevalenceTable = "main.universe"
)

#----4. getConceptRelationship----
conceptIdforRelationship <- 40162359
ConceptSetDiagnostics::getConceptRelationship(
  conceptIds = conceptIdforRelationship,
  connectionDetails = connectionDetails,
  vocabularyDatabaseSchema = databaseSchema
)


#----5. getStringSearchConcepts----
searchResultDataFrame <-
  ConceptSetDiagnostics::getStringSearchConcepts(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = databaseSchema,
    searchString =  searchKeyword
  )


searchResultDataFrame$includeDescendants <- TRUE
searchResultDataFrame$includeMapped <- TRUE
searchResultDataFrame$isExcluded <- FALSE

#----6. getConceptSetExpressionFromConceptSetExpressionDataFrame----
conceptSetExpression <-
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = searchResultDataFrame)


#----7. getConceptSetExpressionDataFrameFromConceptSetExpression----
conceptSetDataFrame <-
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
    connection = connection,
    conceptSetExpression = conceptSetExpression,
    vocabularyDatabaseSchema = databaseSchema, 
    updateVocabularyFields = TRUE
  )

#----8. resolvedConceptsIds----
resolvedConceptsIds <-
  ConceptSetDiagnostics::resolveConceptSetExpression(
    conceptSetExpression = conceptSetExpression,
    connection = connection,
    vocabularyDatabaseSchema = databaseSchema
  )

#----9. getConceptSynonym----
ConceptSetDiagnostics::getConceptSynonym(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)


#----10. getDomain----
ConceptSetDiagnostics::getDomain(connection = connection,
                                 vocabularyDatabaseSchema = databaseSchema)

#----11. getMappedSourceConcepts----
ConceptSetDiagnostics::getMappedSourceConcepts(
  conceptIds = 192671,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----12.getMappedStandardConcepts----
ConceptSetDiagnostics::getMappedStandardConcepts(
  conceptIds = 35208414,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----13. getRelationship----
ConceptSetDiagnostics::getRelationship(connection = connection, vocabularyDatabaseSchema = databaseSchema)

#----14. getVocabulary----
ConceptSetDiagnostics::getVocabulary(connection = connection, vocabularyDatabaseSchema = databaseSchema)









#
ConceptSetDiagnostics::extractConceptSetsInCohortDefinition()

#----13. getOptimizationRecommendationForConceptSetTable----
ConceptSetDiagnostics::getOptimizationRecommendationForConceptSetTable(
  conceptSetExpressionDataFrame = conceptSetExpressionDataFrame,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----17. getRecommendationForConceptSetExpression---- (Includes getRecommendedStandard and GetRecommendedSource)
ConceptSetDiagnostics::getRecommendationForConceptSetExpression(
  conceptSetExpression = conceptSetExpression,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema,
  conceptPrevalenceSchema = databaseSchema
)


#----20. optimizeConceptSetExpression----
ConceptSetDiagnostics::optimizeConceptSetExpression(
  connection = connection,
  conceptSetExpression = conceptSetExpression,
  vocabularyDatabaseSchema = databaseSchema
)
