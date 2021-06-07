library(magrittr)
conceptId <- 381316
searchKeyword <- "Diabetes"
databaseSchema <- "main"
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# DatabaseConnector::insertTable()
connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
# dbReadTable(connection,CONCEPT)


#-------------------------------------------------------------Pre-Requisites ----------------------------
# Get conceptIds to filter from concept_prevalence scheme (Filter Eunomia conceptIds fro Main Database)
conceptIdList <- DatabaseConnector::dbGetQuery(conn = connection,statement = "select CONCEPT_ID from CONCEPT") %>%
  dplyr::pull(.data$CONCEPT_ID) %>% 
  unique()

# Insert Universe Table into Eunomia (From CSV) : Read CSV -> insert table -> overwrite CSV 
ConceptPrevalenceDf <- read.csv(file = file.path(getwd(),"inst","csv","conceptPrevalenceUniverse.csv")) %>% 
  dplyr::filter(.data$concept_id %in% conceptIdList)

DatabaseConnector::insertTable(connection = connection,tableName = "UNIVERSE", data = ConceptPrevalenceDf)

write.csv(x = ConceptPrevalenceDf,file = file.path(getwd(),"inst","csv","conceptPrevalenceUniverse.csv"),row.names = FALSE)

# Insert CP_Master Table into Eunomia (From CSV) : Read CSV -> insert table -> overwrite CSV
ConceptPrevalenceDf <- read.csv(file = file.path(getwd(),"inst","csv","conceptPrevalenceCpMaster.csv")) %>%
  dplyr::filter(.data$concept_id %in% conceptIdList)

DatabaseConnector::insertTable(connection = connection,tableName = "CP_MASTER", data = ConceptPrevalenceDf)

write.csv(x = ConceptPrevalenceDf,file = file.path(getwd(),"inst","csv","conceptPrevalenceCpMaster.csv"),row.names = FALSE)

# Insert RECOMMENDED_BLACKLIST Table into Eunomia (From CSV) : Read CSV -> insert table -> overwrite CSV
ConceptPrevalenceDf <- read.csv(file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommendedBlacklist.csv")) %>%
  dplyr::filter(.data$concept_id %in% conceptIdList)

DatabaseConnector::insertTable(connection = connection,tableName = "RECOMMENDED_BLACKLIST", data = ConceptPrevalenceDf)

write.csv(x = ConceptPrevalenceDf,file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommendedBlacklist.csv"),row.names = FALSE)

# Insert RECOMMENDER_SET Table into Eunomia (From CSV) : Read CSV -> insert table -> overwrite CSV
ConceptPrevalenceDf <- read.csv(file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommenderSet.csv")) %>%
  dplyr::filter(.data$concept_id %in% conceptIdList)

DatabaseConnector::insertTable(connection = connection,tableName = "RECOMMENDER_SET", data = ConceptPrevalenceDf)

write.csv(x = ConceptPrevalenceDf,file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommenderSet.csv"),row.names = FALSE)
#-------------------------------------------------------------------------------------------------------------------


#----1. getConceptAncestor----
ConceptSetDiagnostics::getConceptAncestor(conceptIds = conceptId,
                                                  connectionDetails = connectionDetails,
                                          vocabularyDatabaseSchema = databaseSchema)

#----2. getConceptIdDetails----
ConceptSetDiagnostics::getConceptIdDetails(conceptIds = conceptId,
                                           connectionDetails = connectionDetails,
                                           vocabularyDatabaseSchema = databaseSchema,
                                           conceptPrevalenceSchema = databaseSchema)

#----3. getConceptPrevalenceCountsForConceptIds----
ConceptSetDiagnostics::getConceptPrevalenceCountsForConceptIds(
  conceptIds = conceptId,
  connectionDetails = connectionDetails,
  conceptPrevalenceSchema = "main"
)

#----4. getConceptRelationship----
conceptIdforRelationship <- 40162359
ConceptSetDiagnostics::getConceptRelationship(conceptIds = conceptIdforRelationship,
                                              connectionDetails = connectionDetails,
                                              vocabularyDatabaseSchema = databaseSchema)


#----5. getStringSearchConcepts----
# modified !is.null(connection), Error : no such table: concept_prevalence.universe
searchResultDataFrame <-
  ConceptSetDiagnostics::getStringSearchConcepts(
    connectionDetails = connectionDetails,
    vocabularyDatabaseSchema = databaseSchema,
    conceptPrevalenceSchema = databaseSchema,
    searchString =  searchKeyword
)
searchResultDataFrame$includeDescendants <- TRUE
searchResultDataFrame$includeMapped <- TRUE
searchResultDataFrame$isExcluded <- FALSE

#----6. getConceptSetExpressionFromConceptSetExpressionDataFrame---- 
conceptSetExpression <-
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = searchResultDataFrame)



#----7. getConceptSetSignatureExpression----
signatureConceptSetExpression <- ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connection,
                                                        conceptSetExpression = conceptSetExpression,
                                                        vocabularyDatabaseSchema = vocabularyDatabaseSchema)

#----8. getConceptSetExpressionDataFrameFromConceptSetExpression----
ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
  updateVocabularyFields = TRUE,
  conceptSetExpression = signatureConceptSetExpression,
  vocabularyDatabaseSchema = databaseSchema,
  recordCount = TRUE,
  connection = connection,
  conceptPrevalenceSchema = databaseSchema
)

#----9. resolvedConceptsIds----
resolvedConceptsIds <- ConceptSetDiagnostics::resolveConceptSetExpression(
  conceptSetExpression = conceptSetExpression,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema,
  conceptPrevalenceSchema = databaseSchema
)

#----10. getConceptSynonym----
ConceptSetDiagnostics::getConceptSynonym(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema
)

#----11. getDeepConceptRelationship----
ConceptSetDiagnostics::getDeepConceptRelationship(
  conceptIds = conceptId,
  connection = connection,
  vocabularyDatabaseSchema = databaseSchema,
  conceptPrevalenceSchema = databaseSchema
)

#----12. getDomain----
ConceptSetDiagnostics::getDomain(connection = connection, 
                                 vocabularyDatabaseSchema = databaseSchema)

#----13. getMappedSourceConcepts----
ConceptSetDiagnostics::getMappedSourceConcepts(conceptIds = conceptId,
                                               connection = connection,
                                               vocabularyDatabaseSchema = databaseSchema)

#----14.getMappedStandardConcepts----
ConceptSetDiagnostics::getMappedStandardConcepts(conceptIds = conceptId,
                                                 connection = connection,
                                                 vocabularyDatabaseSchema = databaseSchema)

#----15. getConceptSetExpressionDataFrameFromConceptSetExpression----
conceptSetExpressionDataFrame  <-
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression(
    conceptSetExpression = conceptSetExpression,
    connection = connection,
    vocabularyDatabaseSchema = databaseSchema
  )

#----16. getOptimizationRecommendationForConceptSetTable----
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

#----18. getRelationship----
ConceptSetDiagnostics::getRelationship(connection = connection, vocabularyDatabaseSchema = databaseSchema)

#----19. getVocabulary----
ConceptSetDiagnostics::getVocabulary(connection = connection,vocabularyDatabaseSchema = databaseSchema)




