# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of ConceptSetDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' Given a concept set expression, instantiate a cohort.
#'
#' @description
#' Given a concept set expression, instantiate a cohort. The cohort is generated
#' by checking for the occurrence of any of the resolved conceptId's in the conceptSetExpression
#' in any domain tables. The domain tables are condition_occurrence, drug_exposure, procedure_occurrence,
#' visit_occurrence, observation.
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @param cohortId An integer value to identify the cohort.
#'
#' @param cohortTable the name of the cohort table
#' 
#' @param restrictToObservationPeriod (Default = TRUE) Do you want to restrict to Observation period? i.e
#'                                      Cohort dates are restricted to observation period.
#'
#' @param conceptSetExpression An R object (list) representation of a concept set expression
#'
#' @return
#' NULL
#'
#' @export
instantiateCohortFromConceptSetExpression <-
  function(connectionDetails = NULL,
           connection = NULL,
           cdmDatabaseSchema,
           vocabularyDatabaseSchema = cdmDatabaseSchema,
           cohortDatabaseSchema = cohortDatabaseSchema,
           cohortId,
           cohortTable = "cohort",
           restrictToObservationPeriod = TRUE,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptSetExpression) {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    conceptIds <-
      ConceptSetDiagnostics::resolveConceptSetExpression(
        conceptSetExpression = conceptSetExpression,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      ) %>%
      dplyr::select(conceptId) %>%
      dplyr::distinct() %>%
      dplyr::arrange(conceptId) %>%
      dplyr::pull(conceptId)
    
    tempConceptTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )
    
    sql <- SqlRender::loadRenderTranslateSql(
      "InstantiateCohortFromConceptSetExpression.sql",
      packageName = utils::packageName(),
      dbms = connection@dbms,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      concept_id_table = tempConceptTableName, 
      cohort_id = cohortId,
      restrict_to_observation_period = restrictToObservationPeriod
    )
    ParallelLogger::logInfo(" Looking for resolved concepts in domain tables.")
    DatabaseConnector::executeSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = TRUE,
      reportOverallTime = FALSE
    )
    
    ParallelLogger::logInfo(" Creating cohort table.")
    CohortAlgebra::unionCohorts(
      connection = connection,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTable,
      targetCohortTable = cohortTable,
      oldToNewCohortId = dplyr::tibble(oldCohortId = cohortId, newCohortId = cohortId),
      purgeConflicts = TRUE
    )
  }
