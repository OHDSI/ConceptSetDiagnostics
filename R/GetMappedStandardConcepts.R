# Copyright 2020 Observational Health Data Sciences and Informatics
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

# given a list of conceptIds, get their mapped
#' @export
getMappedStandardConcepts <-
  function(conceptIds,
           connection,
           vocabularyDatabaseSchema = 'vocabulary') {
    if (length(conceptIds) == 0) {
      stop('no concept id provided')
    }
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetMappedStandardcodes.sql",
        packageName = "ConceptSetDiagnostics",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptsIdsToGetMapped = conceptIds
      )
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1)
    return(data)
  }