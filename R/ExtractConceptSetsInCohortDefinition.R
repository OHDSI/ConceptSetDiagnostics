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

#' Extract concept set expressions from cohort definition expression.
#'
#' @description
#' Given a cohort expression, this function extracts the concept set
#' expressions from cohort definition expression.
#'
#' @template CohortExpression
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
extractConceptSetsInCohortDefinition <-
  function(cohortExpression) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }

    # extract concept set expression from cohort expression
    conceptSetExpression <-
      extractConceptSetExpressionsFromCohortExpression(cohortExpression = expression)

    if (is.null(conceptSetExpression)) {
      stop("No concept set expressions found in cohort expression")
    }

    # use circe to render cohort sql and extract concept set sql
    circeRenderedSqlExpression <-
      getCohortSqlFromCohortDefinition(
        cohortExpression = expression,
        generateStats = TRUE
      )

    extractedConceptSetSql <-
      extractConceptSetsSqlFromCohortSql(cohortSql = circeRenderedSqlExpression)

    primaryCriterias <-
      expression$PrimaryCriteria$CriteriaList
    codeSetsIdsInPrimaryCriteria <- c()
    for (i in (1:length(primaryCriterias))) {
      codesets <- primaryCriterias[[i]][[1]]

      if (typeof(codesets) == "list") {
        if (!is.null(codesets$CodesetId)) {
          codeSetsIdsInPrimaryCriteria <- c(
            codeSetsIdsInPrimaryCriteria,
            codesets$CodesetId
          ) %>%
            unique() %>%
            sort()
        }
      } else {
        if (names(codesets) == "CodesetId") {
          codeSetsIdsInPrimaryCriteria <- c(
            codeSetsIdsInPrimaryCriteria,
            as.double(codesets)
          ) %>%
            unique() %>%
            sort()
        }
      }
    }

    conceptSetExpression2 <- list()
    for (j in (1:nrow(conceptSetExpression))) {
      conceptSetExpression2[[j]] <- conceptSetExpression[j, ]
      conceptSetExpression2[[j]]$conceptSetExpressionSignature <-
        convertConceptSetExpressionToDataFrame(
          conceptSetExpression = conceptSetExpression2[[j]][1, ]$conceptSetExpression %>%
            RJSONIO::fromJSON(digits = 23)
        ) %>%
        dplyr::select(
          .data$conceptId,
          .data$includeDescendants,
          .data$includeMapped,
          .data$isExcluded
        ) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$conceptId) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }

    conceptSetExpression <-
      dplyr::bind_rows(conceptSetExpression2) %>% 
      dplyr::mutate(conceptSetUsedInEntryEvent = 0)
    
    if (length(codeSetsIdsInPrimaryCriteria) > 0) {
      conceptSetExpression <- conceptSetExpression %>% 
        dplyr::left_join(
          dplyr::tibble(conceptSetId = codeSetsIdsInPrimaryCriteria) %>%
            dplyr::distinct() %>%
            dplyr::mutate(conceptSetUsedInEntryEvent = 1),
          by = "conceptSetId"
        )
    }

    uniqueConceptSets <- conceptSetExpression %>%
      dplyr::select(.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())

    conceptSetExpression <- conceptSetExpression %>%
      dplyr::left_join(uniqueConceptSets,
        by = "conceptSetExpressionSignature"
      ) %>%
      dplyr::select(-.data$conceptSetExpressionSignature)

    data <- dplyr::inner_join(
      x = conceptSetExpression,
      y = extractedConceptSetSql,
      by = c("conceptSetId")
    )
    return(data)
  }


extractConceptSetExpressionsFromCohortExpression <-
  function(cohortExpression) {
    conceptSetExpression <- list()
    if (length(cohortExpression$ConceptSets) > 0) {
      for (i in (1:length(cohortExpression$ConceptSets))) {
        conceptSetExpression[[i]] <-
          tidyr::tibble(
            conceptSetId = cohortExpression$ConceptSets[[i]]$id,
            conceptSetName = cohortExpression$ConceptSets[[i]]$name,
            conceptSetExpression = cohortExpression$ConceptSets[[i]]$expression$items %>% RJSONIO::toJSON(digits = 23)
          )
      }
    } else {
      warning("There are no concept sets in the given cohort expression.")
      return(NULL)
    }
    return(dplyr::bind_rows(conceptSetExpression))
  }



extractConceptSetsSqlFromCohortSql <- function(cohortSql) {
  sql <- gsub("with primary_events.*", "", cohortSql)

  # Find opening and closing parentheses:
  starts <- stringr::str_locate_all(sql, "\\(")[[1]][, 1]
  ends <- stringr::str_locate_all(sql, "\\)")[[1]][, 1]

  x <- rep(0, nchar(sql))
  x[starts] <- 1
  x[ends] <- -1
  level <- cumsum(x)
  level0 <- which(level == 0)

  subQueryLocations <-
    stringr::str_locate_all(sql, "SELECT [0-9]+ as codeset_id")[[1]]
  subQueryCount <- nrow(subQueryLocations)
  conceptsetSqls <- vector("character", subQueryCount)
  conceptSetIds <- vector("integer", subQueryCount)

  temp <- list()
  for (i in 1:subQueryCount) {
    startForSubQuery <- min(starts[starts > subQueryLocations[i, 2]])
    endForSubQuery <- min(level0[level0 > startForSubQuery])
    subQuery <-
      paste(
        stringr::str_sub(sql, subQueryLocations[i, 1], endForSubQuery),
        "C"
      )
    conceptsetSqls[i] <- subQuery
    conceptSetIds[i] <- stringr::str_replace(
      subQuery,
      pattern = stringr::regex(
        pattern = "SELECT ([0-9]+) as codeset_id.*",
        ignore_case = TRUE,
        multiline = TRUE,
        dotall = TRUE
      ),
      replacement = "\\1"
    ) %>%
      utils::type.convert(as.is = TRUE)
    temp[[i]] <- tidyr::tibble(
      conceptSetId = conceptSetIds[i],
      conceptSetSql = conceptsetSqls[i]
    )
  }
  return(dplyr::bind_rows(temp))
}



getCohortSqlFromCohortDefinition <-
  function(cohortExpression,
           generateStats = TRUE) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }

    # use circe to render cohort sql
    circeRCohortExpressionFromJson <-
      CirceR::cohortExpressionFromJson(expressionJson = RJSONIO::toJSON(
        x = expression,
        digits = 23
      ))
    circeRenderedSqlExpression <-
      CirceR::buildCohortQuery(
        expression = circeRCohortExpressionFromJson,
        options = CirceR::createGenerateOptions(generateStats = generateStats)
      )
    return(circeRenderedSqlExpression)
  }
