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
          ) |>
            unique() |>
            sort()
        }
      } else {
        if (names(codesets) == "CodesetId") {
          codeSetsIdsInPrimaryCriteria <- c(
            codeSetsIdsInPrimaryCriteria,
            as.double(codesets)
          ) |>
            unique() |>
            sort()
        }
      }
    }

    conceptSetExpression2 <- list()
    conceptSetExpressionMetaData <- list()
    
    for (j in (1:nrow(conceptSetExpression))) {
      conceptSetExpression2[[j]] <- conceptSetExpression[j, ]
      
      conceptSetDataFrame <-
        convertConceptSetExpressionToDataFrame(conceptSetExpression =
                                                 conceptSetExpression2[[j]][1,]$conceptSetExpression |>
                                                 RJSONIO::fromJSON(digits = 23))
      conceptSetExpressionMetaData[[j]] <- conceptSetExpression2[[j]][1, ] |>
        dplyr::select(conceptSetId) |> 
        dplyr::mutate(
          hasStandard = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(string = standardConcept,
                                                pattern = "S")) |>
              nrow() > 0
          ),
          hasNonStandard = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(
                stringr::str_detect(
                  string = standardConcept,
                  pattern = "S",
                  negate = TRUE
                )
              ) |>
              nrow() > 0
          ),
          hasValid = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(string = invalidReason,
                                                pattern = "V")) |>
              nrow() > 0
          ),
          hasInvalid = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(
                stringr::str_detect(
                  string = invalidReason,
                  pattern = "V",
                  negate = TRUE
                )
              ) |>
              nrow() > 0
          ),
          hasCondition = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "condition"
              )) |>
              nrow() > 0
          ),
          hasProcedure = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "procedure"
              )) |>
              nrow() > 0
          ),
          hasDevice = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "device"
              )) |>
              nrow() > 0
          ),
          hasDrug = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "drug"
              )) |>
              nrow() > 0
          ),
          hasObservation = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "observation"
              )) |>
              nrow() > 0
          ),
          hasVisit = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "visit"
              )) |>
              nrow() > 0
          ),
          hasType = as.integer(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(
                string = tolower(domainId),
                pattern = "type"
              )) |>
              nrow() > 0
          ),
          isSelectedIncludeMapped = max(as.integer(conceptSetDataFrame$includeMapped)),
          isSelectedIncludeDescendants = max(as.integer(conceptSetDataFrame$includeDescendants)),
          isSelectedIsExcluded = max(as.integer(conceptSetDataFrame$isExcluded)),
          isNotSelectedIncludeMapped = min(as.integer(conceptSetDataFrame$includeMapped)),
          isNotSelectedIncludeDescendants = min(as.integer(conceptSetDataFrame$includeDescendants)),
          isNotSelectedIsExcluded = min(as.integer(conceptSetDataFrame$isExcluded)),
          rowsInConceptSetExpression = nrow(conceptSetDataFrame),
          numberOfUniqueConceptIds = length(conceptSetDataFrame$conceptId |> unique()),
          numberOfUniqueConceptIdsWithoutDescendants = length(
            conceptSetDataFrame |>
              dplyr::filter(includeDescendants == FALSE) |>
              dplyr::pull(conceptId) |>
              unique()
          ),
          numberOfUniqueConceptIdsWitDescendants = length(
            conceptSetDataFrame |>
              dplyr::filter(includeDescendants == TRUE) |>
              dplyr::pull(conceptId) |>
              unique()
          ),
          numberOfUniqueConceptIdIsStandard = length(
            conceptSetDataFrame |>
              dplyr::filter(stringr::str_detect(string = standardConcept,
                                                pattern = "S")) |>
              dplyr::pull(conceptId) |>
              unique()
          ),
          numberOfUniqueConceptIdIsNonStandard = length(
            conceptSetDataFrame |>
              dplyr::filter(
                stringr::str_detect(
                  string = standardConcept,
                  pattern = "S",
                  negate = TRUE
                )
              ) |>
              dplyr::pull(conceptId) |>
              unique()
          )
        )
      
      conceptSetExpression2[[j]]$conceptSetExpressionSignature <-
        conceptSetDataFrame |>
        dplyr::select(.data$conceptId,
                      .data$includeDescendants,
                      .data$includeMapped,
                      .data$isExcluded) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$conceptId) |>
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }

    conceptSetExpressionMetaData <- 
      dplyr::bind_rows(conceptSetExpressionMetaData)
    
    conceptSetExpression <-
      dplyr::bind_rows(conceptSetExpression2) |> 
      dplyr::mutate(conceptSetUsedInEntryEvent = 0)
    
    if (length(codeSetsIdsInPrimaryCriteria) > 0) {
      conceptSetExpression <- conceptSetExpression |> 
        dplyr::select(-conceptSetUsedInEntryEvent) |> 
        dplyr::left_join(
          dplyr::tibble(conceptSetId = codeSetsIdsInPrimaryCriteria) |>
            dplyr::distinct() |>
            dplyr::mutate(conceptSetUsedInEntryEvent = 1),
          by = "conceptSetId"
        )
    }

    uniqueConceptSets <- conceptSetExpression |>
      dplyr::select(.data$conceptSetExpressionSignature) |>
      dplyr::distinct() |>
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())

    conceptSetExpression <- conceptSetExpression |>
      dplyr::left_join(uniqueConceptSets,
        by = "conceptSetExpressionSignature"
      ) |>
      dplyr::select(-.data$conceptSetExpressionSignature)

    data <- dplyr::inner_join(
      x = conceptSetExpression,
      y = extractedConceptSetSql,
      by = c("conceptSetId")
    )
    
    data <- data |> 
      tidyr::replace_na(replace = list(conceptSetUsedInEntryEvent = 0))
    
    data <- data |> 
      dplyr::left_join(conceptSetExpressionMetaData,
                       by = "conceptSetId")
    
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
            conceptSetExpression = cohortExpression$ConceptSets[[i]]$expression$items |> RJSONIO::toJSON(digits = 23)
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
    ) |>
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
