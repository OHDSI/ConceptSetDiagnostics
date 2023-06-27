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


checkIfCohortDefinitionSet <- function(cohortDefinitionSet) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(
    x = cohortDefinitionSet,
    min.cols = 1,
    add = errorMessage
  )
  checkmate::assertNames(
    x = colnames(cohortDefinitionSet),
    must.include = c("cohortId"),
    add = errorMessage
  )
  if (errorMessage$isEmpty()) {
    return(NULL)
  } else {
    return(errorMessage)
  }
}


# private function - not exported
hasData <- function(data) {
  if (is.null(data)) {
    return(FALSE)
  }
  if (is.data.frame(data)) {
    if (nrow(data) == 0) {
      return(FALSE)
    }
  }
  if (!is.data.frame(data)) {
    if (length(data) == 0) {
      return(FALSE)
    }
    if (length(data) == 1) {
      if (is.na(data)) {
        return(FALSE)
      }
      if (data == "") {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

getUniqueString <- function(n = 7) {
  # create a vector of all alphanumeric characters
  alphanumericChars <- c(letters, 0:9)
  
  # generate the first character from the set of letters only
  firstChar <- sample(c(letters), 1)
  
  # generate the remaining characters from the set of all alphanumeric characters
  remainingChars <- sample(alphanumericChars, n, replace = TRUE)
  
  # combine the first character with the remaining characters
  uniqueString <- paste0(firstChar, paste0(remainingChars, collapse = ""))
  
  return(tolower(uniqueString))
}

loadTempConceptTable <- function(conceptIds,
                                 connection,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  conceptIdTable <-
    dplyr::tibble(conceptId = conceptIds %>% unique() %>% as.integer())

  tempTableName <- getUniqueString()

  invisible(utils::capture.output(
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = tempTableName,
      dropTableIfExists = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      data = conceptIdTable,
      camelCaseToSnakeCase = TRUE,
      bulkLoad = TRUE,
      progressBar = TRUE,
      createTable = TRUE
    ),
    file = nullfile()
  ))
  if (connection@dbms %in% c("redshift", "postgresql")) {
    # Some performance tuning:
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "ANALYZE @concept_id_table;",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      concept_id_table = tempTableName
    )
  }

  return(tempTableName)
}


dropTempConceptTable <-
  function(tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           connection,
           tempTableName) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DROP TABLE IF EXISTS @concept_id_table;",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      concept_id_table = tempTableName
    )
  }


#' Get domain information
#'
#' @param packageName e.g. 'CohortDiagnostics'
#'
#' @return
#' A list with two tibble data frame objects with domain information represented in wide and long format respectively.
getDomainInformation <- function(packageName = NULL) {
  domains <-
    readr::read_csv(
      system.file(file.path("csv", "domains.csv"),
        package = "ConceptSetDiagnostics"
      ),
      col_types = readr::cols()
    )

  domains <- domains %>%
    .replaceNaInDataFrameWithEmptyString() %>%
    dplyr::mutate(domainTableShort = stringr::str_sub(
      string = toupper(.data$domain),
      start = 1,
      end = 2
    )) %>%
    dplyr::mutate(
      domainTableShort = dplyr::case_when(
        stringr::str_detect(string = tolower(.data$domain), pattern = "era") ~ paste0(.data$domainTableShort, "E"),
        TRUE ~ .data$domainTableShort
      )
    )

  domains$domainConceptIdShort <-
    stringr::str_replace_all(
      string = sapply(
        stringr::str_extract_all(
          string = camelCaseToTitleCase(snakeCaseToCamelCase(domains$domainConceptId)),
          pattern = "[A-Z]"
        ),
        paste,
        collapse = " "
      ),
      pattern = " ",
      replacement = ""
    )
  domains$domainSourceConceptIdShort <-
    stringr::str_replace_all(
      string = sapply(
        stringr::str_extract_all(
          string = camelCaseToTitleCase(snakeCaseToCamelCase(domains$domainSourceConceptId)),
          pattern = "[A-Z]"
        ),
        paste,
        collapse = " "
      ),
      pattern = " ",
      replacement = ""
    )
  domains <- domains %>%
    dplyr::mutate(isEraTable = stringr::str_detect(
      string = .data$domainTable,
      pattern = "era"
    ))
  data <- list()
  data$wide <- domains
  data$long <- dplyr::bind_rows(
    data$wide %>%
      dplyr::select(
        .data$domainTableShort,
        .data$domainTable,
        .data$domainConceptIdShort,
        .data$domainConceptId
      ) %>%
      dplyr::rename(
        domainFieldShort = .data$domainConceptIdShort,
        domainField = .data$domainConceptId
      ),
    data$wide %>%
      dplyr::select(
        .data$domainTableShort,
        .data$domainSourceConceptIdShort,
        .data$domainTable,
        .data$domainSourceConceptId
      ) %>%
      dplyr::rename(
        domainFieldShort = .data$domainSourceConceptIdShort,
        domainField = .data$domainSourceConceptId
      )
  ) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$domainFieldShort != "") %>%
    dplyr::mutate(eraTable = stringr::str_detect(
      string = .data$domainTable,
      pattern = "era"
    )) %>%
    dplyr::mutate(isSourceField = stringr::str_detect(
      string = .data$domainField,
      pattern = "source"
    ))
  return(data)
}

.replaceNaInDataFrameWithEmptyString <- function(data) {
  # https://github.com/r-lib/tidyselect/issues/201
  data %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(
      tidyselect:::where(is.character),
      ~ tidyr::replace_na(.x, as.character(""))
    )) %>%
    dplyr::mutate(dplyr::across(
      tidyselect:::where(is.logical),
      ~ tidyr::replace_na(.x, as.character(""))
    )) %>%
    dplyr::mutate(dplyr::across(
      tidyselect:::where(is.numeric),
      ~ tidyr::replace_na(.x, as.numeric(""))
    ))
}


# private function - not exported
camelCaseToTitleCase <- function(string) {
  string <- gsub("([A-Z])", " \\1", string)
  string <- gsub("([a-z])([0-9])", "\\1 \\2", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

# private function - not exported
snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <-
      gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

# private function - not exported
camelCaseToSnakeCase <- function(string) {
  string <- gsub("([A-Z])", "_\\1", string)
  string <- tolower(string)
  string <- gsub("([a-z])([0-9])", "\\1_\\2", string)
  return(string)
}

# private function - not exported
titleCaseToCamelCase <- function(string) {
  string <- stringr::str_replace_all(
    string = string,
    pattern = " ",
    replacement = ""
  )
  substr(string, 1, 1) <- tolower(substr(string, 1, 1))
  return(string)
}

# private function - not exported
quoteLiterals <- function(x) {
  if (is.null(x)) {
    return("")
  } else {
    return(paste0("'", paste(x, collapse = "', '"), "'"))
  }
}
