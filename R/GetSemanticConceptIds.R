#' Retrieve Concept IDs for Semantic Reasoning
#'
#' This function retrieves concept IDs from a given concept set definition and processes
#' them for semantic reasoning. It performs several operations:
#' \itemize{
#'   \item Resolves the concept set definition.
#'   \item Retrieves mapped source concepts.
#'   \item Retrieves concept synonyms and ancestors.
#'   \item Filters and appends recommended concepts.
#'   \item Retrieves detailed information for all collected concept IDs.
#'   \item Merges the resolved, mapped, and recommended concepts with their ancestor and
#'         synonym details.
#' }
#'
#' @param baseUrl Character. The base URL for the web API.
#' @param conceptSetDefinition A concept set definition object used by ConceptSetDiagnostics.
#' @param connection Optional. A pre-established database connection. If not provided,
#'   a connection will be created using \code{connectionDetails}.
#' @param connectionDetails List. Connection details for establishing a connection via
#'   \code{DatabaseConnector::connect}.
#' @param vocabularyDatabaseSchema Character. The name of the vocabulary database schema.
#' @param conceptRecommender Data frame. A data frame containing recommended concepts
#'   used to filter additional concepts.
#'
#' @template Connection
#' @template VocabularyDatabaseSchema
#' @template TempEmulationSchema
#'
#' @return A list containing:
#' \describe{
#'   \item{resolvedMappedRecommended}{A data frame with the resolved, mapped, and recommended
#'   concepts along with associated concept names, ancestor, and synonym information.}
#'   \item{conceptIdDetails}{A data frame of detailed information for all collected concept IDs.}
#'   \item{recommendedConcepts}{A data frame of recommended concepts filtered based on the
#'   resolved concepts.}
#' }
#'
#' @details
#' The function first establishes a database connection (if not provided) using the given
#' connection details. It then uses functions from the \code{ConceptSetDiagnostics} package to:
#' \enumerate{
#'   \item Resolve the provided concept set.
#'   \item Retrieve mapped source concepts.
#'   \item Retrieve synonyms and ancestors for each resolved concept.
#' }
#' The unique set of concept IDs is then used to filter recommended concepts and obtain
#' detailed concept information. Finally, the function merges the retrieved datasets and returns
#' a list of outputs for downstream semantic reasoning.
#'
#' @export
getSemanticConceptIds <- function(baseUrl,
                                  conceptSetDefinition,
                                  connection = NULL,
                                  connectionDetails = NULL,
                                  vocabularyDatabaseSchema,
                                  conceptRecommender) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(
      DatabaseConnector::dropEmulatedTempTables(connection = connection, tempEmulationSchema = tempEmulationSchema)
    )
    on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  }
  
  # Resolve the concept set using ConceptSetDiagnostics
  resolvedConcepts <- ConceptSetDiagnostics::resolveConceptSetExpression(
    conceptSetExpression = conceptSetDefinition,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  ) |>
    dplyr::select(conceptId) |>
    dplyr::distinct() |>
    dplyr::arrange()
  
  # Get mapped source concepts
  mappedConceptSet <- ConceptSetDiagnostics::getMappedSourceConcepts(
    conceptIds = resolvedConcepts$conceptId,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  ) |>
    dplyr::select(givenConceptId, conceptId) |>
    dplyr::distinct() |>
    dplyr::arrange(givenConceptId, conceptId)
  
  # Get concept synonyms for each resolved concept
  conceptSynonyms <- list()
  for (i in seq_len(nrow(resolvedConcepts))) {
    conceptSynonyms[[i]] <- ConceptSetDiagnostics::getConceptSynonym(
      conceptIds = resolvedConcepts[i, "conceptId"],
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) |>
      dplyr::mutate(givenConceptId = resolvedConcepts[i, "conceptId"]) |>
      dplyr::relocate(givenConceptId) |>
      dplyr::select(givenConceptId, conceptId)
  }
  conceptSynonyms <- dplyr::bind_rows(conceptSynonyms) |>
    dplyr::distinct() |>
    dplyr::arrange(givenConceptId, conceptId)
  
  # Get concept ancestors for each resolved concept
  conceptAncestor <- list()
  for (i in seq_len(nrow(resolvedConcepts))) {
    conceptAncestor[[i]] <- ConceptSetDiagnostics::getConceptAncestor(
      conceptIds = resolvedConcepts[i, "conceptId"],
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) |>
      dplyr::mutate(givenConceptId = resolvedConcepts[i, "conceptId"]) |>
      dplyr::relocate(givenConceptId)
  }
  conceptAncestor <- dplyr::bind_rows(conceptAncestor) |>
    dplyr::distinct() |>
    dplyr::arrange(givenConceptId, ancestorConceptId)
  
  # Collect all unique conceptIds from the resolved, mapped, synonyms, and ancestors
  allConceptIds <- unique(sort(
    c(
      resolvedConcepts$conceptId,
      mappedConceptSet$conceptId,
      conceptSynonyms$conceptId,
      conceptAncestor$ancestorConceptId
    )
  ))
  
  # Filter recommended concepts based on the resolved concepts
  recommendedConcepts <- conceptRecommender |>
    dplyr::filter(conceptId1 %in% allConceptIds)
  
  # Append recommended concepts to the list of all conceptIds
  allConceptIds <- unique(sort(c(
    allConceptIds, recommendedConcepts$conceptId2
  )))
  
  # Get concept details for all collected concept IDs
  conceptIdDetails <- ConceptSetDiagnostics::getConceptIdDetails(
    conceptIds = allConceptIds,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  )
  
  # Combine resolved, mapped, and recommended concepts
  resolvedMappedRecommended <- dplyr::bind_rows(
    dplyr::tibble(conceptId = resolvedConcepts$conceptId, type = "resolvedStandard"),
    dplyr::tibble(conceptId = mappedConceptSet$conceptId, type = "resolvedSource"),
    dplyr::tibble(conceptId = mappedConceptSet$conceptId, type = "recommended")
  ) |>
    dplyr::left_join(conceptIdDetails |>
                       dplyr::select(conceptId, conceptName),
                     by = "conceptId")
  
  # Get ancestor information for each concept
  ancestorConcepts <- resolvedMappedRecommended |>
    dplyr::select(conceptId) |>
    dplyr::distinct() |>
    dplyr::inner_join(
      conceptAncestor |>
        dplyr::rename(conceptId = givenConceptId) |>
        dplyr::select(conceptId, ancestorConceptId) |>
        dplyr::inner_join(
          conceptIdDetails |>
            dplyr::select(conceptId, conceptName),
          by = c("ancestorConceptId" = "conceptId")
        ),
      by = "conceptId"
    ) |>
    dplyr::group_by(conceptId) |>
    dplyr::summarise(ancestors = paste0(conceptName, collapse = ", "))
  
  # Get synonym information for each concept
  synonyms <- resolvedMappedRecommended |>
    dplyr::select(conceptId) |>
    dplyr::distinct() |>
    dplyr::inner_join(
      conceptSynonyms |>
        dplyr::rename(synonymConceptId = conceptId) |>
        dplyr::rename(conceptId = givenConceptId) |>
        dplyr::select(conceptId, synonymConceptId) |>
        dplyr::inner_join(
          conceptIdDetails |>
            dplyr::select(conceptId, conceptName),
          by = c("synonymConceptId" = "conceptId")
        ),
      by = "conceptId"
    ) |>
    dplyr::group_by(conceptId) |>
    dplyr::summarise(synonyms = paste0(conceptName, collapse = ", ")) |>
    dplyr::distinct()
  
  # Merge ancestor and synonym data into the main dataset
  resolvedMappedRecommended <- resolvedMappedRecommended |>
    dplyr::left_join(ancestorConcepts) |>
    dplyr::left_join(synonyms)
  
  # Optionally, you could close the connection here if desired:
  # DatabaseConnector::disconnect(connection)
  
  # Return a list of outputs
  return(
    list(
      resolvedMappedRecommended = resolvedMappedRecommended,
      conceptIdDetails = conceptIdDetails,
      recommendedConcepts = recommendedConcepts
    )
  )
}
