#' @details 
#' The \code{cohortDefinitionSet} argument must be a data frame with at least the following columns.
#' \describe{
#' \item{cohortId}{The cohort Id is the id used to identify  a cohort definition. This is required to be unique. It is usually used to create file names.}
#' \item{cohortName}{The full name of the cohort.}
#' \item{json}{The JSON cohort definition for the cohort.}
#' \item{sql}{The SQL of the cohort definition rendered from the cohort json.}
#' }
