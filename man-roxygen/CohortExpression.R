#' @param cohortExpression  A R-object (list) that represents cohort definition expression. This is derived from cohort expression json 
#'                          using RJSONIO::fromJSON(content = json, digits = 23). Note: it is important to use digits = 23, otherwise
#'                          numerical precision may be lost for large integer values like conceptId's in cohort definition. The cohort
#'                          expression JSON is commonly generated using OHDSI tools like Atlas or CapR.
