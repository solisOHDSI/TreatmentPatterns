#' @param cohorts (`data.frame()`)\cr
#' Data frame containing the following columns and data types:
#' \describe{
#'  \item{cohortId `numeric(1)`}{Cohort ID's of the cohorts to be used in the cohort table.}
#'  \item{cohortName `character(1)`}{Cohort names of the cohorts to be used in the cohort table.}
#'  \item{type `character(1)` \["target", "event', "exit"\]}{Cohort type, describing if the cohort is a target, event, or exit cohort}
#' }
