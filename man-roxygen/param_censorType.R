#' @param censorType (`character(1)`)\cr
#' \describe{
#'   \item{`"minCellCount"`}{Censors pathways <`minCellCount` to `minCellCount`.}
#'   \item{`"remove"`}{Censors pathways <`minCellCount` by removing them completely.}
#'   \item{`"mean"`}{Censors pathways <`minCellCount` to the mean of all frequencies below `minCellCount`}
#' }
