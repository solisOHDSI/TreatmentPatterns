#' @param censorType (`character(1)`)\cr
#' \describe{
#'   \item{`"cellCount"`}{Censors pathways <`cellCount` to `cellCount`.}
#'   \item{`"remove"`}{Censors pathways <`cellCount` by removing them completely.}
#'   \item{`"mean"`}{Censors pathways <`cellCount` to the mean of all frequencies below `cellCount`}
#' }
