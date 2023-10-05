#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import stringr
#' @import utils
#' @import SqlRender
#' @import dplyr
#' @import Andromeda
#' @import R6
#' @import sunburstR
#' @importFrom stats sd median
#' @importFrom tidyr pivot_wider
#' @importFrom googleVis gvisSankey
#' @importFrom rjson toJSON
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom DatabaseConnector connect disconnect querySql querySqlToAndromeda
## usethis namespace: end
NULL

# Global Variables
utils::globalVariables(c("x", "y", "rowNumber", "path"))
