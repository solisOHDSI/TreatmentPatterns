#' exportFigure
#' 
#' Save an SVG-image from a html-file as a PNG, - or PDF-file.
#'
#' @param outputPath HTML-Filename.
#' @param fileNameOut Filename of image, either with .pdf or .png extension.
#' @param zoom Zoom factor, default = 3.
#' @param vwidth Width of frame to capture, default = 430.
#' @param selector Default: `"svg"`; svg only works if the output file is pdf, otherwise use `NULL`
#' @param ... Other potential parameters to be passed to webshot2::webshot()
#'
#' @import checkmate
#' @import webshot2
#'
#' @return Invisibly returns the normalized path to all screenshots taken. The character vector will have a class of '"webshot"'.
#' @export
#'
#' @examples
#' \dontrun{
#'   exportFigure("webFile.html", "outputFile.png")}
exportFigure <- function(filePath, outputPath, zoom = 3, vwidth = 430, selector = "svg", ...) {
  # Assertions
  checkmate::assertCharacter(x = filePath, pattern = "*.html", null.ok = FALSE)
  checkmate::assertCharacter(x = outputPath, pattern = "*.pdf|*.png", null.ok = FALSE)
  checkmate::assertNumeric(x = zoom, lower = 0, len = 1, null.ok = FALSE)
  checkmate::assertNumeric(x = vwidth, lower = 0, len = 1, null.ok = FALSE)
  
  webshot2::webshot(
    url = filePath,
    file = outputPath,
    zoom = zoom,
    vwidth = vwidth,
    ...)
}