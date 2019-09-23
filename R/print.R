#' @export
print.phrasenet <- function(x, ...) {
  msg <- paste("Phrase net of", nrow(x), "connections")
  print(msg, ...)
}