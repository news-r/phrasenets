#' @export
print.phrasenet <- function(x, ...) {
  msg <- paste(
    "Phrase net of", 
    crayon::blue(prettyNum(nrow(x), big.mark = ",")), 
    "connections.\n"
  )
  cat(msg, ...)
}