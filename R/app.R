#' Phrasenet App
#'
#' A shiny app to build phrasenets
#'
#' @examples
#' if (interactive())
#'   phrasenets_app()
#' 
#' @export
phrasenets_app <- function() {
  system.file("app/app.R", package = 'phrasenets') %>% 
    shiny::shinyAppFile()
}