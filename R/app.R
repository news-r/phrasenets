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
  "app/app.R" %>% 
    system.file(package = 'phrasenets') %>% 
    shiny::shinyAppFile()
}