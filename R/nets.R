#' Phrase Net
#' 
#' Create phrase network.
#' 
#' @import purr
#' 
#' @export
phrase_net <- function(text, id = NULL) UseMethod("phrase_net")

#' @pexport
#' @method phrase_net default
phrase_net.default <- function(text, id = NULL){
  if(!is.null(id))
    text$id <- id
  else
    text <- purrr::map2(
      text, 
      1:length(text), 
      function(x, y){
        x$id <- y
        return(x)
      }
    )
}

#' @pexport
#' @method phrase_net data.frame
phrase_net.data.frame <- function(text, id = NULL){
  text <- apply(text, 1, as.list)
  phrase_net(text)
}