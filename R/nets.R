#' Phrase Net
#' 
#' Create phrase network.
#' 
#' @param data A data.frame or \link[tibble]{tibble} containig \code{text}.
#' @param connectors Words that establish connections between words and 
#' ultimately form edges.
#' @param ... Arguments to pass to method, namely \code{text}.
#' 
#' @examples
#' data(reuters)
#' net1 <- phrase_net(reuters, text = text)
#' net2 <- phrase_net(reuters$text)
#' 
#' identical(net1, net2)
#' 
#' @import dplyr
#' @import assertthat
#' 
#' @export
phrase_net <- function(data, connectors = c("to", "in", "at", "and", "of"), ...) UseMethod("phrase_net")

#' @export
phrase_net.default <- function(data, connectors = c("to", "in", "at", "and", "of"), ...){

  data <- tibble::tibble(text = data)

  data %>% 
    tidytext::unnest_tokens(word, text) %>% 
    mutate(
      is_connector = word %in% connectors,
      preceding = lag(word),
      following = lead(word)
    ) %>% 
    filter(is_connector == TRUE) %>% 
    count(preceding, following, name = "occurences") %>% 
    construct_net()
}

#' @export
#' @method phrase_net data.frame
phrase_net.data.frame <- function(data, connectors = c("to", "in", "at", "and", "of"), ..., text){
  assert_that(!missing(text), msg = "Missing `text` column.")

  text_enquo <- enquo(text)
  data <- select(data, text = !!text_enquo)

  data %>% 
    tidytext::unnest_tokens(word, text) %>% 
    mutate(
      is_connector = word %in% connectors,
      preceding = lag(word),
      following = lead(word)
    ) %>% 
    filter(is_connector == TRUE) %>% 
    count(preceding, following, name = "occurences") %>% 
    construct_net()
}

#' Filter Net
#' 
#' Filter edges that contain specific words.
#' 
#' @inheritParams plot_sigmajs
#' @param words Vector or words to remove edges.
#' 
#' @examples
#' data(reuters)
#' phrase_net(reuters, text = text) %>% 
#'   filter_net(c("a", "the"))
#' 
#' @export
filter_net <- function(net, words) UseMethod("filter_net")

#' @export
filter_net.default <- function(net, words){
  assert_that(!missing(words), msg = "Missing `words` vector.")

  net %>% 
    dplyr::filter(!preceding %in% words) %>%
    dplyr::filter(!following %in% words) 
}

#' Plot sigmajs
#' 
#' @param net An object of class \code{phrasenet} as returned by \code{\link{phrase_net}}.
#' 
#' @examples
#' data(reuters)
#' phrase_net(reuters, text = text) %>% 
#'   plot_sigmajs()
#' 
#' @export
plot_sigmajs <- function(net) UseMethod("plot_sigmajs")

#' @export
plot_sigmajs.default <- function(net){
  net$id <- 1:nrow(net)
  nodes <- tibble::tibble(
    id = c(net$preceding, net$following)
  ) %>% 
    count(id) %>% 
    mutate(label = id)

  sigmajs::sigmajs() %>% 
    sigmajs::sg_nodes(nodes, id, size = n, label) %>% 
    sigmajs::sg_edges(net, id, source = preceding, target = following) %>% 
    sigmajs::sg_layout() %>% 
    sigmajs::sg_cluster(colors = c("#247BA0", "#70C1B3", "#B2DBBF", "#F3FFBD", "#FF1654")) %>% 
    sigmajs::sg_neighbours()
}