#' Phrase Net
#' 
#' Create phrase network.
#' 
#' @param data A data.frame or \link[tibble]{tibble} containig \code{text}.
#' @param connectors Words that establish connections between words and 
#' ultimately form edges.
#' @param keep_connector Whether to include the connecting terms 
#' (\code{connectors}) in the edgelist returned.
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
phrase_net <- function(data, connectors = c("to", "in", "at", "and", "of"), 
  keep_connector = FALSE, ...) UseMethod("phrase_net")

#' @export
phrase_net.default <- function(data, connectors = c("to", "in", "at", "and", "of"),
  keep_connector = FALSE, ...){

  data <- tibble::tibble(text = data)

  net <- data %>% 
    tidytext::unnest_tokens(connector, text) %>% 
    mutate(
      is_connector = connector %in% connectors,
      preceding = lag(connector),
      following = lead(connector)
    ) %>% 
    filter(is_connector == TRUE) %>% 
    filter(!is.na(preceding)) %>% 
    filter(!is.na(following))

  if(keep_connector)
    net <- count(net, preceding, connector, following, name = "occurences")
  else
    net <- count(net, preceding, following, name = "occurences")
  
  construct_net(net)
}

#' @export
#' @method phrase_net data.frame
phrase_net.data.frame <- function(data, connectors = c("to", "in", "at", "and", "of"),
  keep_connector = FALSE, ..., text){
  assert_that(!missing(text), msg = "Missing `text` column.")

  text_enquo <- enquo(text)
  data <- select(data, text = !!text_enquo)

  net <- data %>% 
    tidytext::unnest_tokens(connector, text) %>% 
    mutate(
      is_connector = connector %in% connectors,
      preceding = lag(connector),
      following = lead(connector)
    ) %>% 
    filter(is_connector == TRUE) %>% 
    filter(!is.na(preceding)) %>% 
    filter(!is.na(following))

  if(keep_connector)
    net <- count(net, preceding, connector, following, name = "occurences")
  else
    net <- count(net, preceding, following, name = "occurences")
  
  construct_net(net)
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
    dplyr::filter(!following %in% words) %>% 
    construct_net()
}

#' Plot sigmajs
#' 
#' @param net An object of class \code{phrasenet} as returned by \code{\link{phrase_net}}.
#' 
#' @examples
#' data(reuters)
#' 
#' \dontrun{
#' phrase_net(reuters$text[1:10]) %>% 
#'   plot_sigmajs()
#' }
#' 
#' @export
plot_sigmajs <- function(net) UseMethod("plot_sigmajs")

#' @export
plot_sigmajs.phrasenet <- function(net){
  net$connector <- NULL

  nodes <- tibble::tibble(
    id = c(net$preceding, net$following)
  ) %>% 
    count(id) %>% 
    mutate(label = id)

  net <- net %>% 
    dplyr::group_by(preceding, following) %>% 
    dplyr::summarise(occurences = sum(occurences)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      id = 1:dplyr::n(),
      type = "curvedArrow"
    )

  sigmajs::sigmajs() %>% 
    sigmajs::sg_nodes(nodes, id, size = n, label) %>% 
    sigmajs::sg_edges(
      net, 
      id, 
      source = preceding, 
      target = following, 
      size = occurences, 
      type
    ) %>% 
    sigmajs::sg_layout() %>% 
    sigmajs::sg_drag_nodes() %>% 
    sigmajs::sg_cluster(colors = c("#247BA0", "#70C1B3", "#B2DBBF", "#F3FFBD", "#FF1654")) %>% 
    sigmajs::sg_neighbours() %>% 
    sigmajs::sg_settings(
      edgeLabelSize = "proportional",
      minEdgeSize = 1,
      maxEdgeSize = 3
    )
}