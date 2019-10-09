library(shiny)
library(dplyr)
library(sigmajs)
library(phrasenets)

ui <- fluidPage(
  tags$head(
    tags$link(href = "phrasenets-assets/styles.css", rel="stylesheet", type="text/css")
  ),
  img(src="https://news-r.org/news-r-margin.png", id = "logo"),
  tags$a("Made by news-r.org", href="https://news-r.org", target="_blank", id = "tagline"),
  fluidRow(
    column(
      3,
      h2("phrasenets", id = "brand"),
      p(
        class = "centered",
        "Paste your text in the box below (up to 10,000 characters) to reveal the phrasenet."
      ),
      textAreaInput(
        "text",
        width = "100%",
        rows = 20, 
        label = NULL, 
        placeholder = "Paste your text in here. 1000 characters maximum."
      ),
      fluidRow(
        column(10, textInput("connectors", "Connectors:", value = "to, and, at, in, of")),
        column(2, br(), actionButton("send", "", icon = icon("search")))
      ),
      uiOutput("slider")
    ),
    column(9, sigmajsOutput("net", width = "100%", height = "100vh"))
  )
)

server <- function(input, output, session) {

  net <- eventReactive(input$send, {

    cons <- input$connectors %>% 
      strsplit(split = ",") %>% 
      .[[1]] %>% 
      trimws()

    print(cons)

    if(input$text == "")
      return(NULL)

    edges <- input$text %>%
      substr(1, 10000) %>%   
      phrase_net(connectors = cons) %>% 
      mutate(id = 1:n())

    nodes <- tibble::tibble(
      id = c(edges$preceding, edges$following)
    ) %>% 
      count(id) %>% 
      mutate(label = id)

    list(edges = edges, nodes = nodes)    
  })

  output$net <- renderSigmajs({

    if(is.null(net()))
      return(sigmajs())

    sigmajs() %>% 
      sg_nodes(net()$nodes, id, size = n, label) %>% 
      sg_edges(net()$edges, id, source = preceding, target = following) %>% 
      sg_layout() %>% 
      sg_cluster(colors = c("#247BA0", "#70C1B3", "#B2DBBF", "#F3FFBD", "#FF1654")) %>% 
      sg_neighbours() %>% 
      sg_settings(
        labelThreshold = 1,
        labelSizeRatio = 1,
        labelSize = "proportional",
        font = "Montserrat",
        fontStyle = "sans-serif"
      )
  })

  output$slider <- renderUI({
    if(!is.null(net())){
      rng <- range(net()$nodes$n)
      
      sliderInput(
        "nodeRange",
        "Node Size Range",
        min = rng[1],
        max = rng[2],
        value = rng,
        step = 1,
        dragRange = TRUE,
        width = "100%"
      )
    }
  })

  observeEvent(input$nodeRange, {
    req(input$nodeRange)

    sigmajsProxy("net") %>% 
      sg_filter_undo_p("filter1") %>% 
      sg_filter_undo_p("filter2") %>% 
      sg_filter_gt_p(input$nodeRange[1] - 1, var = "size", target = "nodes", name = "filter1") %>% 
      sg_filter_lt_p(input$nodeRange[2] + 1, var = "size", target = "nodes", name = "filter2")
  })

}

shinyApp(ui, server)