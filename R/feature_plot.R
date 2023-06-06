#' FeaturePlotShiny UI
#'
#' @importFrom shiny textInput NS
#' @importFrom shinycssloaders withSpinner
#'
#' @export
FeaturePlotShinyUI <- function(id, 
                               label="Select gene:",
                               value="",
                               placeholder="myh6",
                               width="100px",
                               label_radio = "Stage:") {
    ns <- shiny::NS(id)
    
    shiny::tags$div(
       style = "background-color: gray99; color: black; padding-top: 6px",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::textInput(
                inputId = ns("gene_selector"), 
                label = label,
                value = value,
                placeholder = placeholder,
                width = width)
                ),
          shiny::column(
            width = 3,
            shiny::tags$div(
              class = "my_test_id",
              shiny::radioButtons(
                inputId = ns("stage_FT_plot"), 
                label = label_radio ,
                choices = list("All" = "all",
                               "48h" = "dpf2", 
                               "72h"= "dpf3"),
                inline = TRUE, 
                selected = "all", 
                width = "150px")
              )
            )
          ),
          shinycustomloader::withLoader(type = "html",
                                        loader = "dnaspin",
                                        shiny::plotOutput(ns("feature_plot"), 
                                                          width = "100%", 
                                                          height = "500"))
       )
}

#' FeaturePlotShiny
#'
#' @importFrom shiny moduleServer renderPlot
#'
#' @export
FeaturePlotShiny <- function(id, 
                             metadata = NULL, 
                             data_slot = NULL, 
                             identity, 
                             order = FALSE, 
                             stage) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
          
          rec_metadata <- reactive({
            if(input$stage_FT_plot == "dpf2") {
              metadata <- 
                metadata %>% dplyr::filter(stage == "48h")
            } else if(input$stage_FT_plot == "dpf3") {
              metadata <- 
                metadata %>% dplyr::filter(stage == "72h")
            } else if (input$stage_FT_plot == "all") {
              metadata <- metadata
            }
          })
          
            output$feature_plot <- shiny::renderPlot({
              
              # shiny::validate(
              #   shiny::need(
              #     is.null(input$gene_selector), "LOOOOOOOOOOOOOL")
              #   )
              
                my_FeaturePlot(
                    metadata = rec_metadata(),
                    data_slot = data_slot,
                    gene = input$gene_selector,
                    identity = identity, # TODO add identity choice?
                    order = order,
                    label = TRUE
                )
            }) %>% shiny::bindCache(input$gene_selector,
                                    input$stage_FT_plot)
        }
    )
}