#' FeaturePlotShiny UI
#'
#' @importFrom shiny textInput NS
#' @importFrom shinycssloaders withSpinner
#'
#' @export
ExpressionLevelUI <- function(id,
                              gene_label = "Select gene",
                              cluster_label = "Cluster:", 
                              selected = "myh6",
                              choices = levels(metadata_all$edited_res.1.5),
                              width="100px",
                              plot_height = 450
                              ) {
  
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::selectizeInput( 
            inputId = ns("gene_selector"), 
            label = gene_label,
            choices = NULL,
            selected = selected,
            width = width)
        ),
        shiny::column(
          width = 4,
          offset = 0,
          shiny::selectInput(
            inputId = ns("cluster"),
            label = cluster_label,
            choices = choices,
            selected = "Myocardium",
            multiple = FALSE,
            width = '180px')
          )
        ),
      
      plotly::plotlyOutput(ns("test_violin"), width = "auto", height = plot_height)
      
      )
    )
  
}



#' ExpressionLevelShiny
#'
#' @importFrom shiny moduleServer renderPlot
#'
#' @export
ExpressionLevelShiny <- function(id,
                                 clustering,
                                 selected = "myh6",
                                 metadata = NULL, 
                                 slot_data = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      updateSelectizeInput(session = session, 
                           inputId =  "gene_selector", 
                           choices = sort(unique(rownames(slot_data))),
                           selected = selected,
                           server = TRUE)
      
      output$test_violin <- plotly::renderPlotly({
        
        ScExploreR::ViolinGeneExpStage(gene = input$gene_selector,
                                       cluster = input$cluster,
                                       clustering = clustering,
                                       metadata = metadata, 
                                       slot_data = slot_data)
        })
      }
    )
}


