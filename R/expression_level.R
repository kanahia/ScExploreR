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
          width = 2,
          shiny::selectizeInput( 
            inputId = ns("gene_selector"), 
            label = gene_label,
            choices = NULL,
            selected = selected,
            width = width)
        ),
        shiny::column(
          width = 3,
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
      
      shiny::fluidRow(
          shinydashboard::box(
            shinycustomloader::withLoader(
              type = "html",
              loader = "dnaspin",plotly::plotlyOutput(ns("test_violin"), 
                                                   width = "auto", 
                                                   height = plot_height)
              )
            ),
          shinydashboard::box(
            cell_matrix_info,
            shinycustomloader::withLoader(
              type = "html",
              loader = "dnaspin",
              DT::dataTableOutput(ns('exp_genes_in_cell'),
                                  width = "auto",
                                  height = plot_height)
              )
            )
       
       #shinydashboard::box(shiny::verbatimTextOutput(ns('exp_genes_in_cell')))
         # )
        )
      
      
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
                                 slot_data = NULL,
                                 boxplot = TRUE,
                                 Data2extractCell = NULL) {
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
                                       slot_data = slot_data, 
                                       only_boxplot = boxplot)
        })
      
      output$exp_genes_in_cell <- DT::renderDataTable({ #shiny::renderPrint({
        event.data <- plotly::event_data("plotly_click", source = "click")

        if(is.null(event.data) == T) return(" ")
        
        # Find selected cell
        if(! "key" %in% colnames(event.data)) {
          notify_user <- 
		 shiny::showNotification(ui = "This is a boxplot component not a cell. Please select a valid cell.",
                 			 type = "error",
                                 	 duration = 5)
                     
          
          returrn(notify_user)

        } else {
          whichCell <- unique(event.data[1,5])[[1]]
          }
        

        res <- data.frame("cell" = whichCell,
                          "cluster" = metadata[[clustering]][metadata$cell == whichCell],
                          "norm.counts" = round(sort(Data2extractCell[, whichCell], decreasing = T), 4)
                          )
        #res <- whichCell
        return(res)
      
        })
      }
    )
}


