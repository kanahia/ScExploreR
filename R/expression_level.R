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
          ),
        shiny::column(shiny::div(class = "info-icon",
                                 shiny::icon("info-circle", class = "fa-lg", 
                                             "data-toggle" = "tooltip"),
                                 shiny::div(class = "info-tooltip-bottom", 
                                            info_exp_lvl)),
                      width = 1)
        ),
      
      shiny::fluidRow(
          shinydashboard::box(
            shinycustomloader::withLoader(
              type = "html",
              loader = "dnaspin",plotly::plotlyOutput(ns("test_violin"), 
                                                   width = "auto", 
                                                   height = plot_height)
              ),
            
            shiny::fluidRow(
              shiny::column( 
                width = 3, offset = 3,
                shinydashboard::valueBoxOutput(outputId = ns("cells_48h"), width = 4) %>%
                  shiny::tagAppendAttributes(style= 'margin-top: -10px; align: center;')
                ),
              shiny::column(
                width = 3,
                shinydashboard::valueBoxOutput(outputId = ns("cells_72h"), width = 4) %>%
                  shiny::tagAppendAttributes(style= 'margin-top: -10px; align: center;')
                )
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
      
      
      n_cells_for_value <- 
        shiny::reactive({
          shiny::req(input$gene_selector)
          
          metadata %>% 
            dplyr::filter(cell %in% names(slot_data[input$gene_selector, 
                                                    slot_data[input$gene_selector, ] > 0]
                                          )
                          ) %>% 
            dplyr::select(cell, stage, !!sym(clustering)) %>%
            group_by(!!sym(clustering), stage) %>% 
            summarise(n = n()) %>%
            dplyr::filter(!!sym(clustering) %in% input$cluster)
          # cell_names <- 
          #   names(slot_data[input$gene_selector, slot_data[input$gene_selector, ] > 0])
          # 
          # base::as.matrix(table(metadata$stage[metadata$cell %in% cell_names]))
   
        })
      
      output$cells_48h <-
        shinydashboard::renderValueBox({
          if ("48h" %in% n_cells_for_value()$stage) {
            shinydashboard::valueBox(value = 
                                       shiny::tags$p(
                                         paste0("Cells ",
                                                n_cells_for_value()$n[n_cells_for_value()$stage=="48h"]),
                                         style = "font-size: 20px; text-align: center;"),
                                     subtitle = shiny::tags$p("48h", style = "font-size: 18px; text-align: center;"),
                                     width = NULL,
                                     color = "aqua")
          } else {
            shinydashboard::valueBox(value = 
                                       shiny::tags$p(paste0("Cells: ", value =  0),
                                                     style = "font-size: 20px; text-align: center;"),
                                     subtitle = shiny::tags$p("48h", style = "font-size: 18px; text-align: center;"),
                                     width = NULL,
                                     color = "aqua")
          }
        })
      
      output$cells_72h <-
        shinydashboard::renderValueBox({
          if ("72h" %in% n_cells_for_value()$stage) {
            shinydashboard::valueBox(value = 
                                       shiny::tags$p(
                                         paste0("Cells: ", n_cells_for_value()$n[n_cells_for_value()$stage=="72h"]),
                                         style = "font-size: 20px; text-align: center;"),
                                     subtitle = shiny::tags$p("72h", style = "font-size: 18px; text-align: center;"),
                                     width = NULL,
                                     color = "yellow")
          } else {
            shinydashboard::valueBox(value = 
                                       shiny::tags$p(paste0("Cells: ", 0),
                                                     style = "font-size: 20px; text-align: center;"),
                                     subtitle = shiny::tags$p("72h", style = "font-size: 18px; text-align: center;"),
                                     width = NULL,
                                     color = "yellow")
          }
        })
      
      
      output$exp_genes_in_cell <- DT::renderDataTable({ #shiny::renderPrint({
        event.data <- plotly::event_data("plotly_click", source = "click")
        
        if(is.null(event.data) == T) return(NULL)
        
        # Find selected cell
        if(! "key" %in% colnames(event.data)) {
          
            shiny::showNotification(ui = "This is a boxplot component not a cell. Please select a valid cell.",
                                    type = "error",
                                    duration = 5)
          }
              
        whichCell <- unique(event.data[1,5])[[1]]
        
        if(!is.null(whichCell)) {
          
          res <- data.frame("cell" = whichCell,
                            "cluster" = metadata[[clustering]][metadata$cell == whichCell],
                            "norm.counts" = round(sort(Data2extractCell[, whichCell], decreasing = T), 4)
          )
        } else {
          res <- NULL
        } 

        #res <- whichCell
        return(res) 
      })
    }
  )
}


