#' 
#' #' Ligand receptor
#' #'
#' #' @export
#' LR_panel_UI <- function(id){
#'   
#'   ns <- shiny::NS(id)
#'   
#'   shiny::tabsetPanel(
#'     shiny::tabPanel("Ligand-Receptor",
#'                     shiny::sidebarLayout(
#'                         shiny::sidebarPanel(
#'                           shiny::radioButtons(inputId = ns("stage"), 
#'                                               label = "Developmental stage:",
#'                                               choices = c("48 hpf" = "dev_48h",
#'                                                           "72 hpf" = "dev_72h"),
#'                                               inline = FALSE),
#'                           
#'                           shiny::radioButtons(inputId = ns("threshold"), 
#'                                               label = "Threshold:",
#'                                               choices = c("Medium" = "medium",
#'                                                           "High" = "high"),
#'                                               inline = FALSE),
#'                           
#'                           shiny::actionButton(inputId = ns("L_R_button"),
#'                                               label = "Submit!", 
#'                                               icon = shiny::icon("file-upload", lib = "glyphicon"), 
#'                                               width = NULL,
#'                                               style="color: #fff; background-color: #3c8dbc; border-radius: 15%;"),
#'                           width = 2
#'                           ),
#'                         shiny::mainPanel(
#'                           shinydashboard::box(
#'                             DT::dataTableOutput(outputId = ns("LR_table"),
#'                                                 height = "100%",
#'                                                 width = "100%"),
#'                             width = 12,
#'                             align="left"),
#'                           width = 10)
#'                         )
#'                     ),
#'     shiny::tabPanel("Draft Text", main_text)
#'     )
#' }
#' 
#' #' enrichment_analysis_Shiny
#' #'
#' #' @importFrom shiny moduleServer
#' #'
#' #' @export
#' LR_panel_Shiny <- function(id) {
#'   shiny::moduleServer(
#'     id,
#'     function(input, output, session) {
#'       
#'       click2 <- 
#'         shiny::eventReactive(
#'           eventExpr = input$L_R_button,
#'           valueExpr = {
#'             if(input$stage == "dev_48h" & input$threshold == "medium") {
#'               LR_lig_rec[[1]]
#'             } else if(input$stage == "dev_48h" & input$threshold == "high") {
#'               LR_lig_rec[[2]]
#'             } else if(input$stage == "dev_72h" & input$threshold == "medium") {
#'               LR_lig_rec[[3]]
#'             } else { 
#'               LR_lig_rec[[4]]
#'               }
#'             
#'           }, ignoreNULL = FALSE)
#'       
#'       output$LR_table <- 
#'         DT::renderDataTable(as.data.frame(click2()),
#'                             options = list(pageLength = 15, 
#'                                            scrollX = TRUE,
#'                                            columnDefs =  list(list(width = '150px', targets = 0),
#'                                                               list(width = '100px', targets = 1),
#'                                                               list(width = '30px', className = 'dt-center', targets = 2:4))),
#'                             filter = "top",
#'                             rownames= FALSE)
#'       }
#'     )
#'   }
#'     
#'     
#'       
