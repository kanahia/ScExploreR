#' 
#' 
#' #' downloadButton_UI
#' #'
#' #' @param id 
#' #' @param label 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' downloadButton_UI <- function(id,
#'                               label = "Download"){
#' 
#'   ns <- shiny::NS(id)
#' 
#'   shiny::downloadButton(
#'     outputId = ns("downloadData"),
#'     label = label,
#'     style = "color: #fff; background-color: #27AE60; border-radius: 10%;")
#' 
#' }
#' 
#' 
#' #' downloadButton_Server
#' #'
#' #' @param id 
#' #' @param dataset 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' downloadButton_Server <- function(id, dataset) {
#'   shiny::moduleServer(
#'     id,
#'     function(input, output, session) {
#' 
#'       output$downloadData <-
#'         shiny::downloadHandler(
#'           filename = function() {
#'             paste("AbuNahia2022_", "markers", ".csv", sep="")
#'           },
#'           content = function(file) {
#'             write.csv(!!sym(dataset), file, row.names = FALSE)
#'           }
#'         )
#' 
#'     })
#' }