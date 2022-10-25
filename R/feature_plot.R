#' FeaturePlotShiny UI
#'
#' @importFrom shiny textInput NS
#' @importFrom shinycssloaders withSpinner
#'
#' @export
FeaturePlotShinyUI <- function(id, 
                               label="Choose gene",
                               value="",
                               placeholder="",
                               width="100px",
                               spinnertype = 5) {
    ns <- shiny::NS(id)
    shiny::tags$div(
        style = "background-color: gray99;box-shadow: 1px 2px;",
        shiny::textInput(
            inputId = ns("gene_selector"), 
            label = label,
            value = value,
            placeholder = placeholder,
            width = width),
        shinycssloaders::withSpinner(
            type = spinnertype,
            shiny::plotOutput(ns("feature_plot"), width = "100%")
        )
    )
}

#' FeaturePlotShiny
#'
#' @importFrom shiny moduleServer renderPlot
#'
#' @export
FeaturePlotShiny <- function(id, metadata, data_slot, identity) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            output$feature_plot <- shiny::renderPlot({
                my_FeaturePlot(
                    metadata = metadata_all,
                    data_slot = slot_data_all,
                    gene = input$gene_selector,
                    identity = identity, # TODO add identity choice?
                    order = FALSE,
                    label = TRUE
                )
            })
        }
    )
}
