#' enrichment_analysis_UI
#'
#' @importFrom shiny textInput NS textAreaInput
#' @importFrom shinycssloaders withSpinner
#'
#' @export
enrichment_analysis_UI <- function(id, 
                                   label = "Paste your input genes here",
                                   info = "Please, upload your list of genes for gene set enrichment analysis",
                                   placeholder =  "myh6\ncmlc2\nisl1\ntbx2a\n..."
                                   ){
  ns <- shiny::NS(id)
  
  shiny::wellPanel(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        h1(info,
           style = "font-size:20px;")
      ),
      shiny::column(
        width = 2,
        shiny::textAreaInput(
          inputId = ns("caption"), 
          label = label, 
          width = "100%", 
          height = "400px",
          value = "", 
          placeholder = placeholder
        ),
        shiny::submitButton(text = "Submit", 
                            icon = shiny::icon("file-upload", lib = "glyphicon"), 
                            width = NULL)
      ),
      shiny::column(
        width = 6,
        shiny::plotOutput(outputId = ns("text_output")))
    )
  )
}


#' enrichment_analysis_Shiny
#'
#' @importFrom shiny moduleServer renderText
#' @importFrom clusterProfiler enricher
#'
#' @export
enrichment_analysis_Shiny <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$caption, {
      
      output$text_output <- shiny::renderPlot({

        out_df <- 
          data.frame("gene" =
                       unlist(
                         stringr::str_split(string = input$caption,
                                            pattern = "\\\n"))) %>%
            dplyr::left_join(., drerio_mart_ids, by = c("gene" = "Gene name")) %>%
            dplyr::select(NCBI) %>%
            tidyr::drop_na()

        out <-
          clusterProfiler::enricher(
            gene = out_df$NCBI,
            pvalueCutoff = 0.05,
            pAdjustMethod = "BH",
            minGSSize = 10,
            maxGSSize = 500,
            qvalueCutoff = 0.2,
            TERM2GENE = term2gene,
            TERM2NAME = NA
          )
        
        if(!is.null(out)) {
          enrichplot::dotplot(out) +
            ggplot2::geom_point(inherit.aes = TRUE, border = "black", alpha = 0.8)+
            one_theme() +
            ggplot2::scale_alpha(range = c(0.2, 0.8))
        } else {print("Your output will appear here:")}
   
      }) #render
    }) #obseevent
    } #function
  ) #module
}

