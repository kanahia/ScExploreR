#' enrichment_analysis_UI
#'
#' @importFrom shiny textInput NS textAreaInput
#' @importFrom shinycssloaders withSpinner
#'
#' @export
enrichment_analysis_UI <- function(id, 
                                   label = "Paste input genes here: ",
                                   info = "Upload yout genes of interest for gene set enrichment analysis",
                                   placeholder =  "myh6\ncmlc2\nisl1\ntbx2a\n..."){
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
        radioButtons(inputId = ns("choose_reference"), 
                     label = "Annotate by:",
                     choices = c("Single cell heart data" = "scData",
                                 "ZFIN anatomical terms" = "anatomicalData"),
                     selected = "scData",
                     inline = FALSE),
        shiny::textAreaInput(
          inputId = ns("caption"), 
          label = label, 
          width = "100%", 
          height = "400px",
          value = "", 
          placeholder = placeholder
        ),
        shiny::actionButton(inputId = ns("button"),
                            label = "Submit!", 
                            icon = shiny::icon("file-upload", lib = "glyphicon"), 
                            width = NULL,
                            style="color: #fff; background-color: #3c8dbc; border-radius: 15%;")
      ),
      shiny::column(
        width = 5,
        shiny::plotOutput(outputId = ns("text_output"),
                          height = "600px",
                          width = "85%")),
      shiny::column(
        width = 5,
        DT::dataTableOutput(outputId = ns("enrich_table"),
                            height = "100%",
                            width = "100%")
        )
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
      click <- 
        eventReactive(
          eventExpr = input$button,
          valueExpr = {
            out_df <- 
              data.frame("gene" =
                           unlist(
                             stringr::str_split(string = input$caption,
                                                pattern = "\\\n"))) %>%
              dplyr::left_join(., drerio_mart_ids, by = c("gene" = "Gene name")) %>%
              dplyr::select(NCBI) %>%
              tidyr::drop_na()
            
            #out <-
              clusterProfiler::enricher(
                gene = out_df$NCBI,
                pvalueCutoff = 0.05,
                pAdjustMethod = "BH",
                minGSSize = 10,
                maxGSSize = 500,
                qvalueCutoff = 0.2,
                TERM2GENE =
                  if (input$choose_reference == "scData") {
                    term2gene
                    } else if (input$choose_reference == "anatomicalData") {
                      anatomical_terms},
                TERM2NAME = NA
              )
          })
    
        output$text_output <- shiny::renderPlot({
          
          if(!is.null(click())){
            enrichplot::dotplot(click())+
              one_theme() +
              ggplot2::theme(
                panel.background = ggplot2::element_rect(fill='transparent', colour = NA),
                plot.background = ggplot2::element_rect(fill='transparent', color=NA),
                legend.background = ggplot2::element_rect(fill='transparent'),
                legend.box.background = ggplot2::element_rect(fill='transparent')
              ) +
              ggplot2::scale_alpha(range = c(0.2, 0.8)) +
              viridis::scale_color_viridis(direction = -1)
          }#  click()
        }) #render
        
        output$enrich_table <- 
          DT::renderDataTable(as.data.frame(click()) %>%
                                dplyr::select(-c(1,2,9)) %>%
                                dplyr::mutate(pvalue = rstatix::p_round(pvalue),
                                              p.adjust = rstatix::p_round(p.adjust),
                                              qvalue = rstatix::p_round(qvalue),
                                              gene.name = ScExploreR::ncbi2gene(enrichResult = click())) %>%
                                dplyr::select(-c(geneID)),
                              options = list(pageLength = 10, scrollX = TRUE), 
                              filter = "top")
    } #function
  ) #module
}