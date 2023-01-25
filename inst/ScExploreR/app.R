devtools::load_all()

ui <- 
  shinydashboard::dashboardPage(
    skin = "blue",
    shinydashboard::dashboardHeader(
      shiny::tags$li(
        class = "dropdown",
        shiny::tags$style(".main-header {max-height: 100px}"),
        #shiny::tags$head(shiny::tags$style(".shiny-plot-output{height:50vh !important;}")),
        shiny::tags$style(".main-header .logo {height: 100px;}")
        ),
      shiny::tags$li(
        class="dropdown",
        tags$a(href="https://www.linkedin.com/in/kanahia/", shiny::icon("linkedin"), 
               "LinkedIn", target="_blank")
        ),
      shiny::tags$li(
        class="dropdown",
        tags$a(href="https://github.com/kanahia", shiny::icon("github"), 
               "GitHub", target="_blank")
        ),
      title = header_title, #in css formating
      titleWidth = 220
      ),
    shinydashboard::dashboardSidebar(
      shiny::tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      width = 220,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Welcome panel",
          tabName = "welcome",
          icon = shiny::icon("stats", lib = "glyphicon")
        ),
        shinydashboard::menuItem(
          "Whole heart",
          tabName = "whole_heart",
          icon = shiny::icon("heart", lib = "glyphicon"),
          shinydashboard::menuSubItem("Visualization", tabName = "visualization_all"),
          shinydashboard::menuSubItem("Differential expression", tabName = "DE"),
          shinydashboard::menuSubItem("Cell markers", tabName = "gene_table_all"),
          shinydashboard::menuSubItem("Enrichment analysis", 
                                      tabName = "enrichment_analysis",  
                                      icon = shiny::icon("blackberry", lib = "font-awesome"))
          ),
        shinydashboard::menuItem(
          "Myocardium",
          tabName = "myocardium",
          icon = shiny::icon("triangle-top", lib = "glyphicon"),
          shinydashboard::menuSubItem("Visualization", tabName = "visualization_myo"),
          shinydashboard::menuSubItem("Cell markers", tabName = "gene_table_myo")
          )
        )
      ),
    shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
      #added in css_formating
      skin_colors,
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "welcome",
          shiny::fluidRow(
            main_text,
            shiny::br()
            ),
          shiny::fluidRow(
            shiny::column(width = 9,
                          experiment),
            shiny::column(width = 3)
            )
          ),
        shinydashboard::tabItem(
          "visualization_all",
          shiny::tabPanel(
            "Visualization",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Overview",
                shiny::wellPanel(
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 6, 
                      align = "center",
                      shinycustomloader::withLoader(
                        type = "html",
                        loader = "dnaspin",
                        shiny::plotOutput("DimPlot", width = "auto", height = "600")
                        )
                      ),
                    shinydashboard::box(width = 6,
                                        ScExploreR::MultiPlot_UI("Multiplot"))
                    
                                  
                    )
                  ),
                shiny::wellPanel(
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 6,
                      style = 'padding-top:0px; color: white',
                      ScExploreR::FeaturePlotShinyUI(
                        "all_featureplot_1",
                        value = "myh6",
                        placeholder = "myh6")
                      ),
                    shinydashboard::box(
                      width = 6,
                      style = 'padding-top:0px; color: white;',
                      ScExploreR::FeaturePlotShinyUI(
                        "all_featureplot_2",
                        value = "myh7l",
                        placeholder = "myh7l")
                      )
                    )
                  )
                )
              ) # tabsetPanel end
            ) # the end of TabPanel
          ),
        shinydashboard::tabItem(
          "gene_table_all",
          shiny::fluidRow(
            shiny::column(
              width = 1,
              offset = 11, 
              #downloadButton_UI(id = "all_m", label = "Download")
              shiny::downloadButton(
                outputId = "downloadData",
                label = "Download",
                style = "color: #fff; background-color: #27AE60; border-radius: 360px;")
              )
            ),
          shiny::fluidRow(
            shiny::h1("Marker genes", style = "margin: 15px;"),
            shinydashboard::box(
              DT::dataTableOutput("markers_all"),
              width = 12
              )
            )
        ),
        shinydashboard::tabItem(
          tabName = "enrichment_analysis",
          ScExploreR::enrichment_analysis_UI(id = "enrich")
          
         ),
        shinydashboard::tabItem(
          "visualization_myo",
          shiny::fluidRow(
            shiny::column(
              #offset = 1,
              width = 6,
              style = 'padding-top:0px; height:200px;',
              ScExploreR::FeaturePlotShinyUI(
                "myo_featureplot_1",
                value = "myh6",
                placeholder = "myh6"
              )
            ),
            shiny::column(
              width = 6,
              #offset = 1,
              style = 'padding-top:0px;',
              ScExploreR::FeaturePlotShinyUI(
                "myo_featureplot_2",
                value = "myh7l",
                placeholder = "myh7l"
              )
            )
          )
        ),
        shinydashboard::tabItem(
          "gene_table_myo",
          shiny::fluidPage(shiny::h1("Marker genes: Myocardium")),
          DT::dataTableOutput("markers_myo")
        ),
        shinydashboard::tabItem(
          tabName = "DE",
          #shiny::wellPanel(
          shiny::h1("Differential expression"),
          shiny::p(rep("some text some text", times = 50) %>% paste0(collapse = " "), 
                   style = "font-size: 18px; align: justify;"),
          shiny::br(),
            shiny::fluidRow(
              shiny::column(width = 2,
                            shiny::selectInput(inputId = "cluster_1", 
                                   label = "Cluster 1:",
                                   choices = sort(levels(metadata_all$edited_res.1.5)),
                                   width = "240px"
                                   ),
                            shiny::selectInput(inputId = "cluster_2", 
                                   label = "Cluster 2:",
                                   choices = sort(levels(metadata_all$edited_res.1.5)),
                                   width = "240px",
                                   selected = "Bulbus arteriosus"),
                       shinyWidgets::chooseSliderSkin("Modern", color = "#3C8DBC"),
                       #shinyWidgets::setSliderColor(c("lightsalmon1"), c(1)),
                       shiny::sliderInput(inputId = "slider",
                                   label = "Number of genes",
                                   min = 1, max = 20, value = 10, step = 1),
                       shiny::radioButtons(inputId = "sort_by", 
                                    label = "Label genes by:",
                                    choices = c("log2FoldChange" = "avg_log2FC",
                                                "Significance" = "p_val_adj"),
                                    selected = "avg_log2FC",
                                    inline = FALSE)
                       ),
              shinydashboard::box(
                shiny::tags$style(class = 'volcano-plot'),
              #shiny::column(
                width = 5,
                shinycustomloader::withLoader(
                  type = "html",
                  loader = "dnaspin",
                  shiny::plotOutput(outputId = "Volcano_plot", width = "100%", height = "700"))
                #shiny::tags$style(".shiny-plot-output{height:50vh !important;}")))
                ),
              shinydashboard::box(
              #shiny::column(
                width = 5,
                DT::dataTableOutput("DE_cluster", width = "100%", height = "700"),
                style = "overflow-y: auto; height: 718px;"
                )
              )
            #)
          )
        ) # end of tabItems
      ), #dashboard body
    title = title
    )


server <- function(input, output, session) {

  output$markers_all <-
    DT::renderDataTable(markers_all, options = list(pageLength = 10), filter = "top")
  
  output$markers_myo <-
    DT::renderDataTable(markers_myo, options = list(pageLength = 10), filter = "top")

  output$DimPlot <-
    shiny::renderPlot({
      ggplot2::ggplot(metadata_all) +
      ggplot2::geom_point(ggplot2::aes(
        x = UMAP_1,
        y = UMAP_2,
        color = edited_res.1.5),
        alpha = 0.9,
        size = 0.1
      ) +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme() +
      Seurat::NoLegend() +
      ggplot2::scale_color_manual(values = ScExploreR::colors_main_umap) + # TODO improve color scheme name
      ggrepel::geom_label_repel(
        data = labels_all,
        x = labels_all$u1,
        y = labels_all$u2,
        label = labels_all$label  %>% stringr::str_wrap(., width = 10),
        fill = labels_all$color,
        alpha = 0.8
        )
    })

  ScExploreR::MultiPlot_Shiny(
    id = "Multiplot",
    metadata = metadata_all)
  
  # FT plots for all data
  ScExploreR::FeaturePlotShiny(
              id = "all_featureplot_1",
              metadata = metadata_all,
              data_slot = slot_data_all,
              identity = "edited_res.1.5")
  
  ScExploreR::FeaturePlotShiny(
              id = "all_featureplot_2",
              metadata = metadata_all,
              data_slot = slot_data_all,
              identity = "edited_res.1.5")

  # FT plots for myocardium  
  ScExploreR::FeaturePlotShiny(
              id = "myo_featureplot_1",
              metadata = metadata_myo,
              data_slot = myo_slot_data,
              identity = "custom_int_res2")
  
  ScExploreR::FeaturePlotShiny(
              id = "myo_featureplot_2",
              metadata = metadata_myo,
              data_slot = myo_slot_data,
              identity = "custom_int_res2")
  
  output$test_plot <- shiny::renderPlot({
    
    ggplot2::ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
    ggplot2::geom_point(aes(color=Species, shape=Species))
  })
  
  # testing plots
  output$Volcano_plot <- shiny::renderPlot({
    
    if(input$cluster_1 == input$cluster_2){
      shinyalert::shinyalert("Oops!",
                             "Make sure to choose different clusters!",
                             type = "error",
                             className = "my_alert")
    } else {
      volcano_plot(integrated_data,
                   markers = DE_list[[paste0(input$cluster_1, "_vs_", input$cluster_2)]],
                   ident.1 = input$cluster_1,
                   ident.2 = input$cluster_2,
                   avg_log2FC.1 = NULL,
                   avg_log2FC.2 = NULL,
                   plot_top = if(input$sort_by == "avg_log2FC") {TRUE} else if(input$sort_by == "p_val_adj") {FALSE},
                   n_genes = input$slider,
                   height = 45,
                   # pos.label.1 = 1,
                   # pos.label.2 = -1,
                   label.title.size = 6.5 ,
                   ann_text_size = 6)
      }
      
    }) %>%  shiny::bindCache(input$cluster_1, input$cluster_2, input$sort_by, input$slider)
    
  output$DE_cluster <- DT::renderDataTable(

    DE_list[[paste0(input$cluster_1, "_vs_", input$cluster_2)]][, c(2:5,7)] %>%
      DT::datatable(options = list(pageLength = 15, 
                                   filter = "top",
                                   height = "700",
                                   autoWidth = TRUE, 
                                   scrollX = FALSE,
                                   #scrollY = "400px",
                                   columnDefs = list(list(width = "75px", targets = 0),
                                                     list(width = "40px", targets = 1:4),
                                                     list(width = "100px", targets = 5),
                                                     list(className = 'dt-left', targets = "_all"))))
      #DT::formatRound(columns = c(1, 2), digits = 2)
    )
  
  ScExploreR::enrichment_analysis_Shiny(
    id = "enrich")
  
  output$downloadData <-
    shiny::downloadHandler(
      filename = function() {
        paste("AbuNahia2022_", "markers_all", ".csv", sep="")
        },
      content = function(file) {
        write.csv(markers_all, file, row.names = FALSE)
        }
      )
  
}

shiny::shinyApp(ui, server)
