devtools::load_all()

title <- "ScExploreR" # name to display in title bar
header_title <- # logo to display in header bar
  shiny::tags$a(
    href = 'https://zdglab.iimcb.gov.pl/',
    shiny::tags$img(
      src = "zdg_logo.png",
      height = '100',
      width = '200',
      align = "center"
    ),
    target = "_blank"
  )

ui <- 
  shinydashboard::dashboardPage(
    skin = "blue",
    shinydashboard::dashboardHeader(
      shiny::tags$li(
        class = "dropdown",
        shiny::tags$style(".main-header {max-height: 100px}"),
        shiny::tags$style(".main-header .logo {height: 100px;}")
        ),
      title = header_title,
      titleWidth = 220
      ),
    shinydashboard::dashboardSidebar(
      shiny::tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      width = 220,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Whole heart",
          tabName = "whole_heart",
          icon = icon("heart", lib = "glyphicon"),
          shinydashboard::menuSubItem("Gene table", tabName = "gene_table_all"),
          shinydashboard::menuSubItem("Visualization", tabName = "visualization_all"),
          shinydashboard::menuSubItem("Differential expression", tabName = "DE")
          ),
        shinydashboard::menuItem(
          "Myocardium",
          tabName = "myocardium",
          icon = icon("triangle-top", lib = "glyphicon"),
          shinydashboard::menuSubItem("Gene table", tabName = "gene_table_myo"),
          shinydashboard::menuSubItem("Visualization", tabName = "visualization_myo")
          )
        )
      ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          "visualization_all",
          shiny::tabPanel(
            "Visualization",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Overview",
                shiny::wellPanel(
                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      shinycustomloader::withLoader(
                        type = "html",
                        loader = "dnaspin",
                        shiny::plotOutput("DimPlot", width = "85%", height = "600")
                        )
                      ),
                    shiny::column(width = 6,
                                  ScExploreR::MultiPlot_UI("Multiplot"))
                    )
                  ),
                shiny::wellPanel(
                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      style = 'padding-top:0px; height:200px;',
                      ScExploreR::FeaturePlotShinyUI(
                        "all_featureplot_1",
                        value = "myh6",
                        placeholder = "myh6")
                      ),
                    shiny::column(
                      width = 6,
                      style = 'padding-top:0px;',
                      ScExploreR::FeaturePlotShinyUI(
                        "all_featureplot_2",
                        value = "myh7l",
                        placeholder = "myh7l")
                      )
                    )
                  )
                ),
              shiny::tabPanel(
                "Test tab",
                shiny::wellPanel(
                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      shiny::plotOutput("test_plot", width = "85%", height = "600")
                      )
                    )
                  )
                )
              ) # tabsetPanel end
            ) # the end of TabPanel
          ),
        shinydashboard::tabItem(
          "gene_table_all",
          shiny::fluidPage(shiny::h1("Gene table: Whole heart")),
          DT::dataTableOutput("markers_all")
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
          shiny::fluidPage(shiny::h1("Gene table: Myocardium")),
          DT::dataTableOutput("markers_myo")
        ),
        shinydashboard::tabItem(
          tabName = "DE",
          shiny::wellPanel(
            shiny::fluidRow(
              column(width = 2,
                     selectInput(inputId = "cluster_1", 
                                 label = "Cluster 1:",
                                 choices = sort(levels(metadata_all$edited_res.1.5)),
                                 width = "240px"
                                 ),
                     selectInput(inputId = "cluster_2", 
                                 label = "Cluster 2:",
                                 choices = sort(levels(metadata_all$edited_res.1.5)),
                                 width = "240px",
                                 selected = "Bulbus arteriosus"),
                     radioButtons(inputId = "sort_by", 
                                  label = "Label genes by:",
                                  choices = c("Significance" = "p_val_adj",
                                              "log2FoldChange" = "avc_log2FC"),
                                  inline = TRUE)
                     ),
              column(width = 10,
                     shiny::plotOutput(outputId = "Volcano_plot", width = "85%", height = "600")
                     )
            )
          )
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
    
    # volcano_plot(integrated_data, 
    #              markers = markers_all,
    #              ident.1 = "Myocardium",
    #              ident.2 = "AV cushion",
    #              avg_log2FC.1 = -3,
    #              avg_log2FC.2 = 3,
    #              plot_top = TRUE, 
    #              n_genes = 10,
    #              height = 45,
    #              pos.label.1 = 1.7,
    #              pos.label.2 = -1.62,
    #              label.title.size = 6.5 ,
    #              ann_text_size = 6) + 
    #   #theme(plot.margin = unit(c(0.1,8.5,0.1,8.5), "cm")) +
    #   scale_x_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, by = 1)) +
    #   coord_fixed(ratio = 0.0163)
    
    ggplot2::ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) +
    ggplot2::geom_point(aes(color=Species, shape=Species))
    
  })
}

shiny::shinyApp(ui, server)
