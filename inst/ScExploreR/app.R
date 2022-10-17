devtools::load_all()
#read data


  ## all_markers
   markers_all <- ScExploreR::read_multiple_sheets(file = "/home/jason/data/shiny_dashboard/heart10x/data/main_clustering_markers.xlsx")
   markers_all <- markers_all %>%
     dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
     dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
     dplyr::mutate(p_val = format.pval(p_val, digits = 2))

  ## markers_myo
   markers_myo <- ScExploreR::read_multiple_sheets("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Plots/Myocardium/markers_myo_new-updated.xlsx")
   markers_myo <- markers_myo %>%
     dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
     dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
     dplyr::mutate(p_val = format.pval(p_val, digits = 2))

   # all_metadata
   metadata_all <- data.table::fread("/home/jason/data/shiny_dashboard/heart10x/data/metadata_all.csv")
   cluster_order <- c("Myocardium", "Bulbus arteriosus", "Epicardium", "Mesoderm progenitors", "AV endocardium", "AV cushion",
                      "Neural crest", "Red blood cells", "Hematopoietic precursor", "Mesenchymal fibroblasts", "Cardiac peripheral nerves",
                      "Neuropeptide secreting neurons", "Leukocytes", "Resident fibroblasts", "Endothelial precursors",
                       "Proliferating cells", "Endothelial cells", "Unclassified")
   metadata_all$edited_res.1.5 <- factor(metadata_all$edited_res.1.5,
                                         levels = cluster_order)


   #metadata myo
   metadata_myo <- data.table::fread("/home/jason/data/shiny_dashboard/heart10x/data/metadata_myo.csv")

   #slot data
   slot_data_all <- readRDS("/home/jason/data/shiny_dashboard/heart10x/data/slot_data_all.rds")
   slot_data_all <- readRDS("/home/jason/data/shiny_dashboard/heart10x/data/slot_data_myo.rds")

   labels_all <-
     metadata_all %>%
     dplyr::select(edited_res.1.5, UMAP_1, UMAP_2) %>%
     dplyr::group_by(edited_res.1.5) %>%
     dplyr::summarise(u1 = median(UMAP_1),
                      u2 = median(UMAP_2)) %>%
     dplyr::rename("label" = 1) %>%
     dplyr::mutate(color = ScExploreR::colors_main_umap)

title = "ScExploreR" # name to display in title bar
header_title <- # logo to display in header bar
    tags$a(href='https://zdglab.iimcb.gov.pl/',
        tags$img(src="zdg_logo.png",
                 height = '100',
                 width = '200',
                 align = "center"),
        target="_blank")

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
                  title = header_title,
                  titleWidth = 220,
                  tags$li(class = "dropdown",
                          shiny::tags$style(".main-header {max-height: 100px}"),
                          shiny::tags$style(".main-header .logo {height: 100px;}"),
                          shiny::tags$style(".sidebar-toggle {height: 100px; padding-top: 20px !important;}"),
                          shiny::tags$style(".navbar {min-height:100px; !important}")
                          )),
  shinydashboard::dashboardSidebar(
                   width = 220,
                   shinydashboard::sidebarMenu(
                     shinydashboard::menuItem("Blank", tabName = "blank", icon = icon("dashboard")),
                     shinydashboard::menuItem("Menu"),
                     shinydashboard::menuItem("Whole heart", tabName = "whole_heart", icon = icon("heart", lib = "glyphicon"),
                              shinydashboard::menuSubItem("Gene table", tabName = "gene_table_all"),
                              shinydashboard::menuSubItem("Visualization", tabName = "visualization_all")),
                     shinydashboard::menuItem("Myocardium", tabName = "myocardium", icon = icon("triangle-top", lib = "glyphicon"),
                              shinydashboard::menuSubItem("Gene table", tabName = "gene_table_myo"),
                              shinydashboard::menuSubItem("Visualization", tabName = "visualization_myo"))
                     )
                   # Adjust the sidebar
                   #tags$style(".left-side, .main-sidebar {padding-top: -20px}")
                   ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem("whole_heart"),
      shinydashboard::tabItem("myocardium"),
      shinydashboard::tabItem("visualization_all",
              shinydashboard::tabPanel("Visualization",
                    shinydashboard::wellPanel(
                       shiny::fluidRow(#column(width = 2),
                                shiny::column(width = 2, offset = 6,
                                       shiny::selectInput(inputId = "type_of_data",
                                                   label = "Type of data",
                                                   choices = c("Overview", "Dataset", "Line",
                                                               "Stage", "Percent.mt", "Percent.hemoglobin",
                                                               "Cell-Cycle-phase", "nCount_RNA", "nFeature_RNA", "iris"),
                                                   selected = "Overview"))
                                ),
                        shiny::fluidRow(shiny::column(width = 5,
                                        shiny::plotOutput("DimPlot", width = "100%")), #, width = 800, height = 600)),
                                 shiny::column(width = 5, offset = 1,
                                        shiny::plotOutput("Multiplot", width = "100%")) # width = 800, height = 600))
                                )
                       ),
                    shiny::wellPanel(
                       shiny::fluidRow(shiny::column(width = 2,
                                       shiny::textInput(inputId = "gene",
                                                 label = "Choose gene",
                                                 value = "myh6",
                                                 placeholder = "myh6",
                                                 width = "100px")),
                                shiny::column(width = 2, offset = 4, style= 'padding-top:0px;',
                                  shiny::textInput(inputId = "gene2",
                                            label = "Choose gene",
                                            value = "myh7l",
                                            placeholder = "myh7l",
                                            width = "100px"))
                                ),
                       shiny::fluidRow(shiny::column(width = 5,
                                       shiny::plotOutput("Featureplot", width = "100%")), # width = 800, height = 600)),
                                shiny::column(width = 5,
                                       offset = 1,
                                       shiny::plotOutput("my_FT", width = "100%")), ## width = 800, height = 600)),
                                style = "background-color: gray99;"
                                )
                       )
                    )
              ),
      shinydashboard::tabItem("gene_table_all",
              shiny::fluidPage(
                shiny::h1("Gene table: Whole heart")),
              shiny::dataTableOutput("markers_all")
              ),
      shinydashboard::tabItem("visualization_myo", "Sub-item 1 tab content"),
      shinydashboard::tabItem("gene_table_myo",
              shiny::fluidPage(
                shiny::h1("Gene table: Myocardium")),
              shiny::dataTableOutput("markers_myo"))
      )
    ),
    title = title
  )


server <- function(input, output) {
  output$markers_all <- shiny::renderDataTable(markers_all, options = list(iDisplayLength = 10))
  output$markers_myo <- shiny::renderDataTable(markers_myo, options = list(iDisplayLength = 10))
   output$Featureplot <-
     shiny::renderPlot({
        my_FeaturePlot(metadata = metadata_all,
                       data_slot = slot_data_all,
                       gene = c(input$gene),
                       identity = "edited_res.1.5",
                       order = FALSE,
                       label = TRUE)
      #FeaturePlot(integrated_all, features = c(input$gene), pt.size = 0.04, order = T) + scale_color_viridis(direction = -1)
      })

  output$my_FT <-
    shiny::renderPlot({
      my_FeaturePlot(metadata = metadata_all,
                     data_slot = slot_data_all,
                     gene = c(input$gene2),
                     identity = "edited_res.1.5",
                     order = FALSE,
                     label = TRUE)
      })

  output$DimPlot <-
    shiny::renderPlot({
      ggplot2::ggplot(metadata_all) +
        ggplot2::geom_point(ggplot2::aes(x = UMAP_1 ,
                       y = UMAP_2,
                       color = edited_res.1.5),
                   alpha = 0.9,
                   size = 0.04) +
        ggplot2::theme_minimal() +
        ScExploreR::one_theme() +
        Seurat::NoLegend() +
        # annotate(geom = "text",
        #          label = labels_all$label  %>% stringr::str_wrap(., width = 10),
        #          x = labels_all$u1,
        #          y = labels_all$u2,
        #          size = 4,
        #          lineheight = 0.8) +
        ggplot2::scale_color_manual(values = ScExploreR::colors_main_umap) +
        ggrepel::geom_label_repel(data = labels_all,
                                  x = labels_all$u1,
                                  y = labels_all$u2,
                                  label = labels_all$label  %>% stringr::str_wrap(., width = 10),
                                  fill = labels_all$color, alpha = 0.5)

    })

  output$Multiplot <-
    shiny::renderPlot({
      if(input$type_of_data == "Overview") {
        raw_ngene_mt(input_metadata = "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")
        }

      else if(input$type_of_data == "DataSet") {

        print("lol")

      } else if(input$type_of_data == "iris") {

        ggplot2::ggplot(data = iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
          ggplot2::geom_point()

      }

    })
}

shiny::shinyApp(ui, server)
