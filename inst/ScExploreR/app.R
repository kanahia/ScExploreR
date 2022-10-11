library("shiny")
library("shinydashboard")
library("dplyr")
source("/home/jason/data/shiny_dashboard/heart10x/R/read_multiple_sheets.R")
source("/home/jason/data/shiny_dashboard/heart10x/R/raw_ngene_mt.R")
devtools::load_all()
#read data


  ## all_markers
   markers_all <- read_multiple_sheets(file = "/home/jason/data/shiny_dashboard/heart10x/data/main_clustering_markers.xlsx")
   markers_all <- markers_all %>%
     dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
     dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
     dplyr::mutate(p_val = format.pval(p_val, digits = 2))

  ## markers_myo
   markers_myo <- read_multiple_sheets("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Plots/Myocardium/markers_myo_new-updated.xlsx")
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

   colors_main_umap <- c("#76AF00", "#E48800", "#00B92B", "deeppink3", "#C69900", "#5CB300",
                         "#9290FF", "#00BF79", "#E26EF7", "#C27EFF", "#00B8E5", "#FC61D5",
                         "#FF61C6", "#FF66A7", "#8BAB00", "#00BCD9", "#F8766D", "#B99E00")

   #metadata myo
   metadata_myo <- data.table::fread("/home/jason/data/shiny_dashboard/heart10x/data/metadata_myo.csv")

   #slot data
   slot_data_all <- readRDS("/home/jason/data/shiny_dashboard/heart10x/data/slot_data_all.rds")
   slot_data_all <- readRDS("/home/jason/data/shiny_dashboard/heart10x/data/slot_data_myo.rds")

   labels_all <-
     metadata_all %>%
     dplyr::select(edited_res.1.5, UMAP_1, UMAP_2) %>%
     dplyr::group_by(edited_res.1.5) %>%
     summarise(u1 = median(UMAP_1),
               u2 = median(UMAP_2)) %>%
     dplyr::rename("label" = 1) %>%
     dplyr::mutate(color = colors_main_umap)


## Add icon along with the title in the shinydashboard header
title <- tags$a(href='https://zdglab.iimcb.gov.pl/',
                tags$img(src="zdg_logo.png",
                         height = '100',
                         width = '200',
                         align = "center"),
                target="_blank")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = title,
                  titleWidth = 220,
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 100px}"),
                          tags$style(".main-header .logo {height: 100px;}"),
                          tags$style(".sidebar-toggle {height: 100px; padding-top: 20px !important;}"),
                          tags$style(".navbar {min-height:100px; !important}")
                          )),
  dashboardSidebar(width = 220,
                   sidebarMenu(
                     menuItem("Blank", tabName = "blank", icon = icon("dashboard")),
                     menuItem("Menu"),
                     menuItem("Whole heart", tabName = "whole_heart", icon = icon("heart", lib = "glyphicon"),
                              menuSubItem("Gene table", tabName = "gene_table_all"),
                              menuSubItem("Visualization", tabName = "visualization_all")),
                     menuItem("Myocardium", tabName = "myocardium", icon = icon("triangle-top", lib = "glyphicon"),
                              menuSubItem("Gene table", tabName = "gene_table_myo"),
                              menuSubItem("Visualization", tabName = "visualization_myo"))
                     )
                   # Adjust the sidebar
                   #tags$style(".left-side, .main-sidebar {padding-top: -20px}")
                   ),
  dashboardBody(
    tabItems(
      tabItem("whole_heart"),
      tabItem("myocardium"),
      tabItem("visualization_all",
              tabPanel("Visualization",
                    wellPanel(
                       fluidRow(#column(width = 2),
                                column(width = 2, offset = 6,
                                       selectInput(inputId = "type_of_data",
                                                   label = "Type of data",
                                                   choices = c("Overview", "Dataset", "Line",
                                                               "Stage", "Percent.mt", "Percent.hemoglobin",
                                                               "Cell-Cycle-phase", "nCount_RNA", "nFeature_RNA", "iris"),
                                                   selected = "Overview"))
                                ),
                        fluidRow(column(width = 5,
                                        plotOutput("DimPlot", width = "100%")), #, width = 800, height = 600)),
                                 column(width = 5, offset = 1,
                                        plotOutput("Multiplot", width = "100%")) # width = 800, height = 600))
                                )
                       ),
                    wellPanel(
                       fluidRow(column(width = 2,
                                       textInput(inputId = "gene",
                                                 label = "Choose gene",
                                                 value = "myh6",
                                                 placeholder = "myh6",
                                                 width = "100px")),
                                column(width = 2, offset = 4, style= 'padding-top:0px;',
                                  textInput(inputId = "gene2",
                                            label = "Choose gene",
                                            value = "myh7l",
                                            placeholder = "myh7l",
                                            width = "100px"))
                                ),
                       fluidRow(column(width = 5,
                                       plotOutput("Featureplot", width = "100%")), # width = 800, height = 600)),
                                column(width = 5,
                                       offset = 1,
                                       plotOutput("my_FT", width = "100%")), ## width = 800, height = 600)),
                                style = "background-color: gray99;"

                                )
                       )
                    )
              ),
      tabItem("gene_table_all",
              fluidPage(
                h1("Gene table: Whole heart")),
              dataTableOutput("markers_all")
              ),
      tabItem("visualization_myo", "Sub-item 1 tab content"),
      tabItem("gene_table_myo",
              fluidPage(
                h1("Gene table: Myocardium")),
              dataTableOutput("markers_myo"))
      )
    )
  )


server <- function(input, output) {
  output$markers_all <- renderDataTable(markers_all, options = list(iDisplayLength = 10))
  output$markers_myo <- renderDataTable(markers_myo, options = list(iDisplayLength = 10))
   output$Featureplot <-
     renderPlot({
        my_FeaturePlot(metadata = metadata_all,
                       data_slot = slot_data_all,
                       gene = c(input$gene),
                       identity = "edited_res.1.5",
                       order = FALSE,
                       label = TRUE)
      #FeaturePlot(integrated_all, features = c(input$gene), pt.size = 0.04, order = T) + scale_color_viridis(direction = -1)
      })

  output$my_FT <-
    renderPlot({
      my_FeaturePlot(metadata = metadata_all,
                     data_slot = slot_data_all,
                     gene = c(input$gene2),
                     identity = "edited_res.1.5",
                     order = FALSE,
                     label = TRUE)
      })

  output$DimPlot <-
    renderPlot({
      ggplot(metadata_all) +
        geom_point(aes(x = UMAP_1 ,
                       y = UMAP_2,
                       color = edited_res.1.5),
                   alpha = 0.9,
                   size = 0.04) +
        theme_minimal() +
        one_theme +
        NoLegend() +
        # annotate(geom = "text",
        #          label = labels_all$label  %>% stringr::str_wrap(., width = 10),
        #          x = labels_all$u1,
        #          y = labels_all$u2,
        #          size = 4,
        #          lineheight = 0.8) +
        scale_color_manual(values = colors_main_umap) +
        ggrepel::geom_label_repel(data = labels_all,
                                  x = labels_all$u1,
                                  y = labels_all$u2,
                                  label = labels_all$label  %>% stringr::str_wrap(., width = 10),
                                  fill = labels_all$color, alpha = 0.5)

    })

  output$Multiplot <-
    renderPlot({
      if(input$type_of_data == "Overview") {
        raw_ngene_mt(input_metadata = "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")
        }

      else if(input$type_of_data == "DataSet") {

        print("lol")

      } else if(input$type_of_data == "iris") {

        ggplot2::ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
          geom_point()

      }

      })

}

shinyApp(ui, server)
