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

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    shiny::tags$li(
      class = "dropdown",
      shiny::tags$style(".main-header {max-height: 100px}"),
      shiny::tags$style(".main-header .logo {height: 100px;}"),
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
        shinydashboard::menuSubItem("Visualization", tabName = "visualization_all")
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
      shinydashboard::tabItem("whole_heart"),
      shinydashboard::tabItem("myocardium"),
      shinydashboard::tabItem(
        "visualization_all",
        shiny::tabPanel(
          "Visualization",
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                width = 2,
                offset = 6,
                shiny::selectInput(
                  inputId = "type_of_data",
                  label = "Type of data",
                  choices = c(
                    "Overview",
                    "Dataset",
                    "Line",
                    "Stage",
                    "Percent.mt",
                    "Percent.hemoglobin",
                    "Cell-Cycle-phase",
                    "nCount_RNA",
                    "nFeature_RNA",
                    "iris"
                  ),
                  selected = "Overview"
                )
            )),
            shiny::fluidRow(
              shiny::column(width = 5,
                            shiny::plotOutput("DimPlot", width = "100%")),
              shiny::column(
                width = 5,
                offset = 1,
                shiny::plotOutput("Multiplot", width = "100%")
              )
            )
          ),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                width = 2,
                shiny::textInput(
                  inputId = "gene",
                  label = "Choose gene",
                  value = "myh6",
                  placeholder = "myh6",
                  width = "100px"
                )
              ),
              shiny::column(
                width = 2,
                offset = 4,
                style = 'padding-top:0px;',
                shiny::textInput(
                  inputId = "gene2",
                  label = "Choose gene",
                  value = "myh7l",
                  placeholder = "myh7l",
                  width = "100px"
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 5,
                shiny::plotOutput("Featureplot", width = "100%")),
              shiny::column(
                width = 5,
                offset = 1,
                shiny::plotOutput("my_FT", width = "100%")
              ),
              style = "background-color: gray99;"
            )
          )
        )
      ),
      shinydashboard::tabItem(
        "gene_table_all",
        shiny::fluidPage(shiny::h1("Gene table: Whole heart")),
        shiny::dataTableOutput("markers_all")
      ),
      shinydashboard::tabItem(
        "visualization_myo",
        shiny::fluidRow(
            shiny::column(
                offset = 1,
                width = 5,
                ScExploreR::FeaturePlotShinyUI("myo_featureplot_1",
                                               value = "myh6",
                                               placeholder = "myh6")
            ),
            shiny::column(
                width = 5,
                offset = 1,
                ScExploreR::FeaturePlotShinyUI("myo_featureplot_2",
                                               value = "myh7",
                                               placeholder = "myh7")
            )
        )
      ),
      shinydashboard::tabItem(
        "gene_table_myo",
        shiny::fluidPage(shiny::h1("Gene table: Myocardium")),
        shiny::dataTableOutput("markers_myo")
      )
    )
  ),
  title = title
)


server <- function(input, output, session) {

  output$markers_all <-
    shiny::renderDataTable(markers_all, options = list(pageLength = 10))

  output$markers_myo <-
    shiny::renderDataTable(markers_myo, options = list(pageLength = 10))

  output$Featureplot <-
    shiny::renderPlot({
      my_FeaturePlot(
        metadata = metadata_all,
        data_slot = slot_data_all,
        gene = input$gene,
        identity = "edited_res.1.5",
        order = FALSE,
        label = TRUE
      )
    })

  output$my_FT <-
    shiny::renderPlot({
      my_FeaturePlot(
        metadata = metadata_all,
        data_slot = slot_data_all,
        gene = c(input$gene2),
        identity = "edited_res.1.5",
        order = FALSE,
        label = TRUE
      )
    })

  output$DimPlot <-
    shiny::renderPlot({
      ggplot2::ggplot(metadata_all) +
      ggplot2::geom_point(ggplot2::aes(
        x = UMAP_1,
        y = UMAP_2,
        color = edited_res.1.5),
        alpha = 0.9,
        size = 0.04
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
        alpha = 0.5
      )
    })

  output$Multiplot <-
    shiny::renderPlot({
      if (input$type_of_data == "Overview") {
        raw_ngene_mt(input_metadata = "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")
      }
      else if (input$type_of_data == "DataSet") {
        print("lol")
      } else if (input$type_of_data == "iris") {
        ggplot2::ggplot(data = iris,
                        ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
          ggplot2::geom_point()
      }
    })

  ScExploreR::FeaturePlotShiny(
                   id = "myo_featureplot_1",
                   metadata = metadata_all,
                   data_slot = slot_data_all,
                   identity = "edited_res.1.5")
  ScExploreR::FeaturePlotShiny(
                   id = "myo_featureplot_2",
                   metadata = metadata_all,
                   data_slot = slot_data_all,
                   identity = "edited_res.1.5")
}

shiny::shinyApp(ui, server)
