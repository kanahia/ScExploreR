#' Contribution theme
#'
#' @importFrom ggplot2 theme
#'
#' @export
contribution_theme <- function() {
  ggplot2::theme(
    legend.text = element_text(size = 16),
    legend.title = element_blank(), #element_text(size = 18),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 9.5),
    axis.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 22),
    strip.text.y = element_text(size = 16),
    legend.position="top"
  )
}


#' Plot contributions
#'
#' @param main_group 
#' @param feature 
#' @param choice 
#' @param metadata 
#'
#' @export
plot_contribution <- function(metadata,
                              main_group = "edited_res.1.5",
                              feature = c("line", "stage", "DataSet", "iris", "Phase", "Phase_timepoint"),
                              choice){
  if(feature == "DataSet") {
    
    p <-
      metadata %>%
      dplyr::group_by_(main_group, feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                          y = percent, 
                          fill = !! sym(feature))) +
      ggplot2::geom_col() +
        #ggplot2::ggtitle("Dataset contribution to each cluster") +
        labs(x = "", y = "Percent [%]") +
        ggplot2::theme_minimal() +
        contribution_theme() +
        ggplot2::scale_x_discrete(
          labels = c("et31_48h_raw" = "et31 48 hpf",
                     "et31_72h_raw" = "et31 72 hpf",
                     "et33_48h_raw" = "et33 48 hpf",
                     "et33_72h_raw" = "et33 72 hpf"),
          guide = ggplot2::guide_axis(n.dodge = 2)) +
        #guides(color=guide_legend("Sample")) +
        ggplot2::scale_fill_manual(name = stringr::str_to_title(as.character(feature)), #"Dataset",
                                   labels=c("et31 48 hpf", "et31 72 hpf", "et33 48 hpf", "et33 72 hpf"),
                                   values = c("lemonchiffon3", "lightsalmon3",
                                              wesanderson::wes_palette(n = 4, name = "Rushmore1")[c(3)], "#edae49"))
  } else if(feature == "line") {
    
    p <-
      metadata %>%
      dplyr::group_by_(main_group, feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                          y = percent, 
                          fill = !! sym(feature))) +
      ggplot2::geom_col() +
      #ggplot2::ggtitle("Zebrafish line contribution to each cluster") +
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::scale_fill_manual(values = c("palegreen4", "#edae49")) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  } else if (feature == "stage"){
    
    p <- 
      metadata %>%
        dplyr::group_by_(main_group, feature) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::mutate(percent = prop.table(n) * 100) %>%
        ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                            y = percent, 
                            fill = !! sym(feature))) +
        ggplot2::geom_col() +
        #ggplot2::ggtitle("Developmental stage contribution to each cluster") +
        labs(x = "", y = "Percent [%]") +
        ggplot2::theme_minimal() +
        contribution_theme() +
        ggplot2::scale_fill_manual(values = c("lemonchiffon3", "lightsalmon3")) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  } else if(feature == "Overview") {
    p <- 
      ScExploreR::raw_ngene_mt(input_metadata = "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")
  } else if(feature == "iris") {
    
    p <-
      ggplot2::ggplot(data = iris,
                      ggplot2::aes(x = Sepal.Length, 
                                   y = Sepal.Width, 
                                   color = Species)) +
      ggplot2::geom_point()
  } else if (feature == "Phase"){
    
    p <-
      metadata %>%
      dplyr::group_by_(main_group, feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                 y = percent, 
                 fill = !! sym(feature))) + 
      ggplot2::geom_col() +
      #ggtitle("Percentage of cell cycle phases per cluster \n") +
      ggplot2::labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
    
  } else if(feature == "Phase_timepoint") {
    
    features <- c("stage", "Phase")
    p <-
      metadata %>%
      dplyr::group_by_(main_group, "stage", feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                          y = percent, 
                          fill = !! sym(feature))) + 
      ggplot2::geom_col() +
      #ggtitle("Percentage of cell cycle phases per cluster \n") +
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::facet_grid(stage ~., scales="free_y") +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  }
      
  return(p)
  
}

#' MultiPlot_UI
#'
#' @param id 
#' @param label 
#' @param selected 
#' @param choices 
#' 
#' @importFrom shiny selectInput NS
#'
#' @export
MultiPlot_UI <- function(id,
                         label = "Metadata:",
                         selected = "Overview",
                         choices =  c(
                           "Overview",
                           "Dataset" = "DataSet",
                           "Line" = "line",
                           "Stage" = "stage",
                           "Percent.mt",
                           "Percent.hemoglobin",
                           "Cell cycle" = "Phase",
                           "Cell cycle-timepoint" = "Phase_timepoint",
                           "nCount_RNA",
                           "nFeature_RNA",
                           "Iris" = "iris")
                         ) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::selectInput(
      inputId = ns("type_of_data"),
      label = label,
      choices = choices,
      selected = selected,
      width = '180px'),
      shinycustomloader::withLoader(type = "html",
                                loader = "dnaspin",
                                shiny::plotOutput(ns("Multiplot"), width = "85%", height = "500"))
    )
}


#' MultiPlot_Shiny
#'
#' @param id 
#'
#' @export
MultiPlot_Shiny <- function(id, feature = NULL, metadata = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$Multiplot <- shiny::renderPlot({
        ScExploreR::plot_contribution(feature = input$type_of_data,
                          metadata = metadata)
      })
    }
  )
}

