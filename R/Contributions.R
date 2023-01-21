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

#' metadata_prop
#'
#' @param metadata metadata
#' @param main_group clustering based on which metadata
#' @param feature feature to use
#' @param stage needed for cell cycle plot across stage
#'
#' @return ggplot barplot
#' @export
metadata_prop <- function(metadata,
                          main_group,
                          feature, 
                          stage = NULL,
                          arg_geom_col = ggplot2::geom_col) {
  if(is.null(stage)) {
    metadata_p <-
        metadata %>%
          dplyr::group_by_(main_group, feature) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::mutate(percent = prop.table(n) * 100) %>%
          ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                              y = percent, 
                              fill = !! sym(feature))) +
          arg_geom_col()
      
    
  } else {
    metadata_p <-
        metadata %>%
          dplyr::group_by_(main_group, stage, feature) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::mutate(percent = prop.table(n) * 100) %>%
          ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                              y = percent, 
                              fill = !! sym(feature))) +
          arg_geom_col()
      
  }
 
  
  return(metadata_p)
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
                              feature = c("line", "stage", "DataSet", "iris", "Phase", "Phase_timepoint", "nFeature_RNA", "percent.mt"),
                              choice){
  if(feature == "DataSet") {
    
    p <- 
      metadata_prop(metadata, main_group, feature) +
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
      metadata_prop(metadata, main_group, feature) +
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::scale_fill_manual(values = c("palegreen4", "#edae49")) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  } else if (feature == "stage"){
    
    p <- 
      metadata_prop(metadata, main_group, feature) +
        labs(x = "", y = "Percent [%]") +
        ggplot2::theme_minimal() +
        contribution_theme() +
        ggplot2::scale_fill_manual(values = c("lemonchiffon3", "lightsalmon3")) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  } else if(feature == "Overview") {
    p <- 
      ScExploreR::raw_ngene_mt(input_metadata = "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")

  } else if (feature == "Phase"){
    
    p <-
      metadata_prop(metadata, main_group, feature) +
      ggplot2::labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
    
  } else if(feature == "Phase_timepoint") {
    
    features <- c("stage", "Phase")
    p <-
      metadata_prop(metadata, main_group, feature, stage = "stage") +
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::facet_grid(stage ~., scales="free_y") +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
    
  } else if(feature == "nFeature_RNA") {
    
    metadata_nGenes <- metadata %>% dplyr::mutate(`n Genes` = nFeature_RNA)
    p <-
      ggplot2::ggplot(data = metadata_nGenes) + 
      ggplot2::geom_point(data = metadata_nGenes %>% dplyr::filter(`n Genes` < 1000),
                          aes(x = UMAP_1, 
                              y = UMAP_2,
                              color = `n Genes`), 
                          size = 0.5) + 
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::geom_point(data = metadata_nGenes %>% dplyr::filter(`n Genes` > 1000),
                          aes(x = UMAP_1, 
                              y = UMAP_2,
                              color = `n Genes`),
                          size = 0.5) + 
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                      size = ggplot2::guide_legend(title.position="top", title.hjust = 0.5)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        #plot.margin=unit(c(-1, -0.5, -1, -0.5), units="line"),
        #legend.margin=unit(-1,"lines")
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.position = "top",
        legend.title.align=0.5,
        legend.key.width= unit(2.2, 'cm'),
        legend.key.height = unit(0.3, 'cm')
        
      ) +
      # #labs(color = "Library size (log10 scale)") +
      ggplot2::guides(fill =  ggplot2::guide_legend(
        title = "n Genes",
        title.position = "top",
        title.hjust = 0.5,
        title.vjust = 0.5)) +
      ggplot2::scale_alpha_manual(values=c(0.4),guide=F) +
      viridis::scale_color_viridis(direction = -1)
    
  } else if(feature == "log10_UMI") {
    
    metadata_nUMI <- metadata %>% dplyr::mutate(`log10(UMI)` = log10_UMI)
    
    p <-
      ggplot2::ggplot(data = metadata_nUMI) + 
      ggplot2::geom_point(data = metadata_nUMI %>% dplyr::filter(`log10(UMI)` < 3),
                          aes(x = UMAP_1, 
                              y = UMAP_2,
                              color = `log10(UMI)`), 
                          size = 0.5) + 
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::geom_point(data = metadata_nUMI %>% dplyr::filter(`log10(UMI)` > 3),
                          aes(x = UMAP_1, 
                              y = UMAP_2,
                              color = `log10(UMI)`), 
                          size = 0.5) + 
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                      size = ggplot2::guide_legend(title.position="top", title.hjust = 0.5)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        #plot.margin=unit(c(-1, -0.5, -1, -0.5), units="line"),
        #legend.margin=unit(-1,"lines")
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.position = "top",
        legend.title.align=0.5,
        legend.key.width= unit(2.2, 'cm'),
        legend.key.height = unit(0.3, 'cm')
        
      ) + 
      # #labs(color = "Library size (log10 scale)") +
      ggplot2::guides(fill =  ggplot2::guide_legend(
        title = "n UMI",
        title.position = "top",
        title.hjust = 0.5,
        title.vjust = 0.5)) +
      ggplot2::scale_alpha_manual(values=c(0.4),guide=F) +
      viridis::scale_color_viridis(direction = -1)
  } else if (feature == "percent.mt") {
    
    p <-
      metadata %>%
      dplyr::group_by_(main_group, feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                 y = percent.mt, 
                 fill = !!sym(main_group))) + 
      ggplot2::geom_violin(trim=FALSE)+
      ggplot2::geom_boxplot(width=0.1, fill="white")+
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11.2),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 22),
        strip.text.y = element_text(size = 16)
      ) +
      ggplot2::scale_x_discrete(guide =  ggplot2::guide_axis(n.dodge = 2)) +
      Seurat::NoLegend()
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
                           "Stage" = "stage",
                           "Line" = "line",
                           "Number of genes" = "nFeature_RNA",
                           "Number of UMI" = "log10_UMI",
                           "Percent mt" = "percent.mt",
                           "Cell cycle" = "Phase",
                           "Cell cycle-timepoint" = "Phase_timepoint")
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
                                    shiny::plotOutput(ns("Multiplot"), width = "85%", height = "520"))
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

