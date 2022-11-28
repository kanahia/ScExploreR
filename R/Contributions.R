#' Contribution theme
#'
#' @return
#' @importFrom ggplot2 theme
#'
#' @export
contribution_theme <- function() {
  ggplot2::theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 9.5),
    axis.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 22),
    strip.text.y = element_text(size = 16)
  )
}


#' Plot contributions
#'
#' @param metadata 
#' @param group1 
#' @param group2 
#'
#' @return
#' @export
#'
#' @examples
plot_contribution <- function(metadata, main_group, feature){
  
  if(feature == "DataSet") {
    
    p <-
      metadata %>%
      dplyr::group_by_(main_group, feature) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                          y = percent, 
                          fill = !! sym(feature))) +
      ggplot2::geom_col() +
        ggplot2::ggtitle("Dataset contribution to each cluster  \n") +
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
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                          y = percent, 
                          fill = !! sym(feature))) +
      ggplot2::geom_col() +
      ggplot2::ggtitle("Zebrafish line contribution to each cluster  \n") +
      labs(x = "", y = "Percent [%]") +
      ggplot2::theme_minimal() +
      contribution_theme() +
      ggplot2::scale_fill_manual(values = c("palegreen4", "#edae49")) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  } else {
    
    p <- 
      metadata %>%
        dplyr::group_by_(main_group, feature) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(percent = prop.table(n) * 100) %>%
        ggplot2::ggplot(aes(x = stringr::str_wrap(!! sym(main_group), width = 5), 
                            y = percent, 
                            fill = !! sym(feature))) +
        ggplot2::geom_col() +
        ggplot2::ggtitle("Developmental stage contribution to each cluster  \n") +
        labs(x = "", y = "Percent [%]") +
        ggplot2::theme_minimal() +
        contribution_theme() +
        ggplot2::scale_fill_manual(values = c("lemonchiffon3", "lightsalmon3")) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
    }
      
  return(p)
  
}

