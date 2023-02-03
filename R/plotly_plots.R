
#' Violin plot plotly
#'
#' @param metadata 
#' @param CLUSTERS 
#'
#' @import plotly
#'
#' @export
#'
violin_plotly <- function(metadata,
                          CLUSTERS) {
  
  df <- 
    metadata %>%
    dplyr::filter(edited_res.1.5 %in% CLUSTERS) %>%
    dplyr::select(edited_res.1.5, nFeature_RNA, nCount_RNA, percent.mt, stage) %>% 
    tidyr::gather("metadata", "value", -c(stage, edited_res.1.5))
  
  plotly_list <-
    vector("list", length(unique(df$metadata))) %>% 
    setNames(unique(df$metadata))
  
  ylabel <- c("Number", "Number", "Percent (%)")
  title_name <- c("Genes", "UMI", "Mt-content")
  
  for(i in 1:length(plotly_list)){
    
    plotly_list[[i]] <-
      plotly::plotly_build(
        df %>%
          dplyr::filter(metadata == unique(metadata)[i]) %>%
          plotly::plot_ly(type = 'violin') %>%
          add_trace(
            p = .,
            x = ~ edited_res.1.5[df$stage == '48h'],
            y = ~ value[df$stage == '48h'],
            legendgroup = '48h',
            scalegroup = '48h',
            name = '48h',
            side = 'negative',
            opacity = 0.4,
            box = list(
              visible = T
            ),
            meanline = list(
              visible = T
            ),
            color = I("#1B9E77")
          ) %>%
          add_trace(
            x = ~edited_res.1.5[df$stage == '72h'],
            y = ~value[df$stage == '72h'],
            legendgroup = '72h',
            scalegroup = '72h',
            name = '72h',
            side = 'positive',
            box = list(
              visible = T
            ),
            meanline = list(
              visible = T
            ),
            color = I("#D95F02")
          ) %>%
          layout(
            xaxis = list(
              title = title_name[i]  
            ),
            yaxis = list(
              title = ylabel[i],
              zeroline = F
            ),
            violingap = 10,
            violingroupgap = 10,
            violinmode = 'overlay'
          ) 
                 
      )
    
  }
  
  return(plotly_list)
}



#' Stacked plotly bar plot
#'
#' @param metadata metadata
#' @param feature feature
#' @param main_group metric
#' 
#' @import plotly dplyr
#'
#' @return plotly object
#' @export
#'
stacked_bar_plotly <- function(metadata = metadata_all,
                               feature,
                               main_group = "edited_res.1.5"){
  
  df <-
    get_data_metrics(metadata = metadata,
                     feature = feature,
                     main_group = main_group)
  
  #x = enquo(feature)
  
  # colors <- list("DataSet" = 
  #                  c("lemonchiffon3", "lightsalmon3",
  #                    wesanderson::wes_palette(n = 4, 
  #                                             name = "Rushmore1")[c(3)], "#edae49"),
  #                "stage" = c("palegreen4", "#edae49"),
  #                "line" = 
  #                    
  #             )
  # if(any(colnames(df) == "DataSet")){
  #   df <-
  #     df %>%
  #     dplyr::mutate(
  #       feature =
  #         dplyr::case_when(
  #           !!sym(feature) == "et31_48h_raw" ~ "48 hpf rep 1",
  #           !!sym(feature) == "et33_48h_raw" ~ "48 hpf rep 2",
  #           !!sym(feature) == "et31_72h_raw" ~ "72 hpf rep 1",
  #           !!sym(feature) == "et33_72h_raw" ~ "72 hpf rep 2",
  #           TRUE ~ 'F'
  #         )
  #     )
  # } else {
  #   df <- df
  # }
    
  plot <- 
    df %>% 
    plot_ly(
      x = ~ edited_res.1.5, 
      y = ~ round(percent, digits = 2),
      text = ~ n,
      color = ~ df[[feature]],
      colors = c("lemonchiffon3", "lightsalmon3",
                 wesanderson::wes_palette(n = 4, name = "Rushmore1")[c(3)], "#edae49"),
      type = 'bar',
      hoverinfo = "text",
      hovertext = paste(.$edited_res.1.5,
                        "<br>",
                        paste0("Percent : ", round(.$percent, digits = 2), " %"),
                        "<br> Count :", .$n)
      ) %>% 
    layout(
      yaxis = list(title = 'Percent (%)'),
      xaxis = list(title= ""),
      barmode = 'stack') %>%
    layout(legend = list(traceorder = "normal"))
    
  
  return(plot)
}



#' Violin plot between one metadata split by stage
#'
#' @param metadata metadata
#' @param feature feature
#' @param main_group metric
#' @param split_by split plot by 
#' 
#' @import plotly dplyr
#'
#' @return plotly object
#' @export
#'
violin_metadata_stage <- function(metadata = metadata_all,
                                  feature,
                                  main_group = "edited_res.1.5",
                                  split_by = "stage") {
  
  df <-
    get_data_metrics(metadata = metadata,
                     feature = feature,
                     main_group = main_group,
                     split_by = split_by)
  
  p <-
    plotly::plotly_build(
      df %>%
        plotly::plot_ly(type = 'violin') %>%
        add_trace(
          p = .,
          x = ~ edited_res.1.5[df$stage == '48h'],
          y = ~ df[[feature]][df$stage == '48h'],
          legendgroup = '48h',
          scalegroup = '48h',
          name = '48h',
          side = 'negative',
          opacity = 0.4,
          box = list(
            visible = T),
          meanline = list(
            visible = T),
          color = I("red" )
        ) %>%
        add_trace(
          x = ~edited_res.1.5[df$stage == '72h'],
          y = ~ df[[feature]][df$stage == '72h'],
          legendgroup = '72h',
          scalegroup = '72h',
          name = '72h',
          side = 'positive',
          box = 
            list(
            visible = T),
          meanline = 
            list(
            visible = T),
          color = I("deepskyblue3") #I("#00A08A")
        ) %>%
        layout(
          xaxis = list(
            title = feature 
          ),
          yaxis = 
            list(
              title = "Percent (%)",
              zeroline = F
              ),
          violingap = 10,
          violingroupgap = 10,
          violinmode = 'overlay'
        ) 
      
    )
  
  return(p)
}
