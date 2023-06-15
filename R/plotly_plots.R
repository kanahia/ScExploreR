

# Violin plot plotly ------------------------------------------------------

#' Violin plot plotly
#'
#' @param metadata 
#' @param CLUSTERS 
#'
#' @import plotly
#' @import dplyr
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
            hoveron = "violins",
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
            hoveron = "violins",
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


# Stacked plotly bar plot -------------------------------------------------

#' Stacked plotly bar plot
#'
#' @param metadata metadata
#' @param feature feature
#' @param main_group metric
#' 
#' @import plotly
#' @import dplyr
#'
#' @return plotly object
#' @export
#'
stacked_bar_plotly <- function(metadata = metadata_all,
                               feature,
                               main_group = "edited_res.1.5"){
  
  if(feature == "DataSet") {
    df <-
      get_data_metrics(metadata = metadata,
                       feature = feature,
                       main_group = main_group) %>%
      dplyr::mutate(
        DataSet = 
          dplyr::case_when(
            DataSet == "et31_48h_raw" ~ "48h rep 1",
            DataSet == "et33_48h_raw" ~ "48h rep 2",
            DataSet == "et31_72h_raw" ~ "72h rep 1",
            DataSet == "et33_72h_raw" ~ "72h rep 2")
        )
    
    my_colors <-
      c("lightsalmon3",
        wesanderson::wes_palette(n = 4, name = "Rushmore1")[c(3)],
        "lemonchiffon3",
        "#edae49")
    
  } else {
    df <-
      get_data_metrics(metadata = metadata,
                       feature = feature,
                       main_group = main_group)
    if(feature == "line") {
      my_colors <-
        c(wesanderson::wes_palette(n = 4, name = "Rushmore1")[c(3)], 
          "#edae49")
      
    } else if(feature == "stage") {
      my_colors <- c("lemonchiffon3", "lightsalmon3")
    } else if(feature == "Phase"){
      my_colors <- c("#ee6a5b", 
                     "#edae49",
                     wesanderson::wes_palette(n = 4, name = "Rushmore1")[c(3)]
                     )
    }
      
  } 
  
  plot <- 
    df %>% 
    plot_ly(
      x = ~ edited_res.1.5, 
      y = ~ round(percent, digits = 2),
      text = ~ n,
      color = ~ df[[feature]],
      colors = my_colors,
      opacity = rep(0.7, times = length(my_colors)),
      type = 'bar',
      hoverinfo = "text",
      hovertext = paste(.$edited_res.1.5,
                        "<br>",
                        paste0("Percent : ", round(.$percent, digits = 2), " %"),
                        "<br> Count :", .$n)
      ) %>% 
    layout(
      yaxis = list(title = ''),
      xaxis = list(title= ""),
      barmode = 'stack',
      hovermode = 'x',
      legend = list(traceorder = "normal"),
      annotations =
        list(
          list(
            x = -0.09 ,
            y = 0.5,
            text = "Percent %",
            font = list(color = "black", size = 18),
            textangle = 270,
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            size = 48
          ))
      )
    
  
  return(plot)
}


# Violin plot between one metadata split by stage -------------------------



#' Violin plot between one metadata split by stage
#'
#' @param metadata metadata
#' @param feature feature
#' @param main_group metric
#' @param split_by split plot by 
#' 
#' @import plotly
#' @import dplyr
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
          violinmode = 'overlay',
          hovermode = 'x'
        ) 
      
    )
  
  return(p)
}


# plotly_cc_stage ---------------------------------------------------------

#' Cell-cycle bar chart in plotly
#'
#' @param data metadata
#' @param identity main clustering group
#' @param stage condition for grouping
#' @param feature condition to plot
#' @param my_colors colors
#' 
#' @import dplyr
#' @import plotly
#'
#' @return 
#' @export
#'
plotly_cc_stage <- function(data = metadata_all,
                            identity = "edited_res.1.5",
                            stage = "stage",
                            feature = "Phase",
                            my_colors = rev(c("#0B775E", "#DC863B"))#c("#ff7f0eeb", "#2ca02ca6", "#d62728d4")
                            ){
  
  make_plotly <- function(timepoint) {
    dfA <-
      data %>%
      dplyr::group_by_(identity, stage, feature) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(percent = prop.table(n) * 100) %>%
      dplyr::filter(stage == timepoint)
    
    with(data = dfA, {
      p <-
        plotly::plot_ly(x = ~dfA[[identity]],
                        y = ~percent,
                        color = ~as.factor(dfA[[feature]]),
                        colors = my_colors,
                        #opacity = c(0.7, 0.7, 0.7),
                        type = "bar",
                        alpha = 0.5,
                        legendgroup= ~ dfA[[feature]],
                        hoverinfo = "text",
                        hovertext = paste(dfA[[identity]],
                                          "<br>",
                                          paste0("Stage: ", dfA[[feature]]),
                                          "<br>",
                                          paste0("Percent: ", round(percent, digits = 2), " %")
                                          )
                        ) %>%
        plotly::layout(barmode = "stack",
                       yaxis = list(title = 'Percent (%)'),
                       xaxis = list(title= "")
                       )
    })
  }
  
  A <- make_plotly("48h")
  
  B <- make_plotly("72h")
  
  plotly::subplot(
    style(A, showlegend = FALSE),
    #A,
    B,
    nrows = 2,
    shareX = TRUE) %>%
    plotly::layout(annotations =
                     list(
                       list(
                         x = -0.09 ,
                         y = 0.5,
                         text = "Percent %",
                         font = list(color = "black", size = 18),
                         textangle = 270,
                         showarrow = F,
                         xref = 'paper',
                         yref = 'paper',
                         size = 48
                       )
                     ),
                   xaxis= list(title = ""),
                   hovermode = 'x',
                   legend = 
                     list(title = list(text='Phase')),
                   title = 
                     list(title = list(text='Phase'))
                   ) %>%
    plotly::layout(annotations =
                     list(
                       list(
                         x = c(1.03),
                         y = c(0.78),
                         text = c("48h"),
                         font = list(color = "black", size = 18),
                         textangle = -270,
                         showarrow = F,
                         xref = 'paper',
                         yref = 'paper',
                         size = 30
                       )
                     )) %>%
    plotly::layout(annotations =
                     list(
                       list(
                         x = c(1.03),
                         y = c(0.23),
                         text = c("72h"),
                         font = list(color = "black", size = 18),
                         textangle = -270,
                         showarrow = F,
                         xref = 'paper',
                         yref = 'paper',
                         size = 30
                       )
                     ))
}



#' ViolinGeneExpStage
#'
#' @param gene gene of interests
#' @param cluster cluste rof interests
#' @param clustering metadata column with cluster names 
#' @param metadata metadata
#' @param slot_data slot data(SCT here)
#' @param only_boxplot plot boxplot or violin
#'
#' @return plotly plot
#' @import plotly dplyr ggplot2
#' @export
#'
ViolinGeneExpStage <- function(gene = NULL,
                               cluster = NULL,
                               clustering,
                               metadata,
                               slot_data,
                               only_boxplot = FALSE
                               ) {

  chosen_cells <- 
    names(slot_data[gene, ][metadata$cell[metadata[[clustering]] == cluster]])
  stage <- metadata$stage[metadata$cell %in% chosen_cells]
    
  data <-
    data.frame(
      "cells" = chosen_cells,
      "normalized_counts" = slot_data[gene, ][chosen_cells],
      "stage" = stage,
      "cluster" = cluster
      )
  
  data <- data[data$normalized_counts != 0, ]
  data$normalized_counts <- round(data$normalized_counts, digits = 2)
  
  if(only_boxplot) {
  #plot
  fig <- 
    data %>%
    plotly::plot_ly(type = 'violin')
  
  fig <- fig %>%
    add_trace(
      x = ~cluster[data$stage == '48h'],
      y = ~normalized_counts[data$stage == '48h'],
      legendgroup = '48h',
      scalegroup = '48h',
      name = '48h',
      side = 'negative',
      opacity = 0.4,
      box = 
        list(visible = T),
      meanline = 
        list(visible = T),
      color = I("#1B9E77")
    ) 
  
  fig <- fig %>%
    add_trace(
      x = ~cluster[data$stage == '72h'],
      y = ~normalized_counts[data$stage == '72h'],
      legendgroup = '72h',
      scalegroup = '72h',
      name = '72h',
      side = 'positive',
      opacity = 0.9,
      box = 
        list(visible = T),
      meanline = 
        list(visible = T),
      color = I("#D95F02")
    )
  
  fig <- fig %>%
    layout(
      xaxis = list(
        title = "" 
        ),
      yaxis = 
        list(
          title = "normalized counts",
          zeroline = F
        ),
      violingap = 10,
      violingroupgap = 10,
      violinmode = 'overlay'
    ) 
  } else {
    
    # fig <-  
    #   plot_ly(
    #     data = data,
    #     y = ~normalized_counts,
    #     x = ~stage,
    #     type = "box",
    #     color = ~stage,
    #     showlegend = FALSE
    #   )
    
    fig <- 
      ggplot2::ggplot(data = data %>%
                        dplyr::mutate(`normalized counts` = round(normalized_counts, digits = 2)), 
                      ggplot2::aes(x=stage, 
                                   y= `normalized counts`, 
                                   fill = stage)) + 
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() +
      ggplot2::ylab("normalized counts")+
      ggplot2::ggtitle(gene)+
      ggplot2::theme(axis.title = element_text(size = 14),
                     axis.title.x = element_blank(),
                     axis.text = element_text(size = 14),
                     legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14),
                     plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
      ggplot2::scale_fill_brewer(palette="Dark2") +
      ggplot2::geom_jitter(width = 0.25)
    
    fig <- plotly::ggplotly(fig)
  }
  
  
  return(fig)
  
}







