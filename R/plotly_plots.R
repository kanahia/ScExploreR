
#' Violin_plotly
#'
#' @param metadata 
#' @param cluster 
#'
#' @export
#'
violin_plotly <- function(metadata = metadata_all,
                          CLUSTERS = NULL) {
  
  df <- 
    metadata %>%
    dplyr::filter(edited_res.1.5 %in% CLUSTERS) %>%
    dplyr::select(edited_res.1.5, nFeature_RNA, nCount_RNA, percent.mt, stage) %>% 
    tidyr::gather("metadata", "value", -c(stage, edited_res.1.5))
  
  plotly_list <-
    vector("list", length(unique(df$metadata))) %>% 
    setNames(unique(df$metadata))
  
  for(i in 1:length(plotly_list)){
    
    
    plotly_list[[i]] <-
      plotly_build(
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
            x = ~edited_res.1.5[df$stage == '48h'],
            y = ~value[df$stage == '48h'],
            legendgroup = '48h',
            scalegroup = '48h',
            name = '48h',
            side = 'negative',
            box = list(
              visible = T
            ),
            meanline = list(
              visible = T
            ),
            color = I("#1B9E77"),
            showlegend = F
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
              title = ""  
            ),
            yaxis = list(
              title = "",
              zeroline = F
            ),
            violingap = 10,
            violingroupgap = 10,
            violinmode = 'overlay'
          ) %>%
          layout(title = unique(df$metadata)[i])
      )
    
  }
  
  return(plotly_list)
}
