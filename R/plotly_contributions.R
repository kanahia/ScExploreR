
# number of cells in cluster ----------------------------------------------

#' ClusterComposition
#'
#' @param data metadata to be used
#' @param clustering clustering column used for seurat clusters
#' @param condition which data should be used for plotting
#'
#' @return plotly object
#' @export
#'
#' @examples 
#' ClusterComposition(data = metadata_all, clustering = "edited_res.1.5, condition = "stage)
#' @import dplyr plotly
#' 
ClusterComposition <- function(data,
                               clustering = "edited_res.1.5",
                               condition = "stage") {
  toSetOrder <- 
    data %>% 
    dplyr::group_by(!!sym(clustering), stage) %>% 
    dplyr::summarise(n_cells = n()) %>%
    dplyr::mutate(percent = round(prop.table(n_cells) * 100, digits = 0)) %>%
    dplyr::rename(cluster = 1)
  
  order_cells <- toSetOrder %>% dplyr::filter(stage == "48h") %>% dplyr::arrange(percent) %>% pull(cluster)
  order_cells <- factor(order_cells, 
                        levels = levels(order_cells)[base::pmatch(order_cells, levels(data[[clustering]]))])
  
  if(condition == "stage") {
    label <- "Stage"
  } else if (condition=="DataSet"){
    label <- "Dataset"
  } else {
    label <- condition
  }
  
  n_cell <-
    data %>%
    dplyr::mutate(DataSet = gsub("_", " ", gsub("_raw", "", DataSet))) %>%
    dplyr::group_by(!!sym(clustering), !!sym(condition)) %>%
    dplyr::summarise(n_cells = n()) %>%
    dplyr::mutate(percent = prop.table(n_cells) * 100) %>%
    dplyr::rename(cluster = 1) %>%
    dplyr::mutate(cluster = factor(x = cluster, levels(data[[clustering]]))) %>%
    dplyr::arrange(percent)

  
  n_cell$cluster <- factor(n_cell$cluster, levels = levels(order_cells))
  
  p.cluster_composition <-
      plotly::plot_ly(data = n_cell,
                      type = "bar",
                      orientation = "h",
                      name = as.formula(paste0("~", condition)),
                      x = ~percent,
                      y = ~cluster,
                      color = as.formula(paste0("~", condition)),
                      colors = rev(c("#0B775E", "#DC863B")),
                      alpha = 0.5,
                      text = ~n_cells,
                      textposition = "inside",
                      insidetextanchor = "middle",
                      insidetextfont = list(color = "#000000"),
                      hoverinfo = "text",
                      hovertext = paste(n_cell[["cluster"]],
                                        "<br> Cells:", n_cell[["n_cells"]],
                                        "<br>", paste0("Percent : ", round(n_cell[["percent"]], digits = 2), "%"),
                                        "<br>", paste0(label, ": ", n_cell[[condition]])
                                        )
                      ) %>%
      plotly::layout(barmode = "relative",
                     xaxis = list(title = 
                                    list(text = "Percent [%]", 
                                         font = list(size = 15)),
                                  ticksuffix = " %",
                                  type = as.formula(paste0("~", condition))),
                     yaxis = list(title = NA),
                     showlegend = TRUE,
                     legend = list(font = list(size = 16), x = 0.95),
                     hoverlabel = list(font = list(size = 14))) %>%
      plotly::layout(
        xaxis = list(tickfont = list(size = 13)),
        yaxis = list(tickfont = list(size = 13)),
        uniformtext=list(minsize=12, mode='show'),
        margin = list(r=1))
  
  return(p.cluster_composition)
  
}



