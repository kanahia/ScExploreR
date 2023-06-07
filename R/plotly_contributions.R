
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



# horizontal stacked plotly barplots --------------------------------------


#' Title
#'
#' @param data metadata to be used
#' @param clustering clustering column used for seurat clusters
#' @param condition which data should be used for plotting: nCount_RNA, n"Feature_RNA, percent.mt
#'
#' @return plotly plot
#' @export
#'
#' @examples
#' ClusterComposition(data = metadata_all, clustering = "edited_res.1.5, condition = "nCount_RNA")
#' @import dplyr plotly
#' 
StackedMetrics <- function(data,
                           clustering = "edited_res.1.5",
                           condition = c("nCount_RNA", "nFeature_RNA", "percent.mt")) {
  
  stopifnot(length(condition) == 1)
  condition <- condition[which(condition %in% condition)]
  toSetOrder <- 
    data %>% 
    dplyr::group_by(!!sym(clustering), stage) %>% 
    dplyr::summarise(n_cells = n()) %>%
    dplyr::mutate(percent = round(prop.table(n_cells) * 100, digits = 0)) %>%
    dplyr::rename(cluster = 1)
  
  order_cells <- toSetOrder %>% dplyr::filter(stage == "48h") %>% dplyr::arrange(percent) %>% pull(cluster)
  order_cells <- factor(order_cells, 
                        levels = levels(order_cells)[base::pmatch(order_cells, levels(data[[clustering]]))])

  if(condition == "nFeature_RNA") {
    my_limits <- c(-2500, 2000) 
    my_breaks <- seq(-2500, 2000, 1000)
    my_labels <- abs(seq(-2500, 2000, 1000))
    my_colors <- c("#82817e", "#e6e284")
    my_x_label <- "Number of genes"
    } else if(condition == "nCount_RNA") {
      my_limits <- c(-10000, 5000) 
      my_breaks <- seq(-10000, 5000, 2500)
      my_labels <- abs(seq(-10000, 5000, 2500))
      my_colors <- c("#9f996f", "#ebd0bf")
      my_x_label <- "Number of UMI"
      } else if(condition == "percent.mt") {
         my_limits <-  c(-10, 20)
         my_breaks <- seq(-10, 20, 5)
         my_labels <- abs(seq(-10, 20, 5))
         my_colors <- wesanderson::wes_palette("Royal1")
         my_x_label <- "Mitochondrial content [%]"
  }
  
  n_genes_stack <-
    data %>%
    dplyr::group_by(!!sym(clustering), stage) %>%
    summarise(mean = median(!!sym(condition))) %>%
    dplyr::rename(cluster = 1) %>%
    mutate(number = round(prop.table(mean) * 100, digits = 0),
           cluster = factor(x = cluster, levels(order_cells))) %>%
    dplyr::mutate(number = round(ifelse(stage == "48h", -mean, mean), digits = 0))
  
  if(condition %in% c("percent.mt")) {
    colnames(n_genes_stack)[colnames(n_genes_stack) == "number"] = "percent"
    #my_Y <- n_genes_stack[[colnames(n_genes_stack)[4]]]
    text <- paste0("Cluster: ", n_genes_stack$cluster, 
                   paste0("\n", colnames(n_genes_stack)[4], ": ", abs(n_genes_stack[[4]]), "%"),
                   "\nstage: ", n_genes_stack$stage)
  } else {
    text <- paste0("Cluster: ", n_genes_stack$cluster, 
                   paste0("\n", colnames(n_genes_stack)[4], ": ", abs(n_genes_stack[[4]])),
                   "\nstage: ", n_genes_stack$stage)
  }
  
  
  
  #my_Y <- n_genes_stack[[colnames(n_genes_stack)[4]]]
  p.n_genes_stack <-
    ggplot2::ggplot(n_genes_stack, 
                    ggplot2::aes(x = cluster, 
                                 y =
                                  {
                                   if(condition == "percent.mt"){
                                     y = percent
                                     } else if(condition %in% c("nCount_RNA", "nFeature_RNA")) {
                                       y =  number
                                       }
                                    },
                                 fill = stage,
                                 text = text)) + 
    ggplot2::geom_bar(stat = "identity", 
                      position = "identity", 
                      #width = 0.525,
                      alpha = 0.5,
                      size = 0.3,
                      color = "black") + 
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = element_text(size = 15),
                   axis.text.y = element_text(size = 15),  
                   axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   legend.text = element_text(size=12),
                   legend.title = element_text(size=14, face = "bold"),
                   legend.key.size = unit(0.7, 'cm')) +
    ggplot2::ylab(my_x_label) +
    ggplot2::xlab("") +
    ggplot2::scale_fill_manual(values = my_colors) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = my_limits, 
                                breaks = my_breaks,
                                labels = my_labels)
  
  p.n_genes_stack <- plotly::ggplotly(p.n_genes_stack, tooltip = c("text"))
  return(p.n_genes_stack)
  
}

