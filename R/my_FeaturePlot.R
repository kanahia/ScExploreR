#' ScExploreR ggplot2 theme
#' TODO
#'
#' @importFrom ggplot2 element_blank element_text
#'
#' @export
one_theme <- function() {
  theme(axis.text.x = ggplot2::element_text(size = 16),
        axis.text.y = ggplot2::element_text(size = 16),
        axis.title.x = ggplot2::element_text(size = 16),
        axis.title.y = ggplot2::element_text(size = 16),
        legend.text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA))
}

#' Custom FeaturePlot
#'
#' @param metadata metadata exported from seurat
#' @param data_slot slot "data" from seurat
#' @param gene gene to be plotted
#' @param identitty identity of interest (Idents(seurat))
#' @param label to plot labels or not
#' @param order whether sort plotting order or not
#'
#' @importFrom dplyr rename group_by left_join inner_join select summarise
#' @importFrom ggplot2 aes ggplot geom_point theme_classic
#' @import Matrix
#' @importFrom rlang sym
#' @importFrom tibble rownames_to_column
#'
#' @export
my_FeaturePlot <- function(metadata,
                           data_slot,
                           gene,
                           identity,
                           label = FALSE,
                           order = FALSE) {
  
  metadata <- metadata
  # check if identity is a string
  stopifnot(is.character(identity) & identity %in% colnames(metadata))
  
  #filter matrix
  m_data <- data_slot
  m_gene <- m_data[gene, ]
  m_gene_cell <-
    m_gene[m_gene > 0] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::rename("data" =2) %>%
    dplyr::inner_join(.,
                      metadata,
                      by = "cell")
  
  filtered_data_positive <-
    as.data.frame(m_gene[m_gene > 0]) %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::rename("slot_data" = 2)
  
  metadata <- metadata %>%
    dplyr::left_join(., filtered_data_positive,
                     by = c("cell" = "cell"))
  
  metadata$slot_data <- ifelse(is.na(metadata$slot_data), 0, metadata$slot_data)
  
  # get colors for plotting
  metadata$color <- ifelse(metadata$cell %in% m_gene_cell$cell, "positive", "negative")
  metadata$color <- factor(metadata$color, levels = c("positive", "negative"), ordered = TRUE)
  
  #coorinates for labels
  position_label <-
    metadata %>%
    dplyr::select(!!rlang::sym(identity), UMAP_1, UMAP_2) %>%
    dplyr::group_by(!!rlang::sym(identity)) %>%
    dplyr::summarise(
      u1 = median(UMAP_1),
      u2 = median(UMAP_2)) %>%
    as.data.frame()
  
  FT.plot <-
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = metadata[metadata$color == "negative", ],
      ggplot2::aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    ggplot2::theme_minimal() +
    ScExploreR::one_theme() +
    ggplot2::geom_point(
      data = metadata[metadata$color != "negative", ],
      ggplot2::aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
    ggplot2::ggtitle(gene) +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme()
  
  if(order == TRUE) {
    FT.plot <-
      ggplot2::ggplot() +
      ggplot2::geom_point(
        data = metadata[metadata$color == "negative", ],
        ggplot2::aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme() +
      ggplot2::geom_point(
        data = metadata[metadata$color != "negative", ],
        ggplot2::aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
      ggplot2::ggtitle(gene) +
        ggplot2::theme_minimal() +
        ScExploreR::one_theme()
  } else {
    set.seed(42)
    shuffled_metadata <- metadata[sample(1:nrow(metadata)), ]
    FT.plot <-
      ggplot2::ggplot() +
      ggplot2::geom_point(data = shuffled_metadata,
                          ggplot2::aes(x = UMAP_1 , y = UMAP_2, color = slot_data, fill = color), alpha = 0.9, size = 0.04) +
      ggplot2::ggtitle(gene) +
      ggplot2::scale_fill_discrete(guide = "none") +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme()
      
  }
  
  # plot umap
  if(label == TRUE) {
    FT.plot <-
      FT.plot +
      ggplot2::annotate(geom = "text",
                        label = position_label[[identity]],
                        x = position_label$u1,
                        y = position_label$u2,
                        size = 4.5) +
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme()
  } else {
    FT.plot <-
      FT.plot +
      viridis::scale_color_viridis(direction = -1) +
      ggplot2::theme_minimal() +
      ScExploreR::one_theme()
  }
  
  FT.plot <- FT.plot + 
    ggplot2::theme(legend.title=element_blank()) +
    ggplot2::theme_minimal() +
    ScExploreR::one_theme()
  
  return(FT.plot)
}

