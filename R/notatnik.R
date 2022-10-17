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
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
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
    left_join(., filtered_data_positive,
              by = c("cell" = "cell"))

  metadata$slot_data <- ifelse(is.na(metadata$slot_data), 0, metadata$slot_data)

  # get colors for plotting
  metadata$color <- ifelse(metadata$cell %in% m_gene_cell$cell, "positive", "negative")
  metadata$color <- factor(metadata$color, levels = c("positive", "negative"), ordered = TRUE)

  #coorinates for labels
  position_label <-
    metadata %>%
    dplyr::select(!!sym(identity), UMAP_1, UMAP_2) %>%
    group_by(!!sym(identity)) %>%
    summarise(u1 = median(UMAP_1),
              u2 = median(UMAP_2)) %>%
    as.data.frame()

  FT.plot <-
    ggplot() +
    geom_point(data = metadata[metadata$color == "negative", ],
               aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    geom_point(data = metadata[metadata$color != "negative", ],
               aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
    theme_classic() +
    ggtitle(gene) +
    one_theme()

  if(order == TRUE) {
    FT.plot <-
      ggplot() +
      geom_point(data = metadata[metadata$color == "negative", ],
                 aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      geom_point(data = metadata[metadata$color != "negative", ],
                 aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
      theme_classic() +
      ggtitle(gene) +
      one_theme()
    } else {
      set.seed(42)
      shuffled_metadata <- metadata[sample(1:nrow(metadata)), ]
      FT.plot <-
        ggplot() +
        geom_point(data = shuffled_metadata,
                   aes(x = UMAP_1 , y = UMAP_2, color = slot_data, fill = color), alpha = 0.9, size = 0.04) +
        theme_classic() +
        ggtitle(gene) +
        one_theme() +
        scale_fill_discrete(guide = "none")
      }
  # plot umap
  if(label == TRUE) {
    FT.plot <-
      FT.plot +
      annotate(geom = "text",
               label = position_label[[cluster]],
               x = position_label$u1,
               y = position_label$u2,
               size = 4.5) +
      scale_color_viridis(direction = -1)
    } else {
      FT.plot <-
        FT.plot +
        scale_color_viridis(direction = -1)
      }
  return(FT.plot)
}

