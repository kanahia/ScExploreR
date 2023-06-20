#' Volcano plot
#' 
#' @param seurat_object seurat object
#' @param ident.1 
#' @param ident.2 
#' @param avg_log2FC.1 
#' @param avg_log2FC.2 
#' @param plot_pval 
#' @param height 
#' @param label.ident.1 
#' @param label.ident.2 
#' @param pos.label.1 
#' @param pos.label.2 
#' @param label.title.size 
#' @param ann_text_size 
#' @param plot_top 
#' @param n_genes 
#' @param point.size 
#' @param alpha 
#' @param markers output from FindMarkers/FindAllMarkers from Seurat
#'
#' importFrom ggplot2 aes
#' @export

volcano_plot <- function(seurat_object,
                         markers = NULL,
                         ident.1 = NULL,
                         ident.2 = NULL,
                         avg_log2FC.1 = NULL,
                         avg_log2FC.2 = NULL,
                         plot_pval = 1e-296,
                         height = 15,
                         label.ident.1 = ident.1,
                         label.ident.2 = ident.2,
                         pos.label.1 = pos_label_1,
                         pos.label.2 = pos_label_2,
                         label.title.size = 5.5,
                         ann_text_size = 4.5,
                         plot_top = FALSE,
                         n_genes = 5,
                         point.size = 3,
                         alpha = 0.6) {
  if(is.null(markers)) {

    markers <-
      Seurat::FindMarkers(
        object = seurat_object,
        ident.1 = ident.1,
        ident.2 = ident.2,
        min.pct = 0.1,
        only.pos = FALSE)
  }
  
  lim_1 <- max(markers$avg_log2FC)
  lim_2 <- min(markers$avg_log2FC)
  
  
  pos_label_1 <- max(markers$avg_log2FC[markers$avg_log2FC > 0])/2 +0.15
  pos_label_2 <- min(markers$avg_log2FC[markers$avg_log2FC < 0])/2 - 0.15

  # if(!is.numeric(c(avg_log2FC.1, avg_log2FC.2))) {
  #   stop("log2FC is not numeric")
  # }

  markers <- markers %>%
    dplyr::mutate(p_val_adj = base::ifelse(p_val_adj == 0e+00, 1e-299, p_val_adj))
  # markers$p_val_adj <-
  #   ifelse(markers$p_val_adj == 0e+00, plot_pval, markers$p_val_adj)

  markers$gene <- rownames(markers)
  markers$color <-
    sapply(
      X = 1:nrow(markers),
      FUN = function(x) {
        if (markers$avg_log2FC[x] > 0.25 & markers$p_val_adj[x] < 0.05) {
          "#F8766D"
        } else if (markers$avg_log2FC[x] < -0.25 & markers$p_val_adj[x] < 0.05) {
          "red"
        } else if (abs(markers$avg_log2FC[x]) > 0.25 & markers$p_val_adj[x] > 0.05) {
          "green"
        } else if (abs(markers$avg_log2FC[x]) < 0.25 & markers$p_val_adj[x] < 0.05)  {
          "blue"
        } else {
          "grey"
        }
      }
    )


  p.volcano <-
    ggplot2::ggplot(markers) +
    ggplot2::geom_point(aes(x = avg_log2FC,
                            y = -log10(p_val_adj),
                            colour = color),
                        size = point.size,
                        alpha = alpha) +
    # ggtitle("", subtitle = " \n\n") +
    ggplot2::xlab("avg log2FC") +
    ggplot2::ylab("-log10 adjusted p-value") +
    ggplot2::theme_minimal() +
    #scale_y_continuous(limits = c(0,50)) +
    #xlim(-3, 4) +
    ggplot2::geom_vline(xintercept = 0, size = 0.2) +
    ggplot2::geom_hline(yintercept = 0, size = 0.2) +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = c(-0.25, 0.25), linetype = "dashed", size = 0.2) +
    ggplot2::scale_colour_manual(
      name = NULL,
      labels = c("green" = "avg log2FC",
                 "red" = "padj & avg log2FC < -0.25",
                 "#F8766D" = "padj & avg log2FC > 0.25",
                 "blue" = "padj",
                 "grey" = "Not sig"),
      values = c("lawngreen", "royalblue", "chocolate3", "seashell4", "lightsalmon1")) +
    ggplot2::theme(
      legend.position = "bottom",
      #plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
      legend.text = element_text(size = 14),
      #c(0.89, 0.55),
      #legend.box.background = element_rect(color = "grey"),
      plot.title = element_text(size = ggplot2::rel(1.5), hjust = -1),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = ggplot2::rel(1.45)),
      axis.text = element_text(size = 15)
    )

  p.volcano.ann <-
    p.volcano +
    ggplot2::annotate(
      geom = "label",
             x = pos.label.1 ,
             y = max(-log10(markers$p_val_adj)) + height,
             label = paste0(as.character(label.ident.1), " \n # of genes: ",
                            nrow(dplyr::filter(markers, p_val_adj < 0.05, avg_log2FC > 0.25) %>%
                                   dplyr::filter(!is.na(avg_log2FC),!is.na(p_val_adj)))),
             color = "black",
             fill = "lawngreen",
             size = label.title.size) +
    ggplot2::annotate(geom = "label",
                      x = pos.label.2 ,
                      y = max(-log10(markers$p_val_adj)) + 
                        height,
                      label = paste0(as.character(label.ident.2), " \n # of genes: ",
                                     nrow(dplyr::filter(markers, p_val_adj < 0.05, avg_log2FC < -0.25) %>%
                                            dplyr::filter(!is.na(avg_log2FC), !is.na(p_val_adj)))),
                      color = "black",
                      fill = "lightsalmon1",
                      size = label.title.size)

  #+coord_fixed()
  if(plot_top == TRUE) {

    t <- markers %>%
      dplyr::mutate(expression = ifelse(avg_log2FC > 0, "up", "down"),
                    gene = rownames(markers)) %>%
      dplyr::filter(p_val_adj < 0.05)

    t2 <-
      c(
        t %>% dplyr::arrange(desc(avg_log2FC)) %>% .[1:n_genes, 6],
        t %>% dplyr::arrange(avg_log2FC) %>% .[1:n_genes, 6]
      )

    p.volcano.ann.final <-
      p.volcano.ann +
      ggrepel::geom_text_repel(
        data = markers,
        mapping = aes(x = avg_log2FC,
                      y = -log10(p_val_adj),
                      label = ifelse(gene %in% t2, gene, "")),
        size = ann_text_size,
        max.overlaps = 100) +
      ggplot2::coord_cartesian(xlim = c(lim_1 + 1, lim_2 -1))
    
  } else {
    
    t <- markers %>%
      dplyr::mutate(expression = ifelse(avg_log2FC > 0, "up", "down"),
                    gene = rownames(markers)) %>%
      dplyr::filter(p_val_adj < 0.05)

    t2 <-
      c(
        t %>% 
          dplyr::filter(avg_log2FC > 0) %>% 
          dplyr::arrange(p_val_adj) %>% 
          .[1:n_genes, 6],
        t %>% 
          dplyr::filter(avg_log2FC < 0) %>% 
          dplyr::arrange(p_val_adj) %>% 
          .[1:n_genes, 6]
      )
    
    p.volcano.ann.final <-
      p.volcano.ann +
      ggrepel::geom_text_repel(data = markers,
                                 # dplyr::filter(markers,
                                 #               avg_log2FC > avg_log2FC.1 | avg_log2FC < avg_log2FC.2),
                               mapping = aes(x = avg_log2FC,
                                             y = -log10(p_val_adj),
                                             label = ifelse(gene %in% t2, gene, "")),
                               size = ann_text_size,
                               max.overlaps = 100) +
      ggplot2::coord_cartesian(xlim = c(lim_1 + 1, lim_2 -1))
    }
  return(p.volcano.ann.final)
}



#' Volcano plot side by side in particular clusters
#'
#'@param markers #positive and negative markers
#'@param seurat #seurat object
#'@param clusters #clusters to be plotted
#'@param n_pos #how many positive genes to annotate
#'@param n_neg #how many negative genes to annotate
#'@param dot_size #size of the dot
#'@param order.by 
#'@param label.size 
#'
volcano_plot_in_cluster <- function(markers = NULL,
                                    seurat,
                                    clusters = NULL,
                                    n_pos = 8,
                                    n_neg = -8,
                                    dot_size = 3,
                                    order.by = c("avg_log2FC", "p_val_adj"),
                                    label.size = 4.5) {

  if(is.null(markers)) {
    markers <-
      FindAllMarkers(
        seurat,
        only.pos = FALSE,
        min.pct = 0.1,
        logfc.threshold = 0.25,
        slot = "data"
      )
  }
  n_zeros <- nrow(markers[markers$p_val_adj == 0e+00, ])
  markers <-
    markers %>%
    dplyr::mutate(p_val_adj = ifelse(p_val_adj == 0e+00, 
                                     runif(n_zeros, max = 10^-280, min = 10^-289), 
                                     p_val_adj)) %>%
    dplyr::filter(cluster %in% clusters) %>%
    {
      if(order.by == "avg_log2FC") {
        dplyr::arrange(. , cluster, desc(avg_log2FC, .by_group=TRUE))
      } else {
        dplyr::arrange(. , cluster, p_val_adj, .by_group=TRUE)
      }
    }

  markers$sig <- ifelse(markers$p_val_adj < 0.05, "sig", "non-sig")
  markers$trend <- ifelse(markers$avg_log2FC > 0, "up", "down")


  # pos_jitter <- position_jitter(width = 0.5, seed = 5)
  p.volcano_data <- markers

  p.volcano_data$xjitter <-
    jitter(as.numeric(factor(levels(p.volcano_data$cluster)[p.volcano_data$cluster])))

  p.volcano_cluster <-
    ggplot2::ggplot() +
    ggplot2::geom_point(data = p.volcano_data,
                        mapping =
                          aes(x = xjitter,
                              y = avg_log2FC,
                              color = sig,
                              alpha = 0.7),
                        size = dot_size) +
    labs(x = "Cluster") +
    ggplot2::facet_grid(. ~ cluster,
                        scale = "free_x",
                        space = "free_x") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(size = 15),
      axis.text = element_text(size = 14),
      axis.title.y = element_text(size = 15),
      #strip.background = element_blank(),
      strip.background = element_rect(fill = "grey",
                                      color = "black",
                                      size = 0.5),
      strip.text = element_text(size = 14),
      panel.border = element_rect(color = "black",
                                  fill = NA,
                                  size = 0.5)
    ) +
    #panel.grid.minor = element_blank(),
    #panel.grid.major = element_blank()) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      size = 0.15
    ) +
    ggplot2::scale_color_manual(values = c("black", "red2")) +
    ggrepel::geom_label_repel(data = p.volcano_data %>%
                                group_by(cluster) %>%
                                filter(avg_log2FC > 0) %>%
                                {
                                  if(order.by == "avg_log2FC") {
                                    dplyr::top_n(., n = n_pos, wt = avg_log2FC)
                                    } else {
                                      dplyr::top_n(., n = n_neg, wt = p_val_adj)
                                      }
                                  },
                              mapping = aes(x = xjitter,
                                            y = avg_log2FC,
                                            label = gene),
                              size = label.size,
                              min.segment.length = 0.1,
                              force = 10,
                              max.iter = 10000000) +
    ggeepel::geom_label_repel(data = p.volcano_data %>%
                                dplyr::group_by(cluster) %>%
                                dplyr::filter(avg_log2FC < 0) %>%
                                {
                                  if(order.by == "avg_log2FC") {
                                    dplyr::top_n(., n = n_neg, wt = avg_log2FC)
                                    } else {
                                      dplyr::top_n(., n = n_neg, wt = p_val_adj)
                                      }
                                  },
                              mapping = aes(x = xjitter,
                                            y = avg_log2FC,
                                            label = gene),
                              fill = "grey",
                              size = label.size,
                              min.segment.length = 0.1,
                              force = 10,
                              max.iter = 10000000)
  #min.segment.length = 0.1,
  #force = 10,
  #max.iter = 10000,
  #direction = "y")
  return(p.volcano_cluster)
}


  
  
