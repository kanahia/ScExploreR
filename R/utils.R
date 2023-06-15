#' re-export magrittr pipe operator
#'
#' @name %>%
#' @rdname pipe
#'
#' @importFrom magrittr %>%
#'
#' @export
NULL


#' Title
#'
#' @param seurat seurat object
#' @param metadata_clusters metadata to choose for clustering
#'
#' @return ggplot object
#' @export custom_DimPlot
#'
custom_DimPlot <- function(seurat,
                           metadata_clusters = "custom_int_res2",
                           selected_label = c("label", "new_label"),
                           wrap_length = 10) {
  
  main_seurat <- "integrated_all"
  
  colors_main_umap <- 
    c("#76AF00", "#E48800", "#00B92B", "deeppink3", "#C69900", "#5CB300",
      "#9290FF", "#00BF79", "#E26EF7", "#C27EFF", "#00B8E5", "#FC61D5",
      "#FF61C6", "#FF66A7", "#8BAB00", "#00BCD9", "#F8766D", "#B99E00")
  
  #preparing metadata
  metadata <- seurat@meta.data
  metadata$UMAP_1 <- as.data.frame(seurat@reductions$umap@cell.embeddings)$UMAP_1
  metadata$UMAP_2 <- as.data.frame(seurat@reductions$umap@cell.embeddings)$UMAP_2
  
  #calculating labels positions
  labels <-
    metadata %>%
    dplyr::select(!!sym(metadata_clusters), UMAP_1, UMAP_2) %>%
    dplyr::group_by(!!sym(metadata_clusters)) %>%
    summarise(u1 = median(UMAP_1),
              u2 = median(UMAP_2)) %>%
    dplyr::rename("label" = 1) %>%
    dplyr::mutate(new_label = paste0("Cluster ", label))
  
  
  if(deparse(substitute(seurat)) == main_seurat) {
    labels$color <- colors_main_umap
  }
  
  #plot
  manual.Dimplot <-
    ggplot2::ggplot(metadata) +
    ggplot2::geom_point(ggplot2::aes(x = UMAP_1,
                                     y = UMAP_2,
                                     color = !!sym(metadata_clusters)),
                        alpha = 0.9,
                        size = 1.1) + #previosuly 0.6
    ggplot2::theme_minimal() +
    Seurat::NoLegend() +
    # ggrepel::geom_label_repel(data = labels,
    #                           x = labels$u1,
    #                           y = labels$u2,
    #                           label = labels$label  %>% 
    #                             stringr::str_wrap(., width = 10),
    #                           fill = scales::hue_pal()(length(levels(seurat))), 
    #                           alpha = 0.75, 
    #                           size = 6.5) +
    ggplot2::theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.text.x = ggplot2::element_text(size = 16),
      # axis.text.y = ggplot2::element_text(size = 16),
      # axis.title.x = ggplot2::element_text(size = 16),
      # axis.title.y = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour =NA)
    )
  
  if(deparse(substitute(seurat)) == main_seurat) {
    
    manual.Dimplot <- 
      manual.Dimplot +
      ggrepel::geom_label_repel(data = labels,
                                x = labels$u1,
                                y = labels$u2,
                                label = labels[[selected_label]]  %>%
                                  stringr::str_wrap(., width = wrap_length),
                                fill = labels$color,
                                alpha = 0.75,
                                size = 6.5) +
      ggplot2::scale_color_manual(values = colors_main_umap)
    
  } else {
    
    manual.Dimplot <- 
      manual.Dimplot +
      ggrepel::geom_label_repel(data = labels,
                                x = labels$u1,
                                y = labels$u2,
                                label = labels[[selected_label]]  %>%
                                  stringr::str_wrap(., width = wrap_length),
                                fill = scales::hue_pal()(length(levels(seurat))),
                                alpha = 0.75,
                                size = 6.5)
  }
  
  return(manual.Dimplot)
  
}


#' Read multiple excel sheets
#' 
#'@param file path to the file
#'@export read_multiple_sheets
#'
read_multiple_sheets <- function(file){
  sheets <- readxl::excel_sheets(file)
  out_list <- list()
  
  for(i in 1:length(sheets)) {
    out_list[[i]] <- readxl::read_xlsx(file,
                                       sheet = sheets[i])
  }
  
  df <- dplyr::bind_rows(out_list)
  return(df)
}



#' Function save markers to excel file, each cluster on separate sheet
#'  
#' @param markers seuratFindMarkers output
#' @param file A character string naming an xlsx file
#' @param outdir A character path to outdir
#'
#' @return Return a xlsx file
#' 
#' @export
#' 
#' @examples  
#' markers2Csv(markers = markers, file = "markers.xlsx", outdir = "/path/to/directory/")

markers2Csv <- function(markers, file, outdir) {
  list_markers <- list()
  
  if(is.null(levels(markers$cluster))) {
    markers$cluster <- factor(markers$cluster, levels = unique(markers$cluster))
  }
  
  for(cl in levels(markers$cluster)) {
    
    if(nchar(cl) < 4) {
      name <- paste0("cluster_", cl)
    } else { 
      name <- cl
    }  
    
    
    list_markers[[name]] <- 
      markers[markers$cluster == cl, ] %>% 
      dplyr::arrange(desc(avg_log2FC))
  }
  
  
  wb <- openxlsx::createWorkbook()
  lapply(seq_along(list_markers), function(i) {
    openxlsx::addWorksheet(wb = wb, sheetName = names(list_markers[i]))
    openxlsx::writeData(wb, sheet = i, list_markers[[i]])
  })
  
  openxlsx::saveWorkbook(wb,
                         sprintf(paste0("%s/", file), outdir),
                         overwrite = TRUE)
}





