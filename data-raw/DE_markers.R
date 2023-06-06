## code to prepare `DE_markers` dataset goes here

# get seurat object for markers calculation

integrated_data <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")

#s <- Seurat::PrepSCTFindMarkers(integrated_data)

DE_list <- list()

for(i in 1:length(levels(integrated_data))){
  for(j in 1:length(levels(integrated_data))) {
    
    if( i != j) {
      
      DE_list[[paste0(levels(integrated_data)[i], "_vs_", levels(integrated_data)[j])]] <-
        Seurat::FindMarkers(
          integrated_data,
          ident.1 = levels(integrated_data)[i],
          ident.2 =  levels(integrated_data)[j],
          only.pos = FALSE,
          min.pct = 0.1,
          logfc.threshold = 0.25,
          slot = "data") %>%
        dplyr::mutate(gene = rownames(.))
      
      DE_list[[paste0(levels(integrated_data)[i], "_vs_", levels(integrated_data)[j])]] <-
        DE_list[[paste0(levels(integrated_data)[i], "_vs_", levels(integrated_data)[j])]] %>%
        dplyr::mutate(cluster = ifelse(avg_log2FC > 0, levels(integrated_data)[i], levels(integrated_data)[j]),
                      p_val_adj = rstatix::p_round(p_val_adj),
                      p_val_adj = rstatix::p_round(p_val),
                      avg_log2FC = round(avg_log2FC, digits = 2))
      
    } else {
      print(paste0("skpped ",levels(integrated_data)[i], "_vs_", levels(integrated_data)[j]))
    }
    
  }
  
}

usethis::use_data(DE_list, overwrite = TRUE)
