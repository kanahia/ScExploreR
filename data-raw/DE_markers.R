## code to prepare `DE_markers` dataset goes here

# get seurat object for markers calculation

integrated_data <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")

s <- Seurat::PrepSCTFindMarkers(integrated_data)

DE_list <- list()

for(i in 1:length(levels(s))){
  for(j in 1:length(levels(s))) {
    
    if( i != j) {
      
      DE_list[[paste0(levels(s)[i], "_vs_", levels(s)[j])]] <-
        Seurat::FindMarkers(
          s,
          ident.1 = levels(s)[i],
          ident.2 =  levels(s)[j],
          only.pos = FALSE,
          min.pct = 0.1,
          logfc.threshold = 0.25,
          slot = "data") %>%
        dplyr::mutate(gene = rownames(.))
    } else {
      print("next")
    }
    
  }
  
}

usethis::use_data(DE_list, overwrite = TRUE)
