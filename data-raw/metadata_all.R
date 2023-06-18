#!/usr/bin/env R
# metadata
#devtools::load_all()
library("magrittr")

#integrated_all <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")
metadata_all <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all_metadata.rds")

# metadata_all <-
#   integrated_all@meta.data %>%
#   tibble::rownames_to_column(var = "cell") %>%
#   dplyr::left_join(
#     y = SeuratObject::Embeddings(integrated_all, reduction = "umap"
#   ) %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(
#     var = "cell"),
#     by = c("cell" = "cell")
#   )
# cluster_order <- c(
#   "Myocardium", "Bulbus arteriosus", "Epicardium", "Mesoderm progenitors",
#   "AV endocardium", "AV cushion", "Neural crest", "Erythrocytes",
#   "Hematopoietic precursor", "Mesenchymal fibroblasts",
#   "Cardiac peripheral nerves", "Neuronal cells", "Leukocytes",
#   "Resident fibroblasts", "Endothelial precursors", "Proliferating cells",
#   "Endothelial cells", "Unclassified")
#  metadata_all$edited_res.1.5 <- factor(metadata_all$edited_res.1.5,
#                                        levels = cluster_order)

usethis::use_data(metadata_all, overwrite=TRUE)

# labels for all dataset
labels_all <- metadata_all %>%
  dplyr::select(edited_res.1.5, UMAP_1, UMAP_2) %>%
  dplyr::group_by(edited_res.1.5) %>%
  dplyr::summarise(u1 = median(UMAP_1),
                   u2 = median(UMAP_2)) %>%
  dplyr::rename("label" = 1) %>%
  dplyr::mutate(color = ScExploreR::colors_main_umap)

usethis::use_data(labels_all, overwrite=TRUE)

# Myo dataset

#myo_data <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/myo_new.rds")
#metadata_myo <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/myo_metadata.rds")
# metadata_myo <-
#   myo_data@meta.data %>%
#   tibble::rownames_to_column(var = "cell") %>%
#   dplyr::left_join(
#     y = SeuratObject::Embeddings(myo_data, reduction = "umap"
#     ) %>%
#       as.data.frame() %>%
#       tibble::rownames_to_column(
#         var = "cell"),
#     by = c("cell" = "cell")
#   )

myo_alt <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/myo_new_alt.rds")
metadata_myo <- myo_alt@meta.data
metadata_myo$UMAP_1 <- as.data.frame(myo_alt@reductions$umap@cell.embeddings)$UMAP_1
metadata_myo$UMAP_2 <- as.data.frame(myo_alt@reductions$umap@cell.embeddings)$UMAP_2

metadata_myo <- metadata_myo %>% tibble::rownames_to_column(var = "cell") %>% as.data.frame()

metadata_myo$custom_int_res2 <- factor(metadata_myo$custom_int_res2,
                                       levels = (c("Atrial CMs I", "Atrial CMs II", "Ventricular CMs", "Sinoatrial CMs",  
                                                   "Atrioventricular CMs", "Cardiomyocytes", "Cluster I")))

usethis::use_data(metadata_myo, overwrite=TRUE)

# labels for all dataset
labels_myo <- metadata_myo %>%
  dplyr::select(custom_int_res2, UMAP_1, UMAP_2) %>%
  dplyr::group_by(custom_int_res2) %>%
  dplyr::summarise(u1 = median(UMAP_1),
                   u2 = median(UMAP_2)) %>%
  dplyr::rename("label" = 1) %>%
  dplyr::mutate(color = scales::hue_pal()(length(levels(metadata_myo$custom_int_res2))))

usethis::use_data(labels_myo, overwrite=TRUE)

