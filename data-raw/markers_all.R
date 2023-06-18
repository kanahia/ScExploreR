#!/usr/bin/env R
# markers_all
devtools::load_all()

markers_all <- 
  ScExploreR::read_multiple_sheets(
    file = "/home/jason/data/shiny_dashboard/heart10x/data/main_clustering_markers.xlsx") %>%
  dplyr::select(-1) %>%
  dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
  #dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
  dplyr::mutate(p_val_adj = format(p_val_adj, scientific = TRUE)) %>%
  dplyr::mutate(p_val = format.pval(p_val, digits = 2)) %>%
  dplyr::select(-c("p_val"))

usethis::use_data(markers_all, overwrite=TRUE)

