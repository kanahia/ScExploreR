#!/usr/bin/env R
# markers_all
devtools::load_all()

markers_myo <-
  ScExploreR::read_multiple_sheets("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/myo_markers_for_shiny.xlsx") %>%
  dplyr::select(-c("GO_def")) %>%
  dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
  #dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
  dplyr::mutate(p_val_adj = format(p_val_adj, scientific = TRUE)) %>%
  dplyr::mutate(p_val = format.pval(p_val, digits = 2)) %>%
  dplyr::select(-c("p_val"))

usethis::use_data(markers_myo, overwrite=TRUE)
