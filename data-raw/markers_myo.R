#!/usr/bin/env R
# markers_all
devtools::load_all()

markers_myo <- ScExploreR::read_multiple_sheets("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Plots/Myocardium/markers_myo_new-updated.xlsx") %>%
  dplyr::mutate(avg_log2FC = round(avg_log2FC, digits = 2)) %>%
  dplyr::mutate(p_val_adj = format.pval(p_val_adj, digits = 2)) %>%
  dplyr::mutate(p_val = format.pval(p_val, digits = 2))

usethis::use_data(markers_myo, overwrite=TRUE)
