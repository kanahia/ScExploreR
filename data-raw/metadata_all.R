#!/usr/bin/env R
# metadata
library("magrittr")

integrated_all <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")
metadata_all <-
  integrated_all@meta.data %>%
  tibble::rownames_to_column(var = "cell") %>%
  dplyr::left_join(
    y = SeuratObject::Embeddings(integrated_all, reduction = "umap"
  ) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(
    var = "cell"),
    by = c("cell" = "cell")
  )

usethis::use_data(metadata_all, overwrite=TRUE)
