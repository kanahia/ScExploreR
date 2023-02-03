#!/usr/bin/env R
integrated_all <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")
slot_data_all <- integrated_all@assays$SCT@data

usethis::use_data(slot_data_all, overwrite=TRUE)
