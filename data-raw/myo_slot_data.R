## code to prepare `myo_slot_data` dataset goes here
#!/usr/bin/env R
myo_alt <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/myo_new_alt.rds")
myo_slot_data <- myo_alt@assays$SCT@data

usethis::use_data(myo_slot_data, overwrite = TRUE)
