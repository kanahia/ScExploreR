## code to prepare `drerio_mart_ids` dataset goes here

drerio_mart_ids <-
  data.table::fread("/home/jason/data/references/mart_drerio_ids.txt") %>%
  dplyr::rename(NCBI = `NCBI gene (formerly Entrezgene) ID`)

usethis::use_data(drerio_mart_ids, overwrite = TRUE)
