## code to prepare `drerio_mart_ids` dataset goes here

#' @export
mart_zfin <- 
  data.table::fread("/home/jason/data/references/mart_export_zfin_ensembl.txt") 

#' @export
drerio_mart_ids <-
  data.table::fread("/home/jason/data/references/mart_drerio_ids.txt") %>%
  dplyr::rename(NCBI = `NCBI gene (formerly Entrezgene) ID`) %>% 
  dplyr::left_join(., mart_zfin, by = c("Gene stable ID"))
  

usethis::use_data(drerio_mart_ids, overwrite = TRUE)
