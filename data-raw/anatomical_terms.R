## code to prepare `anatomical_terms` dataset goes here
wt_colnames <- c("GeneID", "GeneSymbol",	"FishName",	"SuperStructureID",	
                 "SuperStructureName",	"SubStructureID",	"SubStructureName",	
                 "StartStage",	"EndStage",	"Assay", "AssayMMOID",	"PublicationID", 
                 "ProbeID",	"AntibodyID",	"FishID")

anatomical_terms <- 
  data.table::fread("https://zfin.org/downloads/wildtype-expression_fish.txt",
                    col.names = wt_colnames) %>%
  dplyr::select(c(1,5)) %>%
  dplyr::left_join(drerio_mart_ids, ., by = c("ZFIN ID" = "GeneID")) %>%
  dplyr::select(c(6,4)) %>%
  dplyr::rename("term" = 1, "NCBI"=2) %>%
  tidyr::drop_na() %>%
  dplyr::arrange(term) %>% 
  dplyr::distinct()

usethis::use_data(anatomical_terms, overwrite = TRUE)
