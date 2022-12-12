## code to prepare `term2gene` dataset goes here

## code to prepare `term2gene` dataset goes here

term2gene <-
  markers_all %>%
  dplyr::group_by(cluster) %>%
  dplyr::filter(p_val_adj < 0.05, avg_log2FC > 0) %>%
  dplyr::top_n(n = 1000, wt = avg_log2FC) %>%
  dplyr::select(c(1, 2, 4)) %>%
  dplyr::rename("term"= 1, "ensembl_id" = 2, "gene_name" = 3) %>%
  dplyr::select(c(1,2)) %>%
  dplyr::left_join(., drerio_mart_ids[,c(1,4)], by = c("ensembl_id" = "Gene stable ID")) %>%
  dplyr::select(-c(ensembl_id))

usethis::use_data(term2gene, overwrite = TRUE)

# ggplot2::ggplot(data = as.data.frame(o),
#                 aes(x = GeneRatio,
#                     y = ID,
#                     fill = rstatix::p_round(qvalue),
#                     size = Count),
#                 colour = "black") +
#   geom_point(inherit.aes = T)
# 
# 
# View(as.data.frame(o) %>%
#        dplyr::mutate(size = as.data.frame(o)$GeneRatio %>% stringr::str_split(pattern = "/") %>% unlist() %>% .[c(seq(2, length(.), by = 2))] %>% as.numeric(),
#                      ratio = Count/ratio))

