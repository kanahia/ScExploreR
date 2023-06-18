#' ncbi2gene
#'
#' @param enrichResult 
#' @param gene_name_type ensembl or extrenal gene name
#'
#' @return
#' @export
#'
ncbi2gene <- function(enrichResult, gene_id_type = NULL){
  vec <- setNames(drerio_mart_ids[[gene_id_type]], drerio_mart_ids$NCBI)
  vec <- vec[!is.na(names(vec))]
  
  gene_name <- 
    sapply(X = as.data.frame(enrichResult)$geneID %>% stringr::str_split(pattern = "/"),
           FUN = function(x) vec[x] %>% paste(collapse = "/"))
  
  return(gene_name)
}
