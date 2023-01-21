#' ncbi2gene
#'
#' @param enrichResult 
#'
#' @return
#' @export
#'
ncbi2gene <- function(enrichResult){
  vec <- setNames(drerio_mart_ids$`Gene name`, drerio_mart_ids$NCBI)
  vec <- vec[!is.na(names(vec))]
  
  gene_name <- 
    sapply(X = as.data.frame(enrichResult)$geneID %>% stringr::str_split(pattern = "/"),
           FUN = function(x) vec[x] %>% paste(collapse = "/"))
  
  return(gene_name)
}
