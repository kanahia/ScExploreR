#' Read multiple excel sheets
#'
#' @param file path to the file
#'
#' @return a data.frame
#'
#' @importFrom dplyr bind_rows
#' @importFrom readxl excel_sheets
#'
read_multiple_sheets <- function(file){
  sheets <- readxl::excel_sheets(file)
  out_list <- list()

  for(i in 1:length(sheets)) {
    out_list[[i]] <- readxl::read_xlsx(file,
                                       sheet = sheets[i])
  }

  df <- dplyr::bind_rows(out_list)
  return(df)
}
