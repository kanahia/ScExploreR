## code to prepare `LR_data` dataset goes here

lr_m_48h <- 
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_48h_filtered/multiple_groups_LR__Medium_48h_results.xlsx")
lr_h_48h <-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_48h_filtered/multiple_groups_LR__High_48h_results.xlsx")
lr_m_72h <-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_72h_filtered/multiple_groups_LR__Medium_72h_results.xlsx")
lr_h_72h <-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_72h_filtered/multiple_groups_LR__Medium_72h_results.xlsx")

LR_data <- list(
  "lr_m_48h" = lr_m_48h,
  "lr_h_48h" = lr_h_48h,
  "lr_m_72h" = lr_m_72h,
  "lr_h_72h" = lr_h_72h)

usethis::use_data(LR_data, overwrite = TRUE)
