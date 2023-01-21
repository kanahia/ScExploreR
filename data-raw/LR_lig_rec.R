## code to prepare `LR_lig_rec` dataset goes here

## code to prepare `LR_data` dataset goes here

lr_m_48ha <- 
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_48h_filtered/multiple_groups_LR__Medium_48h_results.xlsx",
    sheet = 2)

lr_h_48ha<-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_48h_filtered/multiple_groups_LR__High_48h_results.xlsx",
    sheet = 2)

lr_m_72ha <-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_72h_filtered/multiple_groups_LR__Medium_72h_results.xlsx",
    sheet = 2)

lr_h_72ha <-
  readxl::read_excel(
    "/home/jason/data/10x_heart/DanioTalk/daniotalk/LR_72h_filtered/multiple_groups_LR__Medium_72h_results.xlsx",
    sheet = 2)

LR_lig_rec <- list(
  "lr_m_48h" = lr_m_48ha,
  "lr_h_48h" = lr_h_48ha,
  "lr_m_72h" = lr_m_72ha,
  "lr_h_72h" = lr_h_72ha)

usethis::use_data(LR_lig_rec, overwrite = TRUE)

