library("Seurat")
library("ggplot2")
library("dplyr")
library("tibble")
library("scales")
library("viridis")

## integrated_all
integrated_all <- readRDS("/home/jason/data/10x_heart/SCT_int_no_regression/Outdir/Objects/integrated_all.rds")

#metadata

metadata_all <-
  integrated_all@meta.data %>%
  tibble::rownames_to_column(var = "cell") %>%
  left_join(.,
            Embeddings(integrated_all, reduction = "umap") %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "cell"),
            by = c("cell" = "cell")
            )
# labels
l1 <- c()
l2  <- c()
for(i in 1:length(unique(metadata_all$edited_res.1.5))) {
 l1[i] <-
   metadata_all %>%
    dplyr::filter(edited_res.1.5 == unique(metadata_all$edited_res.1.5)[i]) %>%
    .$UMAP_1 %>% as.numeric() %>% median() %>% round(., digits = 2) %>% as.vector()

 l2[i] <-
   metadata_all %>%
   dplyr::filter(edited_res.1.5 == unique(metadata_all$edited_res.1.5)[i]) %>%
   .$UMAP_2 %>% as.numeric() %>% median() %>% round(., digits = 2) %>% as.vector()
}

labels_for_plotting <- data_frame(label = unique(as.vector(metadata_all$edited_res.1.5)),
                                  umap_1 = as.vector(l1),
                                  umap_2 = as.vector(l2))

labels_for_plotting <- as.data.frame(labels_for_plotting)


#theme
one_theme <- theme(axis.text.x = element_text(size = 16),
                 axis.text.y = element_text(size = 16),
                 axis.title.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16),
                 legend.text=element_text(size = 14),
                 legend.title=element_blank(),
                 plot.title = element_text(hjust = 0.5, face = "bold"))

#umap
  ggplot(metadata_all) +
    geom_point(aes(x = UMAP_1 , y = UMAP_2, color = edited_res.1.5), alpha = 0.75) +
    scale_color_manual(
      values = colors_main_umap) +
    theme_bw() +
    one_theme +
    NoLegend() +
  annotate(geom = "text",
           label = labels_for_plotting$label,
           x = labels_for_plotting$umap_1,
           y = labels_for_plotting$umap_2,
           size = 4.5)

#' Custom FeaturePlot
#'
#' @param metadata metadata exported from seurat
#' @param data_slot slot "data" from seurat
#' @param gene gene to be plotted
#' @param identitty identity of interest (Idents(seurat))
#' @param label to plot labels or not
#' @param order whether sort plotting order or not
#'
my_FeaturePlot <- function(metadata,
                           data_slot,
                           gene,
                           identity,
                           label = FALSE,
                           order = FALSE) {

  metadata <- metadata
  # check if identity is a string
  stopifnot(is.character(identity) & identity %in% colnames(metadata))

  #filter matrix
  m_data <- data_slot
  m_gene <- m_data[gene, ]
  m_gene_cell <-
    m_gene[m_gene > 0] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::rename("data" =2) %>%
    dplyr::inner_join(.,
                      metadata,
                      by = "cell")

  filtered_data_positive <-
    as.data.frame(m_gene[m_gene > 0]) %>%
    tibble::rownames_to_column(var = "cell") %>%
    dplyr::rename("slot_data" = 2)

  metadata <- metadata %>%
    left_join(., filtered_data_positive,
              by = c("cell" = "cell"))

  metadata$slot_data <- ifelse(is.na(metadata$slot_data), 0, metadata$slot_data)

  # get colors for plotting
  metadata$color <- ifelse(metadata$cell %in% m_gene_cell$cell, "positive", "negative")
  metadata$color <- factor(metadata$color, levels = c("positive", "negative"), ordered = TRUE)

  #theme
  one_theme <- theme(axis.text.x = element_text(size = 16),
                     axis.text.y = element_text(size = 16),
                     axis.title.x = element_text(size = 16),
                     axis.title.y = element_text(size = 16),
                     legend.text = element_text(size = 14),
                     legend.title = element_blank(),
                     plot.title = element_text(hjust = 0.5, face = "bold"))

  #coorinates for labels
  position_label <-
    metadata %>%
    dplyr::select(!!sym(identity), UMAP_1, UMAP_2) %>%
    group_by(!!sym(identity)) %>%
    summarise(u1 = median(UMAP_1),
              u2 = median(UMAP_2)) %>%
    as.data.frame()

  FT.plot <-
    ggplot() +
    geom_point(data = metadata[metadata$color == "negative", ],
               aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    geom_point(data = metadata[metadata$color != "negative", ],
               aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
    #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
    theme_classic() +
    ggtitle(gene) +
    one_theme

  if(order == TRUE) {
    FT.plot <-
      ggplot() +
      geom_point(data = metadata[metadata$color == "negative", ],
                 aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      geom_point(data = metadata[metadata$color != "negative", ],
                 aes(x = UMAP_1 , y = UMAP_2, color = slot_data), alpha = 0.9, size = 0.04) +
      #scale_color_manual(values = c("positive" = "#414487FF" , "negative" = "#FDE725FF")) +
      theme_classic() +
      ggtitle(gene) +
      one_theme
    } else {
      set.seed(42)
      shuffled_metadata <- metadata[sample(1:nrow(metadata)), ]
      FT.plot <-
        ggplot() +
        geom_point(data = shuffled_metadata,
                   aes(x = UMAP_1 , y = UMAP_2, color = slot_data, fill = color), alpha = 0.9, size = 0.04) +
        theme_classic() +
        ggtitle(gene) +
        one_theme +
        scale_fill_discrete(guide = "none")
      }
  # plot umap
  if(label == TRUE) {
    FT.plot <-
      FT.plot +
      annotate(geom = "text",
               label = position_label[[cluster]],
               x = position_label$u1,
               y = position_label$u2,
               size = 4.5) +
      scale_color_viridis(direction = -1)
    } else {
      FT.plot <-
        FT.plot +
        scale_color_viridis(direction = -1)
      }
  return(FT.plot)
}


data.table::fwrite(metadata_all,
                 "/home/jason/data/shiny_dashboard/heart10x/data/metadata_all.csv",
                 row.names = FALSE)
saveRDS(integrated_all@assays$SCT@data,
        "/home/jason/data/shiny_dashboard/heart10x/data/slot_data_all.rds")

data.table::fwrite(metadata_all,
                   "/home/jason/data/shiny_dashboard/heart10x/data/metadata_all.csv",
                   row.names = FALSE)
saveRDS(Myo_new@assays$SCT@data,
        "/home/jason/data/shiny_dashboard/heart10x/data/slot_data_myo.rds")

data.table::fwrite(metadata_myo,
                   "/home/jason/data/shiny_dashboard/heart10x/data/metadata_myo.csv",
                   row.names = FALSE)


raw_heart.integrated <-
  readRDS("/home/jason/data/10x_heart/10x_clean/RDS_files/raw_heart_integrated.RDS")

raw_heart.integrated@assays

p.raw_mt.content_vs_nGenes <-
  raw_heart.integrated@meta.data %>%
  ggplot(aes(x = log10(nFeature_RNA), y = percent.mt, color = percent.mt)) +
  geom_point(alpha = 0.1, shape = 19) +
  #scale_colour_gradientn(colours = wes_palette(n = 5, name = "Zissou1")) +
  scale_color_gradientn(colors = viridis(256, option = "D")) +
  # scale_x_log10() +
  theme_minimal() +
  theme(legend.text=element_text(size=16),
        legend.title = element_text(size =18),
        axis.text = element_text(size =16),
        axis.title = element_text(size = 16)) +
  #ylim(0,100) +
  labs(x = "log10(Number of genes)") +
  geom_vline(xintercept = log10(200), linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(xintercept = log10(2500), linetype = "dotted", color = "black", size = 0.5) +
  geom_hline(yintercept = 30, linetype = "dotted", color = "red", size = 0.5) +
  geom_segment(aes(x = log10(200), y = 0, xend = log10(200), yend = 30), colour = "darkred") +
  geom_segment(aes(x = log10(2500), y = 0, xend = log10(2500), yend = 30), colour = "darkred") +
  geom_segment(aes(x = log10(200), y = 0, xend = log10(2500), yend = 0), colour = "darkred") +
  geom_segment(aes(x = log10(200), y = 30, xend = log10(2500), yend = 30), colour = "darkred") +
  geom_segment(aes(x = log10(5630), y = 38, xend = log10(2500), yend = 30), colour = "darkred",
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("label", x = log10(5630), y = 40, label = "Selected cells", size = 6)

p.raw_mt.content_vs_nGenes.hist <-
  ggExtra::ggMarginal(p.raw_mt.content_vs_nGenes,
                      type = "histogram",
                      margins = "x",
                      fill = "slateblue4", #4B03A1FF
                      size = 8)


t <- raw_heart.integrated@meta.data
t <- t[, c("nFeature_RNA", "percent.mt")]

metadata_raw_object <- data.table::fwrite(t, "/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv",
                                          row.names = TRUE)

t <- data.table::fread("/home/jason/data/shiny_dashboard/heart10x/data/metadata_raw_object.csv")



agata <- read.table("/home/jason/practice/first_pass_deseq2.csv", sep = ",")
anton_up <- xlsx::read.xlsx("/home/jason/practice/Anton/For_Karim.xlsx",
                         sheetName = "up")
anton_down <- xlsx::read.xlsx("/home/jason/practice/Anton/For_Karim.xlsx",
                            sheetName = "down")

agata <- agata %>%
  tibble::rownames_to_column(var = "id") %>%
  as.data.frame()

agata_bm <-
  biomaRt::getBM(
    filters = "ensembl_gene_id",
    attributes = c("ensembl_gene_id", "external_gene_name", "entrezgene_id"),
    values = agata_clean$id,
    mart = zebrafish,
    bmHeader = TRUE,
    uniqueRows = TRUE
  )

  GO_terms <- getBM(
    attributes = c("name_1006", "namespace_1003", "external_gene_name", "ensembl_gene_id"),
    filters = "ensembl_gene_id",
    values = agata$id,
    mart = zebrafish)


GO_terms_BP <- GO_terms[GO_terms$namespace_1003 == "biological_process", ]
GO_terms_MF <- GO_terms[GO_terms$namespace_1003 == "molecular_function", ]

df_BP <-
  data.frame(
    external_gene_name = unique(GO_terms_BP$ensembl_gene_id),
    GO_BP = sapply(unique(GO_terms_BP$ensembl_gene_id),
                   function(g)
                     paste0(GO_terms_BP[GO_terms_BP$ensembl_gene_id == g, "name_1006"],
                            collapse = ", ")
    )
  )

df_MF <-
  data.frame(
    external_gene_name = unique(GO_terms_MF$ensembl_gene_id),
    GO_MF = sapply(unique(GO_terms_MF$ensembl_gene_id),
                   function(g)
                     paste0(GO_terms_MF[GO_terms_MF$ensembl_gene_id == g, "name_1006"],
                            collapse = ", ")
    )
  )

agata_long <-
  agata %>%
  dplyr::left_join(., df_BP, by = c("id" = "external_gene_name")) %>%
  dplyr::left_join(., df_MF, by = c("id" = "external_gene_name")) %>%
  dplyr::arrange(desc(log2FoldChange)) %>%
  dplyr::mutate(padj = ifelse(is.na(padj), 1, padj)) %>%
  dplyr::mutate(pvalue = ifelse(is.na(pvalue), 1, pvalue))


xlsx::write.xlsx(agata_long,
                 "/home/jason/practice/first_pass_deseq2_anotated.xlsx",
                 row.names = FALSE)

agata_clean <-
  agata_long %>%
  dplyr::filter(log2FoldChange != "NA") %>%
  dplyr::filter(padj < 0.05) %>%
  dplyr::left_join(., agata_bm[, c(1,3)], by = c("id"= "Gene stable ID"))


up_genes <- agata_clean[agata_clean$log2FoldChange > 0 , ]
up_anoton_x <- anton_up[anton_up$log2FoldChange.x > 0 , ]
up_anoton_y <- anton_up[anton_up$log2FoldChange.y > 0 , ]

down_genes <- agata_clean[agata_clean$log2FoldChange < 0 , ]
down_anoton_x <- anton_down[anton_down$log2FoldChange.x > 0 , ]
down_anoton_y <- anton_down[anton_up$log2FoldChange.y > 0 , ]

list.up_down <- list(up = as.character(up_genes$id), down = as.character(down_genes$id))
anton_list_up_down <- list(up = as.character(up_anoton_x$ensembl_gene_id), down = as.character(down_anoton_x$ensembl_gene_id))

anton_list_up_down_2 <- list(up = as.character(up_anoton_y$ensembl_gene_id), down = as.character(down_anoton_y$ensembl_gene_id))

out_enrichGO <- compareCluster(gene = list.up_down,
                                    fun = "enrichGO",
                                    keyType = "ENSEMBL",
                                    OrgDb = org.Dr.eg.db,
                                    ont = "BP",
                                    pvalueCutoff = 0.05,
                                    #qvalueCutoff =0.05,
                                    pAdjustMethod = "BH")

anton_enrichGO <- compareCluster(gene = anton_list_up_down,
                               fun = "enrichGO",
                               keyType = "ENSEMBL",
                               OrgDb = org.Dr.eg.db,
                               ont = "BP",
                               pvalueCutoff = 0.05,
                               #qvalueCutoff =0.05,
                               pAdjustMethod = "BH")

anton_enrichGO_2 <- compareCluster(gene = anton_list_up_down_2,
                                 fun = "enrichGO",
                                 keyType = "ENSEMBL",
                                 OrgDb = org.Dr.eg.db,
                                 ont = "BP",
                                 pvalueCutoff = 0.05,
                                 #qvalueCutoff =0.05,
                                 pAdjustMethod = "BH")

dotplot(out_enrichGO, showCategory = 20) + ggtitle("ET33 vs CMs")

gofilter(out_enrichGO, level = 4)



up_genes_kegg <- agata_clean[agata_clean$log2FoldChange > 0 , ]
down_genes_kegg <- agata_clean[agata_clean$log2FoldChange < 0 , ]
list.up_down_kegg <- list(up = as.character(up_genes_kegg$`NCBI gene (formerly Entrezgene) ID`),
                          down = as.character(up_genes_kegg$`NCBI gene (formerly Entrezgene) ID`))
out_KEGG <- compareCluster(gene = list.up_down_kegg,
                       fun = "enrichKEGG",
                       organism = "dre",
                       keyType = "kegg",
                       pvalueCutoff = 0.05,
                       pAdjustMethod = "BH")

dotplot(out_KEGG, showCategory = 20) + ggtitle("KEGG: ET33 vs CMs")
