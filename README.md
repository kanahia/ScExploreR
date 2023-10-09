# ScExploreR
<p align: "justify"> R Shiny app supporting the single cell RNAseq data generated within the frame of my doctoral project. Data refelct transcriptome profiles of nearly 50 000 cells building embryonic zebrafish heart at 48 and 72 hpf. At the moment, the preprint <b> scRNA-seq reveals the diversity of the developing cardiac cell lineage and molecular building blocks of the primary pacemaker</b> can be accessed under the following link <a href = "https://www.biorxiv.org/content/10.1101/2023.06.26.546508v1"> https://www.biorxiv.org/content/10.1101/2023.06.26.546508v1 </a> </p>

<p align: "justify"> R shiny app can be accesed here: <a href= "https://zfcardioscape.iimcb.gov.pl/"> https://zfcardioscape.iimcb.gov.pl/ </a></p>

<h2> Abstract</h2>
<p align: "justify"> The heart is comprised of a variety of specialized cell types that work in unison to maintain blood flow. Here we utilized scRNA-seq analysis to delineate the diversity of cardiac cell types in the zebrafish. With the growing use of the zebrafish to model human heart biology, a deeper insight into its complex cellular composition is critical for a better understanding of heart function, development, and associated malformations. We present a high resolution atlas of zebrafish heart single cells transcriptomics, consisting of over 50 000 cells representing the building blocks of the zebrafish heart at 48 and 72 hpf. We defined 18 discrete cell populations comprising major cell lineages and sublineages of the developing heart. We pinpointed a population of cells likely to be the primary pacemaker and identified the transcriptome profile defining this critical cell type. Our analyses identified two genes, atp1b3b and colec10, which were enriched in the sinoatrial pacemaker cells. CRISPR/Cas9-mediated knockout of these two genes significantly reduced heart rate which is accompanied by arrhythmia or morphological defects, suggesting their novel function in cardiac development and conduction. Additionally, we describe other subpopulations of cardiac cell lineages, including the endothelial and neural cells, whose expression profiles we provide as a resource for further investigations into the cellular and molecular mechanisms of this organ. </p>

<h2> Methods </h2>
<p align: "justify"> Whole hearts were extracted from double transgenic individuals sqet31Et x Tg(myl7:mRFP) and sqet33mi59BEt x Tg(myl7:mRFP) and dissociated into single cells which were subsequently encapsulated according to the 10x Genomics workflow (detailed methods available in our publication). The transgenic lines sqet33mi59B and sqet31Et expressed EGFP in cells of the sinoatrial (SA) and atrioventricular (AV) pacemaker regions, which provided an internal control for rare cardiac cell populations. The transgenic line Tg(myl7:mRFP) was used to additionally demarcate cardiomyocytes (CMs), which is the most technically challenging cell type to isolate, and enhance cell clustering. Sequencing reads were mapped to the zebrafish reference genome GRCz11 (Ensembl release 100) extended with additional EGFP and mRFP sequences. Detailed parameters for cell filtering and downstream data processing can be found in our preprint. </p>

<h2> Experimental workflow </h2>

![image](https://github.com/kanahia/ScExploreR/assets/49271254/04eda8d7-d707-4e4e-86fb-88eb5a5563b9)
