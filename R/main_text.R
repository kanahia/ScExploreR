#' @export
main_text <-
  shiny::mainPanel(
    shiny::h1("scRNA-seq reveals the diversity of the developing cardiac cell lineage 
              and molecular building blocks of the primary pacemaker", 
              style = "font-size: 34px; font-weight: bold; margin-top: 25px; text-align: center"),
    
    shiny::p("We present a high-resolution single-cell atlas of the whole developing 
              heart in the zebrafish, a model organism increasingly used to study heart biology. 
              Our data consisted of over 50 000 cells representing the building blocks of the 
              zebrafish heart at 48 and 72 hpf, with two pseudoreplicates sequenced per time-point. 
              We distinguished 18 discrete cell populations comprising major cardiac cell lineages 
              and sublineages. Here, we provide our dataset as an accessible community resource 
              envisaged to pave the way for in-depth analysis of cell populations 
              with high specificity.", 
             shiny::br(
               shiny::span(
                 shiny::tags$u("While we are in the process of publishing our work"), 
                 style = "color: red;"), 
               "should you choose to use our data in your publication, please cite the preprint. "
               ),
            style = "font-size: 17px; margin-top: 10px; text-align: justify;"),
    shiny::p("Abu Nahia K, Sulej A, Migdal M, Ochocka N, Ho R, Kaminska B, Zagorski M, Winata C,. scRNA-seq reveals 
	     the diversity of the developing cardiac cell lineage and molecular building blocks of the primary pacemaker.
	     BioaRxiv:2023.06.26.546508. Available from: ", 
	     shiny::tags$a("https://www.biorxiv.org/content/10.1101/2023.06.26.546508v1",
	     		   href = "https://www.biorxiv.org/content/10.1101/2023.06.26.546508v1", target = "_blank"),
	     style = "font-size: 17px; text-align: justify; font-weight: bold;"),
    
    # shiny::p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.",
    #   style = "font-family: 'times'; font-si16pt"),
    # shiny::strong("strong() makes bold text."),
    # shiny::em("em() creates italicized (i.e, emphasized) text."),
    # shiny::br(),
    # shiny::code("code displays your text similar to computer code"),
    # shiny::div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div",
    #   style = "color:blue"),
    # shiny::br(),
    # shiny::p("span does the same thing as div, but it works with",
    # shiny::span("groups of words", 
    #        style = "color:blue"),
    #   "that appear inside a paragraph."),
    shiny::h2("Methods", style = "font-size: 26px; font-weight: bold; margin-top: 10px"),
    shiny::p("Whole hearts were extracted from double transgenic individuals",  
             shiny::em("sqet31Et x Tg(myl7:mRFP)"), "and", shiny::em("sqet33mi59BEt x Tg(myl7:mRFP)"), 
             "and dissociated 
             into single cells which were subsequently encapsulated according to the 10x 
             Genomics workflow (detailed methods available in our publication). 
             The transgenic lines", shiny::em("sqet33mi59B"), "and", shiny::em("sqet31Et"), 
             "expressed EGFP in cells of the sinoatrial (SA) and atrioventricular (AV) 
             pacemaker regions, which provided an internal control for rare cardiac cell populations. 
             The transgenic line", shiny::em("Tg(myl7:mRFP)"), "was used to additionally 
             demarcate cardiomyocytes (CMs), which is the most technically challenging 
             cell type to isolate, and enhance cell clustering. Sequencing reads were 
             mapped to the zebrafish reference genome GRCz11 (Ensembl release 100) 
             extended with additional EGFP and mRFP sequences. Detailed parameters 
             for cell filtering and downstream data processing can be found in our preprint.",
             shiny::br("Feel free to contact us for further enquiries:",
                       shiny::tags$a("kanahia@iimcb.gov.pl, cwinata@iimcb.gov.pl", 
                                     style = "color: blue"),
	    	      " and visit our lab webpage ", 
			shiny::tags$a("https://zdglab.iimcb.gov.pl/", 
				      href = "https://zdglab.iimcb.gov.pl", target = "_blank"),
                       ), 
             style = "font-size: 17px; margin-top: 10px; text-align: justify;"),
    shiny::br(),
    width = 12
  )



#' @export 
enrichment_text <-
  shiny::p(
    "This tool utilizes a standard hypergeometric test (from ClusterProfiler package)
    which enables the performance of gene enrichment analysis. The test is performed
    on a gene list provided by the user which can be further annotated by single-cell
    clusters representing embryonic zebrafish heart or ZFIN anatomical terms.
    The gene enrichment analysis is visualized by dotplot as well as summary table reflecting
    all the metrics, including enriched genes. At the moment, the ", shiny::tags$b("Ensembl Gene IDs"), "and", 
    shiny::tags$b("Gene names"), "are supported.",
    shiny::span(
      shiny::tags$ul("1. Paste your genes of interests (", shiny::tags$u("one gene per line"),").", 
                     style = "margin-left: 15px; margin-right:15px; font-size: 17px;"),
      shiny::tags$ul("2. Select annotation type.", 
                     style = "margin-left: 15px; margin-right:15px; font-size: 17px;"),
      shiny::tags$ul("3. Click the button.", 
                     style = "margin-left: 15px; margin-right:15px; font-size:17px;") 
      ),
    style = "font-size: 17px; margin-top: 10px; text-align:justify; margin-left:15px; 
            margin-right: 15px;"
  )

#' @export
diff_exp_text <-
  shiny::p("This panel allows for performing differential gene expression analysis between selected clusters. 
           The results are visualized by a volcano plot and a summary table with respective metrics.
           Genes can be labeled based on the significance level or log2FoldChange. 
           The slider can be used to specify the number of gene labels to show." ,
            style = "font-size: 17px; margin-top: 10px; text-align:justify;")
#' @export
heart_markers <-
  shiny::p("Marker genes between clusters were calculated according to Seurat FindMarkers function using default parameters.",
           style = "font-size: 17px; margin-top: 10px; text-align:justify; margin-left: 15px;")

#' @export
cell_matrix_info <-
  shiny::p(
    "Click the dot in the dot plot to display the gene expression matrix for the cell of interest",
    style = "font-size: 17px; color: black; font-weight: bold; text-decoration: underline; margin-top: -70px;
             margin-bottom: 45px; margin-left: -8px; text-align: center;")

#' @export
info_exp_lvl <- 
  shiny::p("This panel allows checking the expression level of the gene of interest in a 
           particular cluster depending on the developmental stage. A generated boxplot 
           is interactive so the user can manipulate the graph (zooming, dragging, etc.). 
           Below the number of cells expressing the chosen gene in a particular cluster is shown.",
           shiny::br(),
           shiny::br(),
           "In order to display other genes expressed in a particular cell, please mouse click on the cell of interest.",
           style = "text-align: justify")

#' @export
info_multiplot <-
  shiny::p("Panel allows data visualization depending on selected conditions." ,
           shiny::br(),
           shiny::br(),
           "At the start, the general QC plot with selected cells included for downstream analysis is shown. 
           Data were quality-checked based on mitochondrial gene content and a number of expressed genes.",
           shiny::br(),
           shiny::br(),
           "The overview section provides detailed information regarding the number of expressed genes, 
           UMI, and mitochondrial gene content in each main cell cluster.",
           shiny::br(),
           shiny::br(),
           "The Contribution panel presents the percentage composition of each cluster depending 
           on the developmental stage, the genetic background of transgenic lines used in the experiment, 
           sample origin, and cell-cycle stage.",
           shiny::br(),
           shiny::br(),
           "Plots are written in an interactive manner so they can be easily manipulated, for instance, enlarged.",
           style = "text-align: justify")

#' @export
info_FeaturePlot <-
  shiny::p("FeaturePlots can be used to visualize the expression of a particular gene. 
           The expression level is shown on the UMAP plot where each dot represents a single cell. 
           By plotting two genes side by side the co-expression can be checked. 
           Data can be displayed for the entire dataset or based on the developmental stage",
           style = "text-align: justify")

