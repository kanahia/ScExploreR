#' Function description... TODO
#'
#' @param input_metadata ... TODO
#'
#' @return ... TODO ggplot object?
#'
#' @importFrom data.table fread
#' @importFrom ggplot2 aes annotate arrow element_text ggplot geom_point geom_hline geom_segment geom_vline labs scale_color_gradientn theme theme_minimal unit
#' @importFrom viridis viridis
#' @importFrom ggExtra ggMarginal
#'
#' @export
raw_ngene_mt <- function(input_metadata) {
  t <- data.table::fread(input_metadata)
  t2 <-
    t %>%
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

  t3 <-
    ggExtra::ggMarginal(t2,
                        type = "histogram",
                        margins = "x",
                        fill = "slateblue4", #4B03A1FF
                        size = 8)
  return(t3)
}
