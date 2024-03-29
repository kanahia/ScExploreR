% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_plots.R
\name{ViolinGeneExpStage}
\alias{ViolinGeneExpStage}
\title{ViolinGeneExpStage}
\usage{
ViolinGeneExpStage(
  gene = NULL,
  cluster = NULL,
  clustering,
  metadata,
  slot_data,
  only_boxplot = TRUE
)
}
\arguments{
\item{gene}{gene of interests}

\item{cluster}{cluste rof interests}

\item{clustering}{metadata column with cluster names}

\item{metadata}{metadata}

\item{slot_data}{slot data(SCT here)}

\item{only_boxplot}{plot boxplot or violin}
}
\value{
plotly plot
}
\description{
ViolinGeneExpStage
}
