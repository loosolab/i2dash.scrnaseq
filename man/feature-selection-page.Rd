% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/feature_selection_page.R
\name{feature-selection-page}
\alias{feature-selection-page}
\alias{add_feature_selection_page}
\alias{add_feature_selection_page,i2dashboard,Seurat-method}
\title{Quantify per-gene variation and explore the threshold on the metric of variation to get the desired set of highly variable features.}
\usage{
add_feature_selection_page(dashboard, object, ...)

\S4method{add_feature_selection_page}{i2dashboard,missing}(
  dashboard,
  exprs_values,
  use_function = c("modelGeneVar", "modelGeneVarByPoisson", "modelGeneCV2",
    "modelGeneCV2WithSpikes", "modelGeneVarWithSpikes"),
  title = "Feature selection",
  menu = "Tools",
  ...
)

\S4method{add_feature_selection_page}{i2dashboard,SingleCellExperiment}(dashboard, object, exprs_values = "logcounts", ...)

\S4method{add_feature_selection_page}{i2dashboard,Seurat}(
  dashboard,
  object,
  assay = "RNA",
  slot = "logcounts",
  ...
)
}
\arguments{
\item{dashboard}{A \linkS4class{i2dash::i2dashboard}.}

\item{object}{An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.}

\item{...}{Further parameters provided to the method specified in \code{use_function}.}

\item{exprs_values}{A numeric matrix of log-counts, or a string indicating the assay of \code{object}.}

\item{use_function}{Choose the \code{scran} method to model the variance of the log-expression profiles for each gene.}

\item{title}{The title of the page.}

\item{menu}{The name of the menu, under which the page should appear.}

\item{assay}{In case of a \code{Seurat}-class \code{object} a string indicating the assay to use.}

\item{slot}{In case of a \code{Seurat}-class \code{object} a string indicating the assay to use.}
}
\value{
An object of class \linkS4class{i2dash::i2dashboard}.
}
\description{
Creates a page with a scatterplot of the variance of log-expression against the mean log-expression and a table with features and their metrics of variation. With shiny inputs you can color highlight the hvgs in dependency to the proportion and minimal threshold of the relevant variation metric. Also you can download the selcted hvgs or the entire variation metrics table.
}
