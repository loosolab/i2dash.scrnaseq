% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/violinplot.R
\name{violinplot}
\alias{violinplot}
\alias{violinplot,i2dashboard,missing-method}
\alias{violinplot,i2dashboard,SingleCellExperiment-method}
\alias{violinplot,i2dashboard,Seurat-method}
\title{Renders a component containing a vertical violin plot}
\usage{
violinplot(dashboard, object, ...)

\S4method{violinplot}{i2dashboard,missing}(
  dashboard,
  y,
  group_by = NULL,
  title = NULL,
  y_title = NULL,
  group_by_title = NULL,
  transmitter = NULL
)

\S4method{violinplot}{i2dashboard,SingleCellExperiment}(
  dashboard,
  object,
  from = c("colData", "rowData"),
  y = NULL,
  group_by = NULL,
  ...
)

\S4method{violinplot}{i2dashboard,Seurat}(
  dashboard,
  object,
  from = c("meta.data", "meta.features"),
  assay = "RNA",
  y = NULL,
  group_by = NULL,
  ...
)
}
\arguments{
\item{dashboard}{An object of class \linkS4class{i2dashboard}.}

\item{object}{An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.}

\item{...}{Further parameters passed to the core function.}

\item{y}{Data containing numeric observations for the vertical axis (see Details).}

\item{group_by}{Optionally provide data containing grouping factors for the vertical axis.}

\item{title}{The title of the component.}

\item{y_title}{The title of the y-axis.}

\item{group_by_title}{The title of the x-axis.}

\item{transmitter}{A character defining the id of an existing transmitter to obtain the data from.}

\item{from}{A character specifying where to obtain the data from (see Details).}

\item{assay}{Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").}
}
\value{
A string containing markdown code for the rendered component
}
\description{
Renders a component containing a vertical violin plot
}
\details{
The parameters \code{y}, \code{group_by} take different arguments depending on the class of \code{object}.
  In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
  In case of the \emph{i2dashboard,SingleCellExperiment}- or \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character} and
  \itemize{
    \item the parameter \code{from} can be either \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment} object or
    \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat} object.
  }
  In both cases, \code{y} and \code{group_by} take column names of \code{from}.
}
