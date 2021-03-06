% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization_functions.R
\name{plotly_bubbleplot}
\alias{plotly_bubbleplot}
\title{Render a bubbleplot with plotly.}
\usage{
plotly_bubbleplot(x, y, size, x_title = NULL, y_title = NULL, ...)
}
\arguments{
\item{x}{Numeric observations mapped to the x-axis.}

\item{y}{Numeric observations mapped to the y-axis.}

\item{size}{Numeric values defining the size of the dots.}

\item{x_title}{The title of the x-axis.}

\item{y_title}{The title of the y-axis.}

\item{...}{these arguments are of either the form \code{value} or \code{tag = value} and should be valid for the \code{plotly::plot_ly()} method.}
}
\value{
An object of class \code{plotly}.
}
\description{
Render a bubbleplot with plotly.
}
