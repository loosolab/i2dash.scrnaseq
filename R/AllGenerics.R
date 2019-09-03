#' @importClassesFrom i2dash i2dashboard
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom Seurat Seurat
NULL

#' Create expression visualization for multiple selected features on a regular grid.
#'
#' Users can select features and a dimension reduction to plot feature expression values.
#'
#' @param report A \linkS4class{i2dash::i2dashboard} report.
#' @param object A \linkS4class{SingleCellExperiment::SingleCellExperiment} object or a \linkS4class{Seurat::Seurat} object.
#' @param use_dimred A list of data.frames (matrices) or a single data.frame (matrix) containing coordinates of the reduced dimensions, a character vector representing valid \code{reducedDim} slots of \code{object} or names of the \linkS4class{Seurat::DimReduc} object in \code{object@reductions}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns, or a string representing the name of an \code{assay} of \code{object}.
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @name feature-grid-page
#' @rdname feature-grid-page
#' @exportMethod add_feature_grid_page
setGeneric("add_feature_grid_page", function(report, object, ...) standardGeneric("add_feature_grid_page"))

#' Summarize samples or features of objects containing data from single-cell experiments.
#'
#' @param object A SingleCellExperiment or Seurat object.
#' @param columns The metadata columns to summarize.
#' @param FUNS A named vector indicating summary functions.
#' @param group_by (Optional) A vector, which is used for grouping.
#'
#' @return A \code{kableExtra} object with one row for each \code{columns} and one column for each \code{FUNS}, containing the desired summary. Optionaly \code{group_by} can be provided. In this case for each level in \code{group_by} a column is created with the summerized values for each \code{FUNS}.
#'
#' @name summarize
#' @rdname summarize
#' @exportMethod summarize_samples
setGeneric("summarize_samples", function(object, ...) standardGeneric("summarize_samples"))

#' @rdname summarize
#' @exportMethod summarize_features
setGeneric("summarize_features", function(object, ...) standardGeneric("summarize_features"))
