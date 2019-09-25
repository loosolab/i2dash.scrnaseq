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

#' Characterize and visualize dimension reductions and sample groupings / metadata.
#'
#' Creates a page with up to four different linked components, including a scatterplot for dimension reductions, a bar plot showing numbers of observations by group, and a silhouette plot to assess grouping consistency.
#' Additional sample metadata is visualized using boxplots and barplots, depending on the data type of the underlying variable.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param use_dimred A data.frame (matrix) containing coordinates of the reduced dimensions. Rownames are used as sample labels.
#' @param metadata A data.frame (matrix) containing metadata (e.g. cluster, timepoint, number of features, etc) along samples.
#' @param group_by A string indicating a column in \code{metadata} that is used to group observations.
#' @param title The title of the page.
#' @param labels An optional vector with sample labels.
#' @param show_group_sizes A logical value indicating if a barplot showing the number of observations from \code{group_by} will be creaed (default \code{TRUE}).
#' @param show_silhouette A logical value indicating if a silhouette plot should be shown (default \code{FALSE}).
#' @param menu The name of the menu, under which the page should appear.
#'
#' @name dimred-metadata-page
#' @rdname dimred-metadata-page
#' @exportMethod add_dimred_metadata_page
setGeneric("add_dimred_metadata_page", function(dashboard, object, ...) standardGeneric("add_dimred_metadata_page"))
