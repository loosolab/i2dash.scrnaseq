#' @importClassesFrom i2dash i2dashboard
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom Seurat Seurat
NULL

#' Create expression visualization for multiple selected features on a regular grid.
#'
#' Users can select features and a dimension reduction to plot feature expression values.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
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
setGeneric("add_feature_grid_page", function(dashboard, object, ...) standardGeneric("add_feature_grid_page"))

#' View a dimension reduction side-by-side with feature metadata
#'
#' The dimension reduction plot is colored by feature expression and updated if the users clicks feature rows in the metadata table.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object A \linkS4class{SingleCellExperiment::SingleCellExperiment} object or a \linkS4class{Seurat::Seurat} object.
#' @param use_dimred A list of data.frames (matrices) or a single data.frame (matrix) containing coordinates of the reduced dimensions, a character vector representing valid \code{reducedDim} slots of \code{object} or names of the \linkS4class{Seurat::DimReduc} object in \code{object@reductions}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns, or a string representing the name of an \code{assay} of \code{object}.
#' @param feature_metadata A data.frame (matrix) along rows of \code{exprs_values} containing feature metadata, or a character vector indicating columns from \code{rowData(object)} or \code{object@meta.data}.
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @name dimred-metadata-page
#' @rdname dimred-metadata-page
#' @exportMethod add_dimred_metadata_page
setGeneric("add_dimred_metadata_page", function(dashboard, object, ...) standardGeneric("add_dimred_metadata_page"))

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

#' Add a gene expression page.
#'
#' This function adds a page with two linked components to the \code{dashboard} object: A scatterplot, showing samples in along two-dimensional coordinates, and a violin plot, showing feature expression values by groups defined in \code{group_by}.
#'
#' @name add_gene_expression_page
#' @rdname add_gene_expression_page
#' @exportMethod add_gene_expression_page
setGeneric("add_gene_expression_page", function(dashboard, object, ...) standardGeneric("add_gene_expression_page"))

#' Renders a component containing a vertical violin plot
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param y A data.frame (matrix) containing numeric observations for the vertical axis, or a character vector indicating column names of \code{colData(object)}, \code{rowData(object)}.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param group_by An optional data.frame (matrix) with columns containing grouping factors for the horizontal axis.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"} or \code{"rowData"}.
#' @param title The title of the component.
#' @param y_title The title of the y-axis.
#' @param group_by_title The title of the x-axis.
#'
#' @name violinplot
#' @rdname violinplot
#' @exportMethod violinplot
setGeneric("violinplot", function(dashboard, object, ...) standardGeneric("violinplot"))

#' Renders a component containing a horizontal barplot.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param y_group_by A data.frame (matrix) with columns containing grouping factors for the vertical axis.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param x_group_by Optionally provide a data.frame (matrix) with columns containing grouping factors for the horizontal axis. The result is a barplot grouped by the levels in \code{x_group_by} and shows the relative number of its observations.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"} or \code{"rowData"}.
#' @param title The title of the components junk.
#' @param y_group_by_title The title of the y-axis.
#' @param x_group_by_title The title of the x-axis.
#'
#' @name barplot
#' @rdname barplot
#' @exportMethod barplot
setGeneric("barplot", function(dashboard, object, ...) standardGeneric("barplot"))

#' Renders a component containing a boxplot
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param x A data.frame (matrix) containing numeric observations for the horizontal axis, or a character vector indicating column names of \code{colData(object)}, \code{rowData(object)}.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param group_by An optional data.frame (matrix) with columns containing grouping factors for the vertical axis.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"} or \code{"rowData"}.
#' @param title The title of the components junk.
#' @param x_title The title of the x-axis.
#' @param group_by_title The title of the y-axis.
#'
#' @name boxplot
#' @rdname boxplot
#' @exportMethod boxplot
setGeneric("boxplot", function(dashboard, object, ...) standardGeneric("boxplot"))

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
