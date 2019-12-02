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

#' Add a dimension reduction page with feature metadata
#'
#' This function adds a page with two linked components to the \code{dashboard} object: A scatterplot, showing samples in along two-dimensional coordinates, and a table, showing feature metadata. A click on a feature in the table updates the scatterplot with the feature expression.
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
#' @exportMethod add_dimred_feature_page
setGeneric("add_dimred_feature_page", function(dashboard, object, ...) standardGeneric("add_dimred_feature_page"))

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
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from (see Details).
#' @param group_by Data along samples that is used for grouping expression values in the violin plot (see Details).
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{Seurat::Seurat} object.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param labels A vector with optional sample labels that are used instead of \code{rownames(use_dimred)}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{Seurat::Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of a item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#'
#' @name add_feature_expression_page
#' @rdname add_feature_expression_page
#' @exportMethod add_feature_expression_page
setGeneric("add_feature_expression_page", function(dashboard, object, ...) standardGeneric("add_feature_expression_page"))

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

#' Renders a component containing a scatterplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param x A data.frame (matrix) containing columns with numeric values that will be mapped to the x-axis.
#' @param y A data.frame (matrix) containing columns with numeric values that will be mapped to the y-axis.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param use A character specifying where to obtain the data from. One of \code{"colData"}, \code{"rowData"}, \code{"reducedDim"}.
#' @param reducedDim A character vector indicating the reduced dimension to use from \code{"reducedDim"}
#' @param colour_by An optional data.frame (matrix) containing columns with numeric or factorial values that will be used for colouring.
#' @param labels An optional vector with sample names. A dropdown menu for colouring by label will be provided.
#' @param exprs_values An optional data.frame (matrix) containing expression data of features of interest in rows and samples in columns.
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#'
#' @name scatterplot
#' @rdname scatterplot
#' @exportMethod scatterplot
setGeneric("scatterplot", function(dashboard, object, ...) standardGeneric("scatterplot"))

#' Renders a component containing a \link[ComplexHeatmap]{Heatmap}.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns or a string representing the name of an \code{assay} of \code{object}.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param split_by An optional data.frame (matrix) containing grouping factors for spliting columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param aggregate_by An optional data.frame (matrix) containing grouping factors for aggregating columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param title Title of the component.
#' @param legend Title of the heatmap legend.
#' @param cluster_rows A logical controls whether to make cluster on rows.
#' @param cluster_columns A logical controls whether to make cluster on columns.
#' @param clustering_method Method to perform hierarchical clustering, passed to \link[stats]{hclust}.
#' @param clustering_distance The distance measure to use for hierarchical clustering.
#'
#' @name heatmap
#' @rdname heatmap
#' @exportMethod heatmap
setGeneric("heatmap", function(dashboard, object, ...) standardGeneric("heatmap"))

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

