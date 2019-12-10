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
#' @param y Data containing numeric observations for the vertical axis (see Details).
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param group_by Optionally provide data containing grouping factors for the vertical axis.
#' @param from A character specifying where to obtain the data from (see Details).
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the component.
#' @param y_title The title of the y-axis.
#' @param group_by_title The title of the x-axis.
#'
#' @details The parameters \code{y}, \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment}- or \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment::SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat::Seurat} object.
#'   }
#'   In both cases, \code{y} and \code{group_by} take column names of \code{from}.
#'
#' @name violinplot
#' @rdname violinplot
#' @exportMethod violinplot
setGeneric("violinplot", function(dashboard, object, ...) standardGeneric("violinplot"))

#' Renders a component containing a horizontal barplot.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param y_group_by Data containing grouping factors for the vertical axis.
#' @param x_group_by Optionally provide data containing grouping factors for the horizontal axis. The result is a barplot grouped by the levels in \code{x_group_by} and shows the relative number of its observations.
#' @param from A character specifying where to obtain the data from (see Details).
#' @param title The title of the components junk.
#' @param y_group_by_title The title of the y-axis.
#' @param x_group_by_title The title of the x-axis.
#'
#' @details The parameters \code{y_group_by}, \code{x_group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment} or \emph{i2dashboard,Seurat} method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item \code{from} can either be \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment::SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat::Seurat} object.
#'   }
#'   In both cases, \code{y_group_by} and \code{x_group_by} take column names of \code{from}.
#'
#' @name barplot
#' @rdname barplot
#' @exportMethod barplot
setGeneric("barplot", function(dashboard, object, ...) standardGeneric("barplot"))

#' Renders a component containing a boxplot
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param x Data containing numeric observations for the horizontal axis.
#' @param group_by Optionally provide data containing grouping factors for the vertical axis..
#' @param from A character specifying where to obtain the data from (see Details).
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the components junk.
#' @param x_title The title of the x-axis.
#' @param group_by_title The title of the y-axis.
#'
#' @details The parameters \code{x}, \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment}- or \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment::SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat::Seurat} object.
#'   }
#'   In both cases, \code{x} and \code{group_by} take column names of \code{from}.
#'
#' @name boxplot
#' @rdname boxplot
#' @exportMethod boxplot
setGeneric("boxplot", function(dashboard, object, ...) standardGeneric("boxplot"))

#' Renders a component containing a scatterplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param x Data that will be mapped to the x-axis (see Details).
#' @param y Data that will be mapped to the y-axis (see Details).
#' @param from A character specifying where to obtain the data from \code{object} (see Details).
#' @param use_dimred A character vector indicating the reduced dimension to use from \code{"object"} (see Details).
#' @param assay A character defining the assay of \code{object} and is used for obtaining the \code{exprs_values} (default "RNA") (see Details).
#' @param slot A character defining the data slot of \code{assay}.
#' @param colour_by Numeric or factorial values that will be used for colouring.
#' @param labels An optional vector with sample names. A dropdown menu for colouring by label will be provided.
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#' @param plot_title An optional title of the plot.
#'
#' @details The parameters \code{x}, \code{y}, \code{colour_by}, \code{use_dimred}, \code{exprs_values}, \code{assay} and \code{slot}) take different arguments depending on the class of \code{object}.
#'   In case the \emph{i2dashboard,missing}-method, the parameters \code{x}, \code{y}, \code{colour_by} and \code{exprs_values} are expected to be of class \code{data.frame} or \code{matrix}. The parameters \code{x}, \code{y} can also be numeric vectors. The parameters \code{use}, \code{use_dimred}, \code{assay} and \code{slot} can be ignored.
#'   In case the \emph{i2dashboard,SingleCellExperiment}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"}, \code{"rowData"} or \code{"reducedDim"}
#'     \item \code{use_dimred} the name of an item in \code{reducedDims(object)}
#'     \item \code{exprs_values} a valid assay name from \code{assayNames(object)}
#'   }
#'   In case of the \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"meta.data"} for sample metadata, \code{"meta.feature"} for feature metadata, \code{"reduction"} for a dimension reduction
#'     \item \code{use_dimred} the name of an item in \code{object@reductions}
#'     \item \code{assay} a valid assay name from \code{names(object@assays)}
#'     \item \code{slot} a valid data slot from \code{assay}
#'   }
#'   In both cases, \code{x}, \code{y}, \code{colour_by} take column names of \code{from}.
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
#' @param subset_row An optional character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use the first 100 features.
#' @param split_by An optional data.frame (matrix) containing grouping factors for spliting columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param aggregate_by An optional data.frame (matrix) containing grouping factors for aggregating columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param title Title of the component.
#' @param legend Title of the heatmap legend.
#' @param cluster_rows A logical controls whether to make cluster on rows.
#' @param cluster_columns A logical controls whether to make cluster on columns.
#' @param clustering_method Method to perform hierarchical clustering, passed to \link[stats]{hclust}.
#' @param clustering_distance The distance measure to use for hierarchical clustering.
#' @param show_column_labels A logical controls whether the column lables should be displayed. Pay attention that a large number of columns with column lables can cause visualization problems.
#' @param column_title The column title of the heatmap.
#' @param row_title The row title of the heatmap.
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

#' Renders a component containing a violinplot with column- or row-level metadata.(This method is a wrapepr for the function \code{scater::plotColData} or \code{scater::plotRowData})
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param y A single characters or vector of characters specifying the metadata to show on the y-axis.
#' @param x A single characters or vector of characters specifying the metadata to show on the x-axis.
#' @param metadata A single characters or vector of characters specifying the metadata that is used to colour, size, shape the observations.
#' @param group_by A single character specifying the metadata from \code{metadata} that is used to to group the observations.
#' @param from A character specifying whether the function \code{scater::plotColData} or \code{scater::plotRowData} is used.
#' @param as_plotly
#' @param plot_title The title of the component.
#' @param y_title The title of the y-axis.
#' @param x_title The title of the x-axis.
#'
#' @details For further information see \code{?scater::plotColData()} / \code{?scater::plotRowData()}
#'
#' @name plotMetadata
#' @rdname plotMetadata
#' @exportMethod plotMetadata
setGeneric("plotMetadata", function(dashboard, object, ...) standardGeneric("plotMetadata"))

#' Renders a component containing a plot of the expression values for a set of features (e.g. genes or transcripts), against a continuous or categorical covariate for all cells.(This method is a wrapepr for the function \code{scater::plotExpression})
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns, or a string representing the name of an \code{assay} of \code{object}.
#' @param features A character vector specifying the features to plot.
#' @param metadata A single characters or vector of characters specifying the metadata that is used to group, colour, size, shape the observations.
#' @param x A single character specifying a feature or column name from "\code{metadata}" to show on the x-axis.
#' @param title The title of the component.
#' @param as_plotly Logical whether the ggplot2 plot should be converted into a plotly plot.
#' @param plot_title The title of the plot.
#' @param y_title The title of the y-axis.
#' @param x_title The title of the x-axis.
#' @param ncol Integer scalar, specifying the number of columns to be used for the panels of a multi-facet plot.
#' @param scales String indicating whether should multi-facet scales be fixed ("\code{fixed}"), free ("\code{free}"), or free in one dimension ("\code{free_x}", "\code{free_y}").
#' @param ... Parameter for \code{scater::plotExpression}.
#'
#' @details For further information see \code{?scater::plotColData()} / \code{?scater::plotRowData()}
#'
#' @name plotExpression
#' @rdname plotExpression
#' @exportMethod plotExpression
setGeneric("plotExpression", function(dashboard, object, ...) standardGeneric("plotExpression"))

