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
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param page The name of the page to be added.
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{Seurat::Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
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
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param feature_metadata A data.frame (matrix) along rows of \code{exprs_values} containing feature metadata, or a character vector indicating columns from \code{rowData(object)} or \code{object[[assay]]@feature.data}.
#' @param page The name of the page to be added.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{Seurat::Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#'
#' @name dimred-feature-page
#' @rdname dimred-feature-page
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

#' Add a feature expression page.
#'
#' This function adds a page with two linked components to the \code{dashboard} object: A scatterplot, showing samples in along two-dimensional coordinates, and a violin plot, showing feature expression values by groups defined in \code{group_by}.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param page The name of the page to be added.
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from (see Details).
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
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
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
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
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param y A data.frame (matrix) containing numeric observations for the vertical axisor a character vector indicating the columns to use from \code{use}.
#' @param group_by An optional data.frame (matrix) with columns containing grouping factors for the horizontal axis or a character vector indicating the columns to use from \code{use}.
#' @param use A character specifying where to obtain the data from. Valid input for SingleCellExperiment object: ("colData", "rowData"). Valid input for Seurat object: ("meta.data" for sample metadata, "meta.feature" for feature metadata.)
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
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
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param y_group_by A data.frame (matrix) with columns containing grouping factors for the vertical axis or a character vector indicating the columns to use from \code{use}.
#' @param x_group_by Optionally provide a data.frame (matrix) with columns containing grouping factors for the horizontal axis or a character vector indicating the columns to use from \code{use}. The result is a barplot grouped by the levels in \code{x_group_by}, which shows the relative number of its observations.
#' @param use A character specifying where to obtain the data from. Valid input for SingleCellExperiment object: ("colData", "rowData"). Valid input for Seurat object: ("meta.data" for sample metadata, "meta.feature" for feature metadata.)
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the components junk.
#' @param y_group_by_title The title of the y-axis.
#' @param x_group_by_title The title of the x-axis.
#' @param transmitter A character defining the id of an existing transmitter to obtain the data from.
#'
#' @name barplot
#' @rdname barplot
#' @exportMethod barplot
setGeneric("barplot", function(dashboard, object, ...) standardGeneric("barplot"))

#' Renders a component containing a boxplot
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param x A data.frame (matrix) containing numeric observations for the horizontal axis or a character vector indicating the columns to use from \code{use}.
#' @param group_by An optional data.frame (matrix) with columns containing grouping factors for the vertical axis or a character vector indicating the columns to use from \code{use}
#' @param use A character specifying where to obtain the data from. Valid input for SingleCellExperiment object: ("colData", "rowData"). Valid input for Seurat object: ("meta.data" for sample metadata, "meta.feature" for feature metadata.)
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the components junk.
#' @param x_title The title of the x-axis.
#' @param group_by_title The title of the y-axis.
#' @param transmitter A character defining the id of an existing transmitter to obtain the data from.
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
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param use_dimred A data.frame (matrix) containing coordinates of the reduced dimensions or a string indicating a dimension reduction from "reductions" of a Seurat \code{object}. Rownames are used as sample labels.
#' @param sample_metadata Sample metadata in columns and samples in rows (see Details).
#' @param group_by A string indicating a column in \code{metadata} that is used to group observations.
#' @param page The name of the page to be added.
#' @param title The title of the page.
#' @param labels An optional vector with sample labels.
#' @param show_group_sizes A logical value indicating if a barplot showing the number of observations from \code{group_by} will be created (default \code{TRUE}).
#' @param show_silhouette A logical value indicating if a silhouette plot should be shown (default \code{FALSE}).
#' @param menu The name of the menu, under which the page should appear.
#'
#' @details The parameters \code{use_dimred}, \code{sample_metadata} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{Seurat::Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#' @name dimred-sample-page
#' @rdname dimred-sample-page
#' @exportMethod add_dimred_sample_page
setGeneric("add_dimred_sample_page", function(dashboard, object, ...) standardGeneric("add_dimred_sample_page"))

#' Renders a component containing a heatmap based on \link[ComplexHeatmap]{Heatmap}.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns or a character indicating which assay of the \linkS4class{SingleCellExperiment::SingleCellExperiment} object to use.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use.
#' @param column_split An optional data.frame (matrix) containing factorial metadata (e.g. cluster, timepoint, etc.) along samples for splitting the columns or a character vector indicating the columns to use from the "meta.data" of a Seurat \code{object}/ the columns to use from "colData" of a SingleCellExperiment. Enables a second visualisation option: heatmap with levels of \code{column_split} as columns and features of \code{exprs_values} as rows.
#' @param visualisation_mode Used in case of the static mode and if \code{column_split} is provided: Select between "splitted" or "summarized" heatmap visualisation according to the levels in the 1. column of \code{column_split}.
#' @param title Title of the component.
#' @param cluster_rows A logical controls whether to make cluster on rows.
#' @param cluster_columns A logical controls whether to make cluster on columns.
#' @param clustering_distance A pre-defined character which is in ("euclidean", "maximum", "manhattan", "binary", "minkowski").
#' @param clustering_method Method to perform hierarchical clustering, pass to \link[stats]{hclust} ("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median","centroid").
#'
#' @name heatmap
#' @rdname heatmap
#' @exportMethod heatmap
setGeneric("heatmap", function(dashboard, object, ...) standardGeneric("heatmap"))

#' Renders a component containing a scatterplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param x Values that will be mapped to the x-axis (see Details).
#' @param y Values that will be mapped to the y-axis (see Details).
#' @param use A character specifying where to obtain the data from \code{object} (see Details).
#' @param use_dimred A character vector indicating the reduced dimension to use from \code{"object"} (see Details).
#' @param assay A character defining the assay of \code{object} and is used for obtaining the \code{exprs_values} (default "RNA") (see Details).
#' @param slot A character defining the data slot of \code{assay}.
#' @param colour_by Numeric or factorial values that will be used for colouring.
#' @param labels An optional vector with sample names. A dropdown menu for colouring by label will be provided.
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#' @param source A character defining the id used in plotly's source argument of the component.
#' @param transmitter A character defining the id of an existing transmitter to obtain the data from.
#'
#' @details The parameters \code{x}, \code{y}, \code{colour_by}, \code{use}, \code{use_dimred}, \code{exprs_values}, \code{assay} and \code{slot}) take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters \code{x}, \code{y}, \code{colour_by} and \code{exprs_values} are expected to be of class \code{data.frame} or \code{matrix}. T he parameters \code{x}, \code{y} can also be a numeric vector. The parameters \code{use}, \code{use_dimred}, \code{assay} and \code{slot} can be ignored.
#'   In case a \linkS4class{SingleCellExperiment::SingleCellExperiment} object is supplied, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item \code{use} "colData", "rowData", "reducedDim",
#'     \item \code{use_dimred} the name of an item in \code{reducedDims(object)},
#'     \item \code{exprs_values} a valid assay name from \code{assayNames(object)},
#'     \item \code{colour_by} column names of \code{colData(object)} or \code{colData(object)} in dependence of \code{use}.
#'   }
#'   In case a \linkS4class{Seurat::Seurat} object is supplied, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item \code{use} "meta.data" for sample metadata, "meta.feature" for feature metadata, "reduction" for a dimension reduction,
#'     \item \code{use_dimred} the name of an item in \code{object@reductions},
#'     \item \code{assay} a valid assay name from \code{names(object@assays)},
#'     \item \code{slot} a valid data slot from \code{assay},
#'     \item \code{colour_by} column names of \code{use}.
#'   }
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

#' Renders a component containing a shiny verbatim_text.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param transmitter A character defining the id of an existing transmitter to obtain the data from.
#' @param event The type of plotly event. (see \code{?plotly::event_data()}.
#' @param title Title of the component.
#'
#' @name verbatim_text
#' @rdname verbatim_text
#' @exportMethod verbatim_text
setGeneric("verbatim_text", function(dashboard, object, ...) standardGeneric("verbatim_text"))
