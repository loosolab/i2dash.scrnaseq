#' @importClassesFrom i2dash i2dashboard
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom Seurat Seurat
NULL

#' Create expression visualization for multiple selected features on a regular grid.
#'
#' Users can select features and a dimension reduction to plot feature expression values.
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object A \linkS4class{SingleCellExperiment} object or a \linkS4class{Seurat} object.
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param page The name of the page to be added.
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment} or \linkS4class{Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name item(s) in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#'
#' @rdname feature-grid-page
#' @exportMethod add_feature_grid_page
setGeneric("add_feature_grid_page", function(dashboard, object, ...) standardGeneric("add_feature_grid_page"))

#' Add a dimension reduction page with feature metadata
#'
#' This function adds a page with two linked components to the \code{dashboard} object: A scatterplot, showing samples in along two-dimensional coordinates, and a table, showing feature metadata. A click on a feature in the table updates the scatterplot with the feature expression.
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object A \linkS4class{SingleCellExperiment} object or a \linkS4class{Seurat} object.
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param feature_metadata A data.frame (matrix) along rows of \code{exprs_values} containing feature metadata, or a character vector indicating columns from \code{rowData(object)} or \code{object[[assay]]@feature.data}.
#' @param page The name of the page to be added.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from.
#' @param assay_slot A character specifying the name of the data slot in the assay.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param subset_row An optional character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment} or \linkS4class{Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#'
#' @rdname dimred-feature-page
#' @exportMethod add_dimred_feature_page
setGeneric("add_dimred_feature_page", function(dashboard, object, ...) standardGeneric("add_dimred_feature_page"))

#' Add a feature expression page.
#'
#' This function adds a page with two linked components to the \code{dashboard} object: A scatterplot, showing samples in along two-dimensional coordinates, and a violin plot, showing feature expression values by groups defined in \code{group_by}.
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param page The name of the page to be added.
#' @param use_dimred Coordinates of the reduced dimensions, used for the scatterplot (see Details).
#' @param exprs_values Expression data of features of interest in rows and samples in columns (see Details).
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from (see Details).
#' @param group_by Data along samples that is used for grouping expression values in the violin plot (see Details).
#' @param object A valid \linkS4class{SingleCellExperiment} or \linkS4class{Seurat} object.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param labels A vector with optional sample labels that are used instead of \code{rownames(use_dimred)}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{use_dimred}, \code{exprs_values} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment} or \linkS4class{Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of a item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#'
#' @rdname feature-expression-page
#' @exportMethod add_feature_expression_page
setGeneric("add_feature_expression_page", function(dashboard, object, ...) standardGeneric("add_feature_expression_page"))

#' Renders a component containing a vertical violin plot
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param y Data containing numeric observations for the vertical axis (see Details).
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
#' @param group_by Optionally provide data containing grouping factors for the vertical axis.
#' @param from A character specifying where to obtain the data from (see Details).
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the component.
#' @param y_title The title of the y-axis.
#' @param group_by_title The title of the x-axis.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{y}, \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment}- or \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat} object.
#'   }
#'   In both cases, \code{y} and \code{group_by} take column names of \code{from}.
#'
#' @name violinplot
#' @rdname violinplot
#' @exportMethod violinplot
setGeneric("violinplot", function(dashboard, object, ...) standardGeneric("violinplot"))

#' Renders a component containing a horizontal barplot.
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
#' @param y_group_by Data containing grouping factors for the vertical axis.
#' @param x_group_by Optionally provide data containing grouping factors for the horizontal axis. The result is a barplot grouped by the levels in \code{x_group_by} and shows the relative number of its observations.
#' @param from A character specifying where to obtain the data from (see Details).
#' @param title The title of the components junk.
#' @param y_group_by_title The title of the y-axis.
#' @param x_group_by_title The title of the x-axis.
#' @param assay A character defining the assay of \code{object} and is used for obtaining the meta data of the respective assay (default "RNA") (see Details).
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{y_group_by}, \code{x_group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment} or \emph{i2dashboard,Seurat} method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item \code{from} can either be \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat} object.
#'   }
#'   In both cases, \code{y_group_by} and \code{x_group_by} take column names of \code{from}.
#'
#' @name barplot
#' @rdname barplot
#' @exportMethod barplot
setGeneric("barplot", function(dashboard, object, ...) standardGeneric("barplot"))

#' Renders a component containing a boxplot
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
#' @param x Data containing numeric observations for the horizontal axis.
#' @param group_by Optionally provide data containing grouping factors for the vertical axis..
#' @param from A character specifying where to obtain the data from (see Details).
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
#' @param title The title of the components junk.
#' @param x_title The title of the x-axis.
#' @param group_by_title The title of the y-axis.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{x}, \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case of the \emph{i2dashboard,missing}-method, the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case of the \emph{i2dashboard,SingleCellExperiment}- or \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character} and
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"} or \code{"rowData"} for a \linkS4class{SingleCellExperiment} object or
#'     \item \code{"meta.data"} or \code{"meta.features"} for a \linkS4class{Seurat} object.
#'   }
#'   In both cases, \code{x} and \code{group_by} take column names of \code{from}.
#'
#' @name boxplot
#' @rdname boxplot
#' @exportMethod boxplot
setGeneric("boxplot", function(dashboard, object, ...) standardGeneric("boxplot"))

#' Renders a component containing a scatterplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
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
#' @param subset_row An optional character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param ... Further parameters passed to the core function.
#'
#' @details The parameters \code{x}, \code{y}, \code{colour_by}, \code{use_dimred}, \code{exprs_values}, \code{assay} and \code{slot}) take different arguments depending on the class of \code{object}.
#'   In case the \emph{i2dashboard,missing}-method, the parameters \code{x}, \code{y}, \code{colour_by} and \code{exprs_values} are expected to be of class \code{data.frame} or \code{matrix}. The parameters \code{x}, \code{y} can also be numeric vectors. The parameters \code{from}, \code{use_dimred}, \code{assay} and \code{slot} can be ignored.
#'   In case the \emph{i2dashboard,SingleCellExperiment}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"}, \code{"rowData"} or \code{"reducedDim"}
#'     \item \code{use_dimred} the name of an item in \code{reducedDims(object)}
#'     \item \code{exprs_values} a valid assay name from \code{assayNames(object)}
#'   }
#'   In case of the \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"meta.data"} for sample metadata, \code{"meta.feature"} for feature metadata, \code{"embedding"} for a dimension reduction
#'     \item \code{reduction} the name of an item in \code{object@reductions}
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
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns or a string representing the name of an \code{assay} of \code{object}.
#' @param object A valid \linkS4class{SingleCellExperiment} object.
#' @param subset_row An optional character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use the first 100 features.
#' @param split_by An optional data.frame (matrix) containing grouping factors for spliting columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param aggregate_by An optional data.frame (matrix) containing grouping factors for aggregating columns of the heatmap. In case of \code{i2dashboard,SingleCellExperiment}, should be column names of \code{colData(object)}.
#' @param title Title of the component.
#' @param legend Title of the heatmap legend.
#' @param cluster_rows Whether or not to perform clustering on rows.
#' @param cluster_columns Whether or not to perform clustering on columns
#' @param clustering_method Method to perform hierarchical clustering, passed to \link[stats]{hclust}.
#' @param clustering_distance The distance measure to use for hierarchical clustering.
#' @param show_column_names Whether or not to show column names. Note that large number of column names can cause visualization problems.
#' @param column_title The column title of the heatmap.
#' @param row_title The row title of the heatmap.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param ... Further parameters passed to the core function.
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
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
#' @param use_dimred Data containing coordinates of the reduced dimensions or a string indicating a dimension reduction from "reductions" of a Seurat \code{object}. Rownames are used as sample labels.
#' @param sample_metadata  Sample metadata in columns and samples in rows (see Details).
#' @param group_by A string indicating a column in \code{sample_metadata} that is used to group observations.
#' @param page The name of the page to be added.
#' @param title The title of the page.
#' @param labels An optional vector with sample labels.
#' @param show_group_sizes A logical value indicating if a barplot showing the number of observations from \code{group_by} will be creaed (default \code{TRUE}).
#' @param show_silhouette A logical value indicating if a silhouette plot should be shown (default \code{FALSE}).
#' @param menu The name of the menu, under which the page should appear.
#' @param ... Further parameters passed to the core function.
#'
#'@details The parameters \code{use_dimred}, \code{sample_metadata} (or \code{assay}) and \code{group_by} take different arguments depending on the class of \code{object}.
#'   In case no object is supplied (\emph{i2dashboard,missing}-method), the parameters are expected to be of class \code{data.frame} or \code{matrix}.
#'   In case a \linkS4class{SingleCellExperiment} or \linkS4class{Seurat} object is supplied, the parameters are expected to be of class \code{character}, containing
#'   \itemize{
#'     \item the name of an item in \code{reducedDims(object)} or \code{object@reductions},
#'     \item a valid assay name from \code{assayNames(object)} or \code{names(object@assays)},
#'     \item column names of \code{colData(object)} or \code{object@meta.data}.
#'   }
#' @rdname dimred-sample-page
#' @exportMethod add_dimred_sample_page
setGeneric("add_dimred_sample_page", function(dashboard, object, ...) standardGeneric("add_dimred_sample_page"))

#' Quantify per-gene variation and explore the threshold on the metric of variation to get the desired set of highly variable features.
#'
#' Creates a page with a scatterplot of the variance of log-expression against the mean log-expression and a table with features and their metrics of variation. With shiny inputs you can color highlight the hvgs in dependency to the proportion and minimal threshold of the relevant variation metric. Also you can download the selcted hvgs or the entire variation metrics table.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param exprs_values A numeric matrix of log-counts, or a string indicating the assay of \code{object}.
#' @param use_function Choose the \code{scran} method to model the variance of the log-expression profiles for each gene.
#' @param title The title of the page.
#' @param menu The name of the menu, under which the page should appear.
#' @param assay In case of a \code{Seurat}-class \code{object} a string indicating the assay to use.
#' @param slot In case of a \code{Seurat}-class \code{object} a string indicating the assay to use.
#' @param ... Further parameters provided to the method specified in \code{use_function}.
#'
#' @name feature-selection-page
#' @rdname feature-selection-page
#' @exportMethod add_feature_selection_page
setGeneric("add_feature_selection_page", function(dashboard, object, ...) standardGeneric("add_feature_selection_page"))

#' Renders a component containing an HTML table of summarized sample or feature metadata of single-cell experiment objects or data.frames.
#'
#' @param dashboard An object of class \linkS4class{i2dashboard}.
#' @param object An object of class \linkS4class{Seurat} or \linkS4class{SingleCellExperiment}.
#' @param data Data.frame containing numeric observations to summarize.
#' @param group_by A column name of a column containing grouping factors for grouping.
#' @param FUNS A named vector indicating summary functions.
#' @param caption The tables caption.
#' @param description The tables description.
#' @param title The title of the component.
#' @param columns The metadata columns to summarize.
#' @param assay A character defining the assay of \code{object}.
#' @param from A character specifying where to obtain the data from \code{object}, e.g. \code{colData}/\code{rowData}.
#' @param ... Further parameters passed to the core function.
#'
#' @rdname summarize_metadata
#' @exportMethod summarize_metadata
setGeneric("summarize_metadata", function(dashboard, object, ...) standardGeneric("summarize_metadata"))

#' Explore the effects of the parameters "theta" and "perplexity" of a t-stochastic neighbour embedding or "n_neighors" of a UMAP embedding.
#'
#' Creates a page with two tabs and a sidebar. The first tab generates the dimension reduction plot of the UMAP or t-SNE with the parameters set in the sidebar by clicking the "Generate plot" button. It is possible to save the scatterplot with its parameters to compare it with another parameters by clicking the button "Add plot for comparison". The second tab "Compare selected plots" contains a grid layout with the saved reduced dimensions. A list in the sidebar shows the saved plots and enables the deletion of plots.
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param exprs_values A numeric matrix of log-expression values where rows are features and columns are cells. Alternatively, a character indicating which assay of \code{object} to use.
#' @param seed An integer vector, containing the random number generator (RNG) state for random number generation with \code{set.seed()}.
#' @param calculateUMAP A list with parameters passed on to \code{scater::calculateUMAP()}
#' @param calculateTSNE A list with parameters passed on to \code{scater::calculateTSNE()}
#' @param page A page name to identify this page.
#' @param title The title of the page.
#' @param menu The menu tab to which this page is subordinated. Default "Tools"
#' @param object An object of class \linkS4class{Seurat}, \linkS4class{SingleCellExperiment} or \linkS4class{SummarizedExperiment}.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param subset_row An optional character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @param ... Further parameters passed to the core function.
#'
#' @rdname dimred-comparison-page
#' @exportMethod add_dimred_comparison_page
setGeneric("add_dimred_comparison_page", function(dashboard, object, ...) standardGeneric("add_dimred_comparison_page"))

#' Renders a component containing a bubbleplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param object An object of class \linkS4class{Seurat::Seurat} or \linkS4class{SingleCellExperiment::SingleCellExperiment}.
#' @param x Data that will be mapped to the x-axis (see Details).
#' @param y Data that will be mapped to the y-axis (see Details).
#' @param from A character specifying where to obtain the data from \code{object} (see Details).
#' @param use_dimred A character vector indicating the reduced dimension to use from \code{"object"} (see Details).
#' @param assay A character defining the assay of \code{object}.
#' @param colour_by Numeric or factorial values that will be used for colouring.
#' @param size Numeric or factorial values that describe the size of the bubbles.
#' @param labels An optional vector with sample names. A dropdown menu for colouring by label will be provided.
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#' @param plot_title An optional title of the plot.
#'
#' @details The parameters \code{x}, \code{y}, \code{size}, \code{colour_by}, \code{use_dimred} and \code{assay} take different arguments depending on the class of \code{object}.
#'   In case the \emph{i2dashboard,missing}-method, the parameters \code{x}, \code{y}, \code{size}, \code{colour_by} are expected to be of class \code{data.frame} or \code{matrix}. The parameters \code{x}, \code{y} and \code{size} can also be numeric vectors. The parameters \code{from}, \code{use_dimred}, \code{assay} and \code{slot} can be ignored.
#'   In case the \emph{i2dashboard,SingleCellExperiment}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"colData"}, \code{"rowData"} or \code{"reducedDim"}
#'     \item \code{use_dimred} the name of an item in \code{reducedDims(object)}
#'   }
#'   In case of the \emph{i2dashboard,Seurat}-method, the parameters are expected to be of class \code{character}:
#'   \itemize{
#'     \item the parameter \code{from} can be either \code{"meta.data"} for sample metadata, \code{"meta.feature"} for feature metadata, \code{"embedding"} for a dimension reduction
#'     \item \code{reduction} the name of an item in \code{object@reductions}
#'     \item \code{assay} a valid assay name from \code{names(object@assays)}
#'   }
#'   In both cases, \code{x}, \code{y}, \code{colour_by} take column names of \code{from}.
#'
#' @name bubbleplot
#' @rdname bubbleplot
#' @exportMethod bubbleplot
setGeneric("bubbleplot", function(dashboard, object, ...) standardGeneric("bubbleplot"))
