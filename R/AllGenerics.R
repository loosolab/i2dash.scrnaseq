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
#' @param use_dimred A data.frame (matrix) containing coordinates of the reduced dimension or a character representing a valid \code{reducedDim} slot of a SingleCellExperiment \code{object}/ a name of the \linkS4class{Seurat::DimReduc} object in \code{object@reductions}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns.
#' @param feature_metadata A data.frame (matrix) along rows of \code{exprs_values} containing feature metadata, or a character vector indicating columns from \code{rowData(object)} or \code{object[[assay]]@feature.data}.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
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
#' @param object A \linkS4class{SingleCellExperiment::SingleCellExperiment} object or a \linkS4class{Seurat::Seurat} object.
#' @param use_dimred A data.frame (matrix) containing coordinates of the reduced dimension or a character representing a valid \code{reducedDim} slot of the SingleCellExperiment \code{object}/ a name of the \linkS4class{Seurat::DimReduc} object in \code{object@reductions}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns.
#' @param group_by A data.frame (matrix) containing factorial metadata (e.g. cluster, timepoint, etc.) along samples or a character vector indicating the columns to use from the "meta.data" of a Seurat \code{object}.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#' @param labels A vector with optional sample labels that are used instead of \code{rownames(use_dimred)}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @name feature-expression-page
#' @rdname feature-expression-page
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
#' @param sample_metadata A data.frame (matrix) containing metadata (e.g. cluster, timepoint, number of features, etc) along samples or a character vector indicating the columns to use from the "meta.data" of a Seurat \code{object}.
#' @param group_by A string indicating a column in \code{metadata} that is used to group observations.
#' @param title The title of the page.
#' @param labels An optional vector with sample labels.
#' @param show_group_sizes A logical value indicating if a barplot showing the number of observations from \code{group_by} will be created (default \code{TRUE}).
#' @param show_silhouette A logical value indicating if a silhouette plot should be shown (default \code{FALSE}).
#' @param menu The name of the menu, under which the page should appear.
#'
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
#' @param x A data.frame (matrix) containing columns with numeric values that will be mapped to the x-axis.
#' @param y A data.frame (matrix) containing columns with numeric values that will be mapped to the y-axis.
#' @param use A character specifying where to obtain the data from. Valid input for SingleCellExperiment object: ("colData", "rowData", "reducedDim"). Valid input for Seurat object: ("meta.data" for sample metadata, "meta.feature" for feature metadata, "reduction" for a dimension reduction)
#' @param use_dimred A character vector indicating the reduced dimension to use from "reducedDim"/ \"reduction".
#' @param assay Necessery, if \code{use} = "meta.feature". A character defining the assay to obtain the feature metadata from (default "RNA").
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

#' Explore the effects of the parameters "theta" and "perplexity" of a t-stochastic neighbour embedding.
#'
#' Creates a page with two tabs and a sidebar. The first tab generates the dimension reduction plot of the t-SNE with the parameters set in the sidebar by clicking the "Generate plot" button. It is possible to save the scatterplot with its parameters to compare it with another parameters by clicking the button "Add plot for comparison". The second tab "Compare selected plots" contains a grid layout with the saved reduced dimensions. A list in the sidebar shows the saved plots and enables the deletion of plots.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param exprs_values A numeric matrix of log-expression values where rows are features and columns are cells. Alternatively, a character indicating which assay of the SummarizedExperiment, SingleCellExperiment or Seurat object provided in \code{object} to use.
#' @param seed An integer vector, containing the random number generator (RNG) state for random number generation with \code{set.seed()}.
#' @param page A page name to identify this page.
#' @param title The title of the page.
#' @param menu The menu tab to which this page is subordinated. Default "Tools"
#' @param object An object of class \linkS4class{Seurat::Seurat}, \linkS4class{SingleCellExperiment::SingleCellExperiment} or \linkS4class{SummarizedExperiment::SummarizedExperiment}.
#' @param assay A character specifying the assay (\code{object@assays}) to obtain expression values from. (Default: "RNA")
#' @param assay_slot A character specifying the name of the data slot in the assay. (Default: "data")
#'
#' @name tsne-comparison-page
#' @rdname tsne-comparison-page
#' @exportMethod add_tsne_comparison_page
setGeneric("add_tsne_comparison_page", function(dashboard, object, ...) standardGeneric("add_tsne_comparison_page"))
