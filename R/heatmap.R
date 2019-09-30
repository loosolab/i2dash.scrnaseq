#' Renders a component containing a heatmap based on \link[ComplexHeatmap]{Heatmap}.
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param exprs_values A data.frame (matrix) containing expression data of features of interest in rows and samples in columns or a character indicating which assay of the \linkS4class{SingleCellExperiment::SingleCellExperiment} object to use.
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param column_split An optional data.frame (matrix) with columns containing grouping factors for spliting the columns or a character vector containing the column names of \code{colData(object)} to use. Enables a second visualisation option: heatmap with levels of \code{column_split} as columns and features of \code{exprs_values}.
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

#' @rdname heatmap
#' @return A string containing markdown code for the rendered component
setMethod("heatmap",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   exprs_values,
                   column_split = NULL,
                   visualisation_mode = "splitted",
                   title = NULL,
                   legend_title  = NULL,
                   cluster_rows = FALSE,
                   cluster_columns = FALSE,
                   clustering_distance = c("euclidean", "maximum", "manhattan", "binary", "minkowski"),
                   clustering_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median","centroid")) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            if(class(exprs_values) == "dgCMatrix") exprs_values <- as.matrix(exprs_values)
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))
            if(is.null(colnames(exprs_values))) colnames(exprs_values) <- paste0("V", 1:ncol(exprs_values))

            if(!is.null(column_split)) {
              assertive.types::assert_is_any_of(column_split, c("data.frame", "matrix"))
              column_split %<>%
                as.data.frame() %>%
                dplyr::select_if(is.factor)
              if(is.null(colnames(column_split))) colnames(column_split) <- paste0("C", 1:ncol(column_split))
              if(ncol(exprs_values) != nrow(column_split)) stop("The number of columns in 'exprs_values' and rows in 'column_split' are not equal.")
            }
            clustering_distance <- match.arg(clustering_distance)
            clustering_method <- match.arg(clustering_method)

            # Create component environment
            env <- new.env()

            env$exprs_values <- exprs_values
            env$column_split <- column_split
            env$visualisation_mode <- visualisation_mode
            env$legend_title <- legend_title
            env$cluster_rows <- cluster_rows
            env$cluster_columns <- cluster_columns
            env$clustering_distance <- clustering_distance
            env$clustering_method <- clustering_method

            # save environment
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "heatmap.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname heatmap
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @param features A vector with features of interest identical to featurenames in \code{exprs_values} for subsetting \code{exprs_values}.
#' @export
setMethod("heatmap",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   exprs_values = "counts",
                   features,
                   column_split = NULL,
                   ...) {


            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            exprs_values <- SummarizedExperiment::assay(object, exprs_values)
            exprs_values <- exprs_values[features,]
            #
            # create data.frame for colour_by
            #
            if(!is.null(column_split)) {
              assertive.sets::assert_is_subset(column_split, colnames(SummarizedExperiment::colData(object)))
              SummarizedExperiment::colData(object) %>%
                as.data.frame() %>%
                dplyr::select(!!column_split) -> column_split
            } else {
              SummarizedExperiment::colData(object) %>%
                as.data.frame() -> column_split
            }

            heatmap(dashboard,
                    exprs_values = exprs_values,
                    column_split = column_split,
                    ...)
          })
