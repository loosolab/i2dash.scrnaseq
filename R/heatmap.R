#' @rdname heatmap
#' @return A string containing markdown code for the rendered component
setMethod("heatmap",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   exprs_values,
                   split_by = NULL,
                   aggregate_by = NULL,
                   title = NULL,
                   legend  = NULL,
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

            if(!is.null(split_by)) {
              assertive.types::assert_is_any_of(split_by, c("data.frame", "matrix"))

              # retain only columns that contain factors
              split_by %<>%
                as.data.frame() %>%
                dplyr::select_if(is.factor)
              if(is.null(colnames(split_by))) colnames(split_by) <- paste0("C", 1:ncol(split_by))
              if(ncol(exprs_values) != nrow(split_by)) stop("The number of columns in 'exprs_values' and rows in 'split_by' are not equal.")
            }

            if(!is.null(aggregate_by)) {
              assertive.types::assert_is_any_of(aggregate_by, c("data.frame", "matrix"))

              # retain only columns that contain factors
              aggregate_by %<>%
                as.data.frame() %>%
                dplyr::select_if(is.factor)
              if(is.null(colnames(aggregate_by))) colnames(aggregate_by) <- paste0("C", 1:ncol(aggregate_by))
              if(ncol(exprs_values) != nrow(aggregate_by)) stop("The number of columns in 'exprs_values' and rows in 'split_by' are not equal.")
            }

            clustering_distance <- match.arg(clustering_distance)
            clustering_method <- match.arg(clustering_method)

            # Create component environment
            env <- new.env()

            env$exprs_values <- exprs_values
            env$split_by <- split_by
            env$aggregate_by <- aggregate_by
            env$legend_title <- legend
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
#' @export
setMethod("heatmap",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   exprs_values = "counts",
                   subset_row = NULL,
                   split_by = NULL,
                   aggregate_by = NULL,
                   ...) {


            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            exprs_values <- SummarizedExperiment::assay(object, exprs_values)

            # Subset to requested features
            if(!is.null(subset_row)) exprs_values <- exprs_values[subset_row, ]

            # Create data.frames for splitting and aggregation
            if(!is.null(split_by)) {
              assertive.sets::assert_is_subset(split_by, colnames(SummarizedExperiment::colData(object)))
              SummarizedExperiment::colData(object) %>%
                as.data.frame() %>%
                dplyr::select(!!split_by) -> split_by
            }

            if(!is.null(aggregate_by)) {
              assertive.sets::assert_is_subset(aggregate_by, colnames(SummarizedExperiment::colData(object)))
              SummarizedExperiment::colData(object) %>%
                as.data.frame() %>%
                dplyr::select(!!aggregate_by) -> aggregate_by
            }

            heatmap(dashboard = dashboard,
                    exprs_values = exprs_values,
                    split_by = split_by,
                    aggregate_by = aggregate_by,
                    ...)
          })

#' @rdname heatmap
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("heatmap",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, assay = "RNA", assay_slot = "data", subset_row, column_split = NULL, ...) {

            assertive.types::assert_is_character(assay)
            assertive.types::assert_is_character(assay_slot)
            assertive.sets::assert_is_subset(assay, names(object@assays))

            # exprs_values
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            #
            # create data.frame for column_split
            #
            if(!is.null(column_split)) {
              assertive.sets::assert_is_subset(column_split,  colnames(object@meta.data))
              object@meta.data %>%
                as.data.frame() %>%
                dplyr::select(!!column_split) -> column_split
            } else {
              object@meta.data %>%
                as.data.frame() -> column_split
            }

            heatmap(dashboard = dashboard,
              exprs_values = exprs_values,
              column_split = column_split,
              ...)
          })