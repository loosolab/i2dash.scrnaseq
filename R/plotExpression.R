#' @rdname plotExpression
#' @return A string containing markdown code for the rendered component
setMethod("plotExpression",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   exprs_values,
                   features = NULL, # vector of features (like subset_row)
                   metadata = NULL, # data.frame
                   x = NULL, # a feature or column name from "metadata"
                   ... # further parameter provided to scater::plotExpression
          ) {
            # validate input
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))
            if(!is.null(features) & all(feautres %in% rownames(exprs_values))){
              exprs_values <- exprs_values[features,]
            }
            if(!is.null(metadata)){
              assertive.types::assert_is_any_of(metadata, c("data.frame", "matrix"))
              if(is.null(colnames(metadata))) colnames(metadata) <- paste0("V", 1:ncol(metadata))
              if(ncol(exprs_values) != nrow(metadata)) stop("The number of rows in 'exprs_values' and the number of columns in 'metadata' should be equal.")
            }
            # create SingleCellExperiment
            sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = exprs_values), colData = metadata)

            plotExpression(
              dashboard = dashboard,
              object = sce,
              exprs_values = "counts",
              features = colnames(exprs_values),
              x = x,
              metadata = colnames(SingleCellExperiment::colData(sce)),
              ...
            )
          })

#' @rdname plotExpression
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("plotExpression",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   exprs_values = "logcounts",
                   features, # vector of features (like subset_row)
                   metadata = NULL, # vector of characters identifying column names from colData
                   x = NULL, # a feature or column name from "metadata"
                   as_plotly = FALSE,
                   title = NULL,
                   x_title = NULL,
                   y_title = NULL,
                   plot_title = NULL,
                   ncol = 2,
                   scales = "fixed",
                   ... # further parameter provided to scater::plotExpression
                   ) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.sets::assert_is_subset(features, rownames(object))
            object <- object[features,]
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            if(!is.null(x)) # todo: check if in colnames(colData(object)) or rownames(object)
            if(!is.null(metadata)) assertive.sets::assert_is_subset(metadata, colnames(SummarizedExperiment::colData(object)))
            valid_arguments <- names(as.list(args(scater::plotExpression)))
            params <- list(...)
            params <- list("col" = 2, "by" = "a")
            common <- intersect(names(params), valid_arguments)
            if(length(common) == 0) common <- list() else common <- params[common]

            # Create component environment
            env <- new.env()
            env$object <- object
            env$exprs_values <- exprs_values
            env$features <- features
            env$metadata <- metadata
            env$x <- x
            env$as_plotly <- as_plotly
            env$x_title <- x_title
            env$y_title <- y_title
            env$plot_title <- plot_title
            env$ncol <- ncol
            env$scales <- scales
            env$params <- common

            # save environment
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "plotExpression.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname plotExpression
#' @return A string containing markdown code for the rendered component
setMethod("plotExpression",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard,
                   object,
                   ...) {

            sce <- Seurat::as.SingleCellExperiment(object)

            plotExpression(
              dashboard = dashboard,
              object = sce,
              ...
            )
          })
