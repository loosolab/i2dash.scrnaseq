#' @rdname plotMetadata
#' @return A string containing markdown code for the rendered component
setMethod("plotMetadata",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   y,
                   x = NULL,
                   from = c("colData", "rowData"),
                   metadata = NULL,
                   ...) {

            from <- match.arg(from)

            # Validate input
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            if(is.null(colnames(y))) colnames(y) <- paste0("V", 1:ncol(y))

            if(!is.null(x)){
              assertive.types::assert_is_any_of(x, c("data.frame", "matrix"))
              if(is.null(colnames(x))) colnames(x) <- paste0("V", 1:ncol(x))
              if(nrow(y) != nrow(x)) stop("The numbers of rows of 'y' and 'x' should be equal.")
            }
            if(!is.null(metadata)){
              assertive.types::assert_is_any_of(metadata, c("data.frame", "matrix"))
              if(is.null(colnames(metadata))) colnames(metadata) <- paste0("V", 1:ncol(metadata))
              if(nrow(y) != nrow(x)) stop("The numbers of rows of 'y' and 'metadata' should be equal.")
            }

            # create SingleCellExperiment
            if(from == "colData"){
              counts <- matrix(rep(0,nrow(y)), ncol=nrow(y), nrow=1)
            } else {
              counts <- matrix(rep(0,nrow(y)), ncol=1, nrow=nrow(y))
            }
            sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = counts))

            # create SingleCellExperiment
            if(from == "colData"){
              counts <- matrix(rep(0,nrow(y)), ncol=nrow(y), nrow=1)
            } else {
              counts <- matrix(rep(0,nrow(y)), ncol=1, nrow=nrow(y))
            }
            sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = counts))

            data <- y
            if(!is.null(x)) data <- dplyr::bind_cols(data, x[setdiff(colnames(x), colnames(data))])
            if(!is.null(metadata)) data <- dplyr::bind_cols(data, metadata[setdiff(colnames(metadata), colnames(data))])

            if (from == "colData"){
              SummarizedExperiment::colData(sce) <- cbind(SummarizedExperiment::colData(sce), data)
            } else {
              SummarizedExperiment::rowData(sce) <- cbind(SummarizedExperiment::colData(sce), data)
            }

            plotMetadata(
              dashboard = dashboard,
              object = sce,
              from = from,
              y = colnames(y),
              x = colnames(x),
              metadata = colnames(metadata),
              ...
            )
          })

#' @rdname plotMetadata
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("plotMetadata",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   from = c("colData", "rowData"),
                   y, # vector of characters
                   x = NULL, # vector of characters
                   metadata = NULL, # vector of characters
                   group_by = NULL, # character
                   as_plotly = FALSE,
                   title = NULL,
                   x_title = NULL,
                   y_title = NULL,
                   plot_title = NULL) {

            from <- match.arg(from)

            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object))

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.sets::assert_is_subset(y, colnames(data))
            if(!is.null(x)) assertive.sets::assert_is_subset(x, colnames(data))
            if(!is.null(metadata)) assertive.sets::assert_is_subset(metadata, colnames(data))
            if(!is.null(group_by)) {
              assertive.types::assert_is_character(group_by)
              assertive.sets::assert_is_subset(group_by, colnames(data))
            }

            # Create component environment
            env <- new.env()
            env$from <- from
            env$object <- object
            env$y <- y
            env$x <- x
            env$metadata <- metadata
            env$group_by <- group_by
            env$as_plotly <- as_plotly
            env$x_title <- x_title
            env$y_title <- y_title
            env$plot_title <- plot_title

            # save environment
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "plotMetadata.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname plotMetadata
#' @return A string containing markdown code for the rendered component
setMethod("plotMetadata",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard,
                   object,
                   ...) {

            sce <- Seurat::as.SingleCellExperiment(object)

            plotMetadata(
              dashboard = dashboard,
              object = sce,
              ...
            )
          })
