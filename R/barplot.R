#' @rdname barplot
#' @return A string containing markdown code for the rendered component
setMethod("barplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, y_group_by, x_group_by = NULL, title = NULL, x_group_by_title = NULL, y_group_by_title = NULL) {
            # Create random env i
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(y_group_by, c("data.frame", "matrix"))
            if(is.null(colnames(y_group_by))) colnames(y_group_by) <- paste0("V", 1:ncol(y_group_by))

            if(!is.null(x_group_by)){
              assertive.types::assert_is_any_of(x_group_by, c("data.frame", "matrix"))

              if(is.null(colnames(x_group_by))) colnames(x_group_by) <- paste0("V", 1:ncol(x_group_by))

              if(nrow(y_group_by) != nrow(x_group_by)) stop("The numbers of rows in 'x_group_by' and 'y_group_by' are not equal.")

              # Columns are swapped in case of equal column names to prevent visualization of the same column (always the first one) on both axes.
              if(ncol(x_group_by) > 1 && colnames(y_group_by)[1] == colnames(x_group_by)[1]) {
                if(ncol(x_group_by) > 2) {
                  x_group_by <-  x_group_by[, c(2, 1, c(3:ncol(x_group_by)))]
                } else {
                  x_group_by <-  x_group_by[, c(2, 1)]
                }
              }
            }

            # Create component environment
            env <- new.env()

            env$y_group_by <- y_group_by
            env$y_group_by_selection <- length(y_group_by) > 1

            env$x_group_by <- x_group_by
            env$x_group_by_selection <- length(x_group_by) > 1

            env$x_group_by_title <- x_group_by_title
            env$y_group_by_title <- y_group_by_title

            # save environment report
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "barplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname barplot
#' @export
setMethod("barplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, y_group_by = NULL, x_group_by = NULL, from = c("colData", "rowData"), ...) {
            from <- match.arg(from)

            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object))

            if(!is.null(y_group_by)) {
              assertive.sets::assert_is_subset(y_group_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y_group_by) -> y_group_by
            } else {
              data %>%
                as.data.frame() -> y_group_by
            }

            if(!is.null(x_group_by)) {
              assertive.sets::assert_is_subset(x_group_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!x_group_by) -> x_group_by
            }

            barplot(dashboard,
                    y_group_by = y_group_by,
                    x_group_by = x_group_by,
                    ...)
          })

#' @rdname barplot
#' @export
setMethod("barplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, y_group_by = NULL, x_group_by = NULL, from = c("meta.data", "meta.features"), assay = "RNA", ...) {
            from <- match.arg(from)

            data <- switch(from,
                           "meta.data" = object@meta.data,
                           "meta.features" = object[[assay]]@meta.features)

            if(!is.null(y_group_by)) {
              assertive.sets::assert_is_subset(y_group_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y_group_by) -> y_group_by
            } else {
              data %>%
                as.data.frame() -> y_group_by
            }

            if(!is.null(x_group_by)) {
              assertive.sets::assert_is_subset(x_group_by,  colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!x_group_by) -> x_group_by
            }

            barplot(dashboard,
                    y_group_by = y_group_by,
                    x_group_by = x_group_by,
                    ...)
          })
