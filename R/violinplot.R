#' @rdname violinplot
#' @return A string containing markdown code for the rendered component
setMethod("violinplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, y, group_by = NULL,  title = NULL, y_title = NULL, group_by_title = NULL, transmitter = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            if(!is.null(transmitter)){
              assertive.types::assert_is_character(transmitter)
              transmitter %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> transmitter
            }

            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            if(is.null(colnames(y))) colnames(y) <- paste0("V", 1:ncol(y))
            if(is.null(rownames(y))) rownames(y) <- 1:nrow(y)

            if(!is.null(group_by)){
              assertive.types::assert_is_any_of(group_by, c("data.frame", "matrix"))
              if(is.null(colnames(group_by))) colnames(group_by) <- paste0("V", 1:ncol(group_by))
              if(is.null(rownames(group_by))) rownames(group_by) <- 1:nrow(group_by)
              if(nrow(y) != nrow(group_by)) stop("The numbers of rows in 'y' and 'group_by' are not equal.")
            }

            # Create component environment
            env <- new.env()

            env$y <- y
            env$y_selection <- length(y) > 1

            env$group_by <- group_by
            env$group_by_selection <- length(group_by) > 1

            env$y_title <- y_title
            env$group_by_title <- group_by_title
            env$transmitter <- transmitter

            # save environment
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "violinplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname violinplot
#' @export
setMethod("violinplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, from = c("colData", "rowData"), y = NULL, group_by = NULL, ...) {
            from <- match.arg(from)

            data <- switch(from,
                           "colData" = SummarizedExperiment::colData(object),
                           "rowData" = SummarizedExperiment::rowData(object))

            if(!is.null(y)) {
              assertive.sets::assert_is_subset(y, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y) -> y
            } else {
              data %>%
                as.data.frame() -> y
            }
            if(!is.null(group_by)) {
              assertive.sets::assert_is_subset(group_by, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!group_by) -> group_by
            }

            violinplot(dashboard,
                       y = y,
                       group_by = group_by,
                       ...)
          })

#' @rdname violinplot
#' @export
setMethod("violinplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, from = c("meta.data", "meta.features"), assay = "RNA", y = NULL, group_by = NULL, ...) {
            from <- match.arg(from)

            data <- switch(from,
                           "meta.data" = object@meta.data,
                           "meta.features" = object[[assay]]@meta.features)

            if(!is.null(y)) {
              assertive.sets::assert_is_subset(y, colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!y) -> y
            } else {
              data %>%
                as.data.frame() -> y
            }

            if(!is.null(group_by)) {
              assertive.sets::assert_is_subset(group_by,  colnames(data))
              data %>%
                as.data.frame() %>%
                dplyr::select(!!group_by) -> group_by
            }
            violinplot(dashboard,
                       y = y,
                       group_by = group_by,
                       ...)
          })
