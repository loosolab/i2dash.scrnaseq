#' @rdname boxplot
#' @return A string containing markdown code for the rendered component
setMethod("boxplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, x, group_by = NULL, title = NULL, x_title = NULL, group_by_title = NULL, transmitter = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            if(!is.null(transmitter)){
              assertive.types::assert_is_character(transmitter)
              transmitter %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> transmitter
            }

            assertive.types::assert_is_any_of(x, c("data.frame", "matrix"))
            if(is.null(colnames(x))) colnames(x) <- paste0("V", 1:ncol(x))

            if(!is.null(group_by)){
              assertive.types::assert_is_any_of(group_by, c("data.frame", "matrix"))
              if(is.null(colnames(group_by))) colnames(group_by) <- paste0("V", 1:ncol(group_by))
              if(nrow(x) != nrow(group_by)) stop("The numbers of rows in 'x' and 'group_by' are not equal.")
            }

            # Create component environment
            env <- new.env()

            env$x <- x
            env$x_selection <- length(x) > 1

            env$group_by <- group_by
            env$group_by_selection <- length(group_by) > 1

            env$x_title <- x_title
            env$group_by_title <- group_by_title
            env$transmitter <- transmitter # the id of an existing transmitter to obtain the data from

            # save environment report
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "boxplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })

#' @rdname boxplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("boxplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use = c("colData", "rowData"), x = NULL, group_by = NULL, ...) {
            use <- match.arg(use)
            if(use == "colData") {
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> x
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "rowData") {
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> x
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            boxplot(dashboard,
                    x = x,
                    group_by = group_by,
                    ...)
          })

#' @rdname boxplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("boxplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use = c("meta.data", "meta.features"), assay = "RNA", x = NULL, group_by = NULL, ...) {
            use <- match.arg(use)
            if(use == "meta.data") {
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(object@meta.data))
                object@meta.data %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                object@meta.data %>%
                  as.data.frame() -> x
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by,  colnames(object@meta.data))
                object@meta.data %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "meta.features") {
              if(!is.null(x)) {
                assertive.sets::assert_is_subset(x, colnames(object[[assay]]@meta.features))
                object[[assay]]@meta.features %>%
                  as.data.frame() %>%
                  dplyr::select(!!x) -> x
              } else {
                object[[assay]]@meta.features %>%
                  as.data.frame() -> x
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(object[[assay]]@meta.features))
                object[[assay]]@meta.features %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            boxplot(dashboard,
                    x = x,
                    group_by = group_by,
                    ...)
          })
