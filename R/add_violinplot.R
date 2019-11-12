#' @rdname add_violinplot
#' @return The dashboard
setMethod("add_violinplot",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, y, group_by = NULL,  title = NULL, y_title = NULL, group_by_title = NULL, page = "default", menu = NULL) {

            page %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> name
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
            if(is.null(colnames(y))) colnames(y) <- paste0("V", 1:ncol(y))

            if(!is.null(group_by)){
              assertive.types::assert_is_any_of(group_by, c("data.frame", "matrix"))
              if(is.null(colnames(group_by))) colnames(group_by) <- paste0("V", 1:ncol(group_by))
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

            # save environment
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "violinplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)

            dashboard@pages[[name]]$components <- append(dashboard@pages[[name]]$components, expanded_component)
            return(dashboard)
          })

#' @rdname add_violinplot
#' @export
setMethod("add_violinplot",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use = c("colData", "rowData"), y = NULL, group_by = NULL, ...) {
            use <- match.arg(use)
            if(use == "colData") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::colData(object)))
                SummarizedExperiment::colData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "rowData") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::rowData(object)))
                SummarizedExperiment::rowData(object) %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            add_violinplot(dashboard,
                           y = y,
                           group_by = group_by,
                           ...)
          })

#' @rdname add_violinplot
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_violinplot",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use = c("meta.data", "meta.features"), assay = "RNA", y = NULL, group_by = NULL, ...) {
            use <- match.arg(use)
            if(use == "meta.data") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(object@meta.data))
                object@meta.data %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                object@meta.data %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by,  colnames(object@meta.data))
                object@meta.data %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            } else if (use == "meta.features") {
              if(!is.null(y)) {
                assertive.sets::assert_is_subset(y, colnames(object[[assay]]@meta.features))
                object[[assay]]@meta.features %>%
                  as.data.frame() %>%
                  dplyr::select(!!y) -> y
              } else {
                object[[assay]]@meta.features %>%
                  as.data.frame() -> y
              }
              if(!is.null(group_by)) {
                assertive.sets::assert_is_subset(group_by, colnames(object[[assay]]@meta.features))
                object[[assay]]@meta.features %>%
                  as.data.frame() %>%
                  dplyr::select(!!group_by) -> group_by
              }
            }
            add_violinplot(dashboard,
                           y = y,
                           group_by = group_by,
                           ...)
          })
