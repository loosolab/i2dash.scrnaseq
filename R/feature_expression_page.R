#' @rdname feature-expression-page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, use_dimred, exprs_values, group_by = NULL, labels = rownames(use_dimred), title = "Feature expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            exprs_values <- as.matrix(exprs_values)

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(!is.null(group_by)) assertive.types::assert_is_any_of(group_by, c("factor", "data.frame"))
            if(is.null(labels)) labels <- 1:ncol(use_dimred)
            if(is.null(rownames(exprs_values))) rownames(exprs_values) <- 1:nrow(exprs_values)

            # Prepare expression data
            exprs_values %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "feature") %>%
              dplyr::mutate(feature = paste0("feature_", feature)) %>%
              tibble::column_to_rownames(var = "feature") %>%
              t() -> e

            # Prepare grouping
            if(is.null(group_by)) group_by <- factor(rep(1, nrow(use_dimred)))
            if(is.factor(group_by)) {
              group_by <- data.frame(default = group_by)
            } else {
              group_by %<>% dplyr::select_if(is.factor)
            }

            # Create plot data
            data.frame(x = use_dimred[, 1], y = use_dimred[, 2], label = labels) %>%
              cbind(e, group_by) %>%
              tidyr::gather(key = "feature", value = "expression", dplyr::starts_with("feature_")) %>%
              tidyr::gather(key = "group_by", value = "level", -x, -y, -label, -feature, -expression) -> data

            # Create component environment
            env <- new.env()
            env$data <- data
            env$group_filter <- colnames(group_by)[1]
            env$exprs_values <- exprs_values
            env$use_dimred <- use_dimred
            env$group_by <- group_by
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            timestamp <- Sys.time()
            expanded_component <- list(knitr::knit_expand(file = system.file("templates", "feature_expression_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp))

            dashboard@pages[["feature_expression_page"]] <- list(title = title, layout = "default", menu = menu, components = expanded_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @rdname feature-expression-page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use_dimred, exprs_values, group_by = NULL, subset_row = NULL, title = "Feature expression", menu = NULL) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            assertive.sets::assert_is_subset(group_by, colnames(SummarizedExperiment::colData(object)))

            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            SummarizedExperiment::colData(object) %>%
              as.data.frame() %>%
              dplyr::select(!!group_by) -> metadata

            add_feature_expression_page(dashboard,
                                     use_dimred = SingleCellExperiment::reducedDim(object, use_dimred),
                                     exprs_values = exprs_values,
                                     group_by = metadata,
                                     ...)
          })

#' @rdname feature-expression-page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use_dimred, assay, group_by, slot = "data", subset_row = NULL, title = "Feature expression", menu = NULL) {

            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))
            assertive.sets::assert_is_subset(group_by, names(object@meta.data))

            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = slot)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            object@meta.data[metadata] %>%
              as.data.frame() %>%
              dplyr::select(!!group_by) -> metadata

            add_feature_expression_page(dashboard,
                                     use_dimred = Seurat::Embeddings(object, reduction = dimred),
                                     exprs_values = exprs_values,
                                     group_by = metadata,
                                     ...)
          })
