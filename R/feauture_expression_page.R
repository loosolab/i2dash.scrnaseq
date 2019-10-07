#' @name feature-expression-page
#' @rdname feature-expression-page
#' @aliases add_feature_expression_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, use_dimred, exprs_values, group_by = NULL, labels = rownames(use_dimred), title = "Feature expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            if(class(exprs_values) == "dgCMatrix") exprs_values <- as.matrix(exprs_values)
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(!is.null(group_by)) assertive.types::assert_is_any_of(group_by, c("factor", "data.frame"))

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
            expanded_component <- list(knitr::knit_expand(file = system.file("templates", "gene_expression_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp))

            dashboard@pages[["feature_expression_page"]] <- list(title = title, layout = "default", menu = menu, components = expanded_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @name feature-expression-page
#' @rdname feature-expression-page
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use_dimred, exprs_values, metadata_columns = NULL, features = NULL, title = "Gene expression", menu = NULL) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            assertive.sets::assert_is_subset(metadata_columns, colnames(SummarizedExperiment::colData(object)))

            expression <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(features)) {
              expression <- SummarizedExperiment::assay(object, i = exprs_values)[features, ]
            }

            SummarizedExperiment::colData(object) %>%
              as.data.frame() %>%
              dplyr::select(!!metadata_columns) -> metadata

            add_feature_expression_page(dashboard,
                                     use_dimred = SingleCellExperiment::reducedDim(object, use_dimred),
                                     exprs_values = expression,
                                     group_by = metadata,
                                     labels = colnames(object),
                                     title = title,
                                     menu = menu)
          })

#' @name feature-expression-page
#' @rdname feature-expression-page
#' @export
setMethod("add_feature_expression_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use_dimred, group_by = NULL, assay = "RNA", assay_slot = "data", subset_row = NULL, ...) {

            assertive.types::assert_is_character(use_dimred)
            assertive.types::assert_is_character(assay)
            assertive.types::assert_is_character(assay_slot)
            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))
            assertive.sets::assert_is_subset(group_by, names(object@meta.data))

            # exprs_values
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            # group_by
            object@meta.data %>%
              as.data.frame() %>%
              dplyr::select(!!group_by) -> group_by

            # use_dimred
            use_dimred <- Seurat::Embeddings(object, reduction = use_dimred)[, 1:2]

            add_feature_expression_page(
              dashboard = dashboard,
              use_dimred = use_dimred,
              exprs_values = exprs_values,
              group_by = group_by,
              ...)
          })
