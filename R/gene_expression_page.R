#' @rdname add_gene_expression_page
#' @param report An object of class \linkS4class{i2dash::i2dashboard}.
#' @param use_dimred A data.frame or matrix containing coordinates of the reduced dimensions.
#' @param exprs_values A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param group_by A factor or data.frame along rows of \code{use_dimred} that is used for grouping expression values in the violin plot.
#' @param labels A vector with optional sample labels that are used instead of \code{rownames(use_dimred)}.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#'
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "missing"),
          function(report, use_dimred, exprs_values, group_by = NULL, labels = rownames(use_dimred), title = "Gene expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
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
            saveRDS(env, file = file.path(report@datadir, paste0(env_id, ".rds")))

            timestamp <- Sys.time()
            expanded_component <- list(knitr::knit_expand(file = system.file("templates", "gene_expression_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp))

            report@pages[["gene_expression_page"]] <- list(title = title, layout = "default", menu = menu, components = expanded_component, max_components = 1, sidebar = NULL)
            return(report)
          })

#' @rdname add_gene_expression_page
#'
#' @param object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object.
#' @param dimred A string or integer scalar indicating the reduced dimension result in \code{reducedDims(object)}.
#' @param exprs_values A string or integer scalar specifying which assay to obtain expression values from.
#' @param metadata_columns A character vector with column names of \code{colData(object)} to use for cell grouping.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @inheritParams add_gene_expression_page,i2dashboard,missing-method
#'
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "SingleCellExperiment"),
          function(report, object, dimred, exprs_values, metadata_columns = NULL, subset_row = NULL, title = "Gene expression", menu = NULL) {

            assertive.sets::assert_is_subset(dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            assertive.sets::assert_is_subset(metadata_columns, colnames(SummarizedExperiment::colData(object)))

            expression <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(features)) {
              expression <- SummarizedExperiment::assay(object, i = exprs_values)[features, ]
            }

            SummarizedExperiment::colData(object) %>%
              as.data.frame() %>%
              dplyr::select(!!metadata_columns) -> metadata

            add_gene_expression_page(report,
                                     use_dimred = SingleCellExperiment::reducedDim(object, dimred),
                                     exprs_values = expression,
                                     group_by = metadata,
                                     labels = colnames(object),
                                     title = title,
                                     menu = menu)
          })

#' @rdname add_gene_expression_page
#'
#' @param object A valid \linkS4class{Seurat::Seurat} object.
#' @param dimred A character vector indicating the \linkS4class{Seurat::DimReduc} object in \code{object@reductions}.
#' @param assay A character vector specifying which assay from \code{object@assays} to obtain expression values from.
#' @param metadata_columns A character vector with column names of \code{object@meta.data} to use for cell grouping.
#' @param slot A character vector specifying the name of the slot in the assay.
#' @param subset_row A character vector (of feature names), a logical vector or numeric vector (of indices) specifying the features to use. The default of NULL will use all features.
#' @inheritParams add_gene_expression_page,i2dashboard,missing-method
#'
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_gene_expression_page",
          signature = signature(report = "i2dashboard", object = "Seurat"),
          function(report, object, dimred, assay, metadata_columns, assay_slot = "data", subset_row = NULL, title = NULL, menu = NULL) {

            assertive.sets::assert_is_subset(dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))
            assertive.sets::assert_is_subset(metadata_columns, names(object@meta.data))

            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            expression <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!is.null(subset_row)) {
              expression <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)[feature, ]
            }

            object@meta.data[metadata] %>%
              as.data.frame() %>%
              dplyr::select(!!metadata_columns) -> metadata

            add_gene_expression_page(report,
                                     use_dimred = Seurat::Embeddings(object, reduction = dimred),
                                     exprs_values = expression,
                                     group_by = metadata,
                                     labels = colnames(expression),
                                     title = title,
                                     menu = menu)
          })
