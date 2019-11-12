#' @name dimred-feature-page
#' @rdname dimred-feature-page
#' @aliases add_dimred_feature_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, use_dimred, exprs_values, feature_metadata, page = "dimred_feature_page", title = "Dim. reduction & deature metadata", menu = NULL) {

            page %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> name

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            exprs_values <- as.matrix(exprs_values)
            assertive.types::assert_is_any_of(feature_metadata, c("data.frame", "matrix"))

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(is.null(colnames(feature_metadata))) {
              colnames(feature_metadata) <- paste0("V", 1:ncol(feature_metadata))
            }

            # Create component environment
            env <- new.env()

            env$reduced_dim <- data.frame("x" = use_dimred[,1], "y" = use_dimred[,2])
            env$expression <- round(exprs_values, 3)
            env$metadata <- feature_metadata

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Render component
            timestamp <- Sys.time()

            component <- knitr::knit_expand(file = system.file("templates", "dimred_metadata.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            dashboard@pages[[name]] <- list(title = title, layout = "default", menu = menu, components = component, max_components = 1)
            return(dashboard)
          })


#' @name dimred-feature-page
#' @rdname dimred-feature-page
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use_dimred, exprs_values, feature_metadata, subset_row, ...) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            assertive.sets::assert_is_subset(feature_metadata, colnames(SummarizedExperiment::rowData(object)))

            use_dimred <- SingleCellExperiment::reducedDim(object, use_dimred)
            exprs_values <- SummarizedExperiment::assay(object, exprs_values)

            SummarizedExperiment::rowData(object) %>%
              as.data.frame() %>%
              dplyr::select(!!feature_metadata) -> metadata

            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
              metadata <- metadata[subset_row, ]
            }

            dashboard <- add_dimred_feature_page(dashboard = dashboard,
                                               use_dimred = use_dimred,
                                               exprs_values = exprs_values,
                                               feature_metadata = metadata,
                                               ...)
          })

#' @name dimred-feature-page
#' @rdname dimred-feature-page
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use_dimred, feature_metadata, subset_row, assay = "RNA", assay_slot = "data", ...){
            assertive.types::assert_is_character(use_dimred)
            assertive.types::assert_is_character(assay)
            assertive.types::assert_is_character(assay_slot)
            assertive.sets::assert_is_subset(use_dimred, colnames(object@reductions))
            assertive.sets::assert_is_subset(assay, colnames(object@assays))
            assertive.sets::assert_is_subset(feature_metadata, colnames(object[[assay]]@meta.features))

            # exprs_values
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)[subset_row, ]

            # feature_metadata
            object[[assay]]@meta.features %>%
              as.data.frame() %>%
              dplyr::select(!!feature_metadata) -> feature_metadata
            feature_metadata <- feature_metadata[subset_row, ]

            # use_dimred
            use_dimred <- Seurat::Embeddings(object, reduction = use_dimred)[, 1:2]

            dashboard <- add_dimred_feature_page(
              dashboard = dashboard,
              use_dimred = use_dimred,
              exprs_values = exprs_values,
              feature_metadata = feature_metadata,
              ...)
          })
