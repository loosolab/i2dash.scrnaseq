#' @name dimred-metadata-page
#' @rdname dimred-metadata-page
#' @aliases add_dimred_feature_page
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, use_dimred, exprs_values, feature_metadata, title = "Marker gene expression", menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            exprs_values <- as.matrix(exprs_values)
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(feature_metadata, c("data.frame", "matrix"))

            if(ncol(use_dimred) < 2 ) stop("'use_dimred' should contain at least two columns.")

            if(is.null(colnames(feature_metadata))) {
              colnames(feature_metadata) <- paste0("V", 1:ncol(feature_metadata))
            }

            if(is.null(rownames(exprs_values))) rownames(exprs_values) <- 1:nrow(exprs_values)

            # Create component environment
            env <- new.env()

            env$reduced_dim <- data.frame("x" = use_dimred[,1], "y" = use_dimred[,2])
            env$expression <- round(exprs_values, 3)
            env$metadata <- feature_metadata

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Render component
            timestamp <- Sys.time()

            component <- knitr::knit_expand(file = system.file("templates", "dimred_feature_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            dashboard@pages[["dimred_metadata_page"]] <- list(title = title, layout = "default", menu = menu, components = component, max_components = 1)
            return(dashboard)
          })


#' @name dimred-metadata-page
#' @rdname dimred-metadata-page
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use_dimred, exprs_values, feature_metadata, subset_row, title = "Marker gene expression", menu = NULL) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment:assay(object))
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
                                               metadata = metadata,
                                               title = title,
                                               menu = menu)
            return(dashboard)
          })

#' @name dimred-metadata-page
#' @rdname dimred-metadata-page
#' @export
setMethod("add_dimred_feature_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use_dimred, exprs_values, feature_metadata, subset_row, assay, assay_slot = "data", title = "Marker gene expression", menu = NULL) {

            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))
            assertive.sets::assert_is_subset(feature_metadata, names(object@meta.data))

            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            expression <- Seurat::GetAssayData(object = assay_obj, slot = slot)
            metadata <- object@meta.data[metadata]

            if(!is.null(subset_row)) {
              expression <- Seurat::GetAssayData(object = assay_obj, slot = slot)[subset_row, ]
              metadata <- metadata[subset_row, ]
            }

            use_dimred <- lapply(use_dimred, function(dimred) {
              Seurat::Embeddings(object, reduction = dimred)[, 1:2]
            })

            dashboard <- add_dimred_feature_page(dashboard = dashboard,
                                               use_dimred = use_dimred,
                                               exprs_values = expression,
                                               metadata = metadata,
                                               title = title,
                                               menu = menu)
            return(dashboard)
          })
