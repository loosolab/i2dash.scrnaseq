#' @name feature-grid-page
#' @rdname feature-grid-page
#' @aliases add_feature_grid_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_grid_page",
          signature = signature(report = "i2dashboard", object = "missing"),
          function(report, use_dimred, exprs_values, title = "Feature grid", menu = "Tools") {

            # warn if no interactive mode is used
            if(!report@interactive) warning("This page can only be used during interactive shiny sessions. Consider setting interactivity(report) <- TRUE.")

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Ensure that use_dimred is a list and has names
            if(class(use_dimred) != "list") use_dimred <- list(use_dimred)

            if(!assertive.properties::has_names(use_dimred)) {
              names(use_dimred) <- paste0("dimred_", 1:length(use_dimred))
            }

            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))

            # Create component environment
            env <- new.env()
            env$use_dimred <- use_dimred
            env$exprs_values <- exprs_values

            saveRDS(env, file = file.path(report@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            multi_gene_expr_component <- knitr::knit_expand(file = system.file("templates", "feature_grid.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            report@pages[["gene_grid_page"]] <- list(title = title, layout = "empty", menu = menu, components = multi_gene_expr_component, max_components = 1, sidebar = NULL)
            return(report)
          })

#' @name feature-grid-page
#' @rdname feature-grid-page
#' @export
setMethod("add_feature_grid_page",
          signature = signature(report = "i2dashboard", object = "SingleCellExperiment"),
          function(report, object, use_dimred, exprs_values, subset_row = NULL, ...) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            if(!is.null(subset_row)) {
              expression <- SummarizedExperiment::assay(object, i = exprs_values)[subset_row, ]
            } else {
              expression <- SummarizedExperiment::assay(object, i = exprs_values)
            }

            use_dimreds <- lapply(use_dimred, function(dimred) {
              SingleCellExperiment::reducedDim(object, dimred)[, 1:2]
            })

            report <- add_gene_grid_page(report = report,
                                         use_dimred = use_dimreds,
                                         exprs_values = expression,
                                         ...)
            return(report)
          })

#' @name feature-grid-page
#' @rdname feature-grid-page
#' @export
setMethod("add_feature_grid_page",
          signature = signature(report = "i2dashboard", object = "Seurat"),
          function(report, object, use_dimred, assay, slot = "data", subset_row = NULL) {

            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))

            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            expression <- Seurat::GetAssayData(object = assay_obj, slot = slot)

            if(!is.null(subset_row)) {
              expression <- Seurat::GetAssayData(object = assay_obj, slot = slot)[subset_row, ]
            }

            use_dimreds <- lapply(use_dimred, function(dimred) {
              Seurat::Embeddings(object, reduction = dimred)[, 1:2]
            })

            report <- add_gene_grid_page(report = report,
                                         use_dimred = use_dimreds,
                                         expression = expression,
                                         ...)
            return(report)
          })