#' @name feature-grid-page
#' @rdname feature-grid-page
#' @aliases add_feature_grid_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_grid_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   use_dimred,
                   exprs_values,
                   title = "Feature grid",
                   page = "feature_grid_page",
                   menu = "Tools") {

            # warn if no interactive mode is used
            if(!dashboard@interactive) warning("This page can only be used during interactive shiny sessions. Consider setting interactivity(dashboard) <- TRUE.")

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Ensure that use_dimred is a list and has names
            if(class(use_dimred) != "list") use_dimred <- list(use_dimred)

            if(!assertive.properties::has_names(use_dimred)) {
              names(use_dimred) <- paste0("dimred_", 1:length(use_dimred))
            }

            exprs_values <- as.matrix(exprs_values)

            # Create component environment
            env <- new.env()
            env$use_dimred <- use_dimred
            env$exprs_values <- exprs_values

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            multi_feature_expr_component <- knitr::knit_expand(file = system.file("templates", "feature_grid.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            dashboard@pages[["feature_grid_page"]] <- list(title = title, layout = "empty", menu = menu, components = multi_feature_expr_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @name feature-grid-page
#' @rdname feature-grid-page
#' @export
setMethod("add_feature_grid_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   use_dimred,
                   exprs_values,
                   subset_row = NULL,
                   ...) {

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            use_dimreds <- lapply(use_dimred, function(dimred) {
              SingleCellExperiment::reducedDim(object, dimred)[, 1:2]
            })

            dashboard <- add_feature_grid_page(
              dashboard = dashboard,
              use_dimred = use_dimreds,
              exprs_values = exprs_values,
              ...
            )
            return(dashboard)
          })

#' @name feature-grid-page
#' @rdname feature-grid-page
#' @export
setMethod("add_feature_grid_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard,
                   object,
                   use_dimred,
                   assay,
                   slot = "data",
                   subset_row = NULL) {

            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(assay, names(object@assays))

            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!is.null(subset_row)) {
              exprs_values <- exprs_values[subset_row, ]
            }

            use_dimreds <- lapply(use_dimred, function(dimred) {
              Seurat::Embeddings(object, reduction = dimred)[, 1:2]
            })

            dashboard <- add_gene_grid_page(dashboard = dashboard,
                                         use_dimred = use_dimreds,
                                         exprs_values = exprs_values,
                                         ...)
            return(dashboard)
          })
