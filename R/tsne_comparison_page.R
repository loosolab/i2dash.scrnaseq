#' @name tsne-comparison-page
#' @rdname tsne-comparison-page
#' @aliases tsne-comparison-page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_tsne_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, exprs_values, seed = NULL, page = "tsne_comparison_page", title = "Explore t-SNE parameter", menu = "Tools", ...){
            page %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> name

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            if(class(exprs_values) == "dgCMatrix") exprs_values <- as.matrix(exprs_values)
            assertive.types::assert_is_any_of(exprs_values, c("data.frame", "matrix"))

            scater.calculateTSNE <- list(x = exprs_values, ...)

            # Create component environment
            env <- new.env()
            env$scater.calculateTSNE <- scater.calculateTSNE
            env$seed <- seed
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            tsne_comparison_component <- knitr::knit_expand(file = system.file("templates", "tsne_comparison_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            dashboard@pages[[name]] <- list(title = title, layout = "empty", menu = menu, components = tsne_comparison_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @name tsne-comparison-page
#' @rdname tsne-comparison-page
#' @export
setMethod("add_tsne_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, exprs_values = "logcounts", ...){
            assertive.types::assert_is_character(exprs_values)
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            # extract exprs_values from sce object
            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)

            add_tsne_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })

#' @name tsne-comparison-page
#' @rdname tsne-comparison-page
#' @export
setMethod("add_tsne_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "SummarizedExperiment"),
          function(dashboard, object, exprs_values = "logcounts", ...){
            assertive.types::assert_is_character(exprs_values)
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            # extract exprs_values from sce object
            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)

            add_tsne_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })

#' @name tsne-comparison-page
#' @rdname tsne-comparison-page
#' @export
setMethod("add_tsne_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, assay = "RNA", assay_slot = "data", ...){
            assertive.types::assert_is_character(assay)
            assertive.types::assert_is_character(assay_slot)
            assertive.sets::assert_is_subset(assay, names(object@assays))

            # extract exprs_values from Seurat object
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)

            add_tsne_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })
