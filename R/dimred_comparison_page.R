#' @rdname dimred-comparison-page
#' @return An object of class \linkS4class{i2dashboard}.
#' @export
setMethod("add_dimred_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   exprs_values,
                   seed = NULL,
                   calculateUMAP = list(), # list with further parameter for scater::calculateUMAP() function
                   calculateTSNE = list(), # list with further parameter for scater::calculateTSNE() function
                   page = "dimred_comparison_page",
                   title = "Explore UMAP/t-SNE parameter",
                   menu = "Tools") {
            . <- NULL # see https://github.com/tidyverse/magrittr/issues/29

            page %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> name

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            exprs_values <- as.matrix(exprs_values)

            assertive.types::assert_is_list(calculateUMAP)
            assertive.types::assert_is_list(calculateTSNE)

            # Create component environment
            env <- new.env()
            env$exprs_values <- exprs_values
            env$calculateUMAP <- calculateUMAP
            env$calculateTSNE <- calculateTSNE
            env$seed <- seed
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            umap_comparison_component <- knitr::knit_expand(file = system.file("templates", "dimred_comparison_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

            dashboard@pages[[name]] <- list(title = title, layout = "empty", menu = menu, components = umap_comparison_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @rdname dimred-comparison-page
#' @export
setMethod("add_dimred_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, exprs_values = "logcounts", subset_row = NULL, ...){
            assertive.types::assert_is_character(exprs_values)
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            # extract exprs_values from sce object
            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(subset_row)) exprs_values <- exprs_values[subset_row,]

            add_dimred_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })

#' @rdname dimred-comparison-page
#' @export
setMethod("add_dimred_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "SummarizedExperiment"),
          function(dashboard, object, exprs_values = "logcounts", subset_row = NULL,...){
            assertive.types::assert_is_character(exprs_values)
            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))

            # extract exprs_values from sce object
            exprs_values <- SummarizedExperiment::assay(object, i = exprs_values)
            if(!is.null(subset_row)) exprs_values <- exprs_values[subset_row,]

            add_dimred_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })

#' @rdname dimred-comparison-page
#' @export
setMethod("add_dimred_comparison_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, assay = "RNA", assay_slot = "data", subset_row = NULL, ...){
            assertive.types::assert_is_character(assay)
            assertive.types::assert_is_character(assay_slot)
            assertive.sets::assert_is_subset(assay, names(object@assays))

            # extract exprs_values from Seurat object
            assay_obj <- Seurat::GetAssay(object = object, assay = assay)
            exprs_values <- Seurat::GetAssayData(object = assay_obj, slot = assay_slot)
            if(!is.null(subset_row)) exprs_values <- exprs_values[subset_row,]

            add_dimred_comparison_page(dashboard, exprs_values = exprs_values, ...)
          })
