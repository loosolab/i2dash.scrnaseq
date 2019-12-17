#' @name feature-selection-page
#' @rdname feature-selection-page
#' @aliases add_feature_selection_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_feature_selection_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard,
                   exprs_values,
                   use_function = c("modelGeneVar", "modelGeneVarByPoisson", "modelGeneCV2", "modelGeneCV2WithSpikes", "modelGeneVarWithSpikes"),
                   title = "Feature selection",
                   menu = "Tools",
                   ... # further parapeter for use_function
                   ) {

            # warn if no interactive mode is used
            if(!dashboard@interactive) warning("This page can only be used during interactive shiny sessions. Consider setting interactivity(dashboard) <- TRUE.")


            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Input validation
            exprs_values <- as.matrix(exprs_values)

            use_function <- match.arg(use_function)
            default <- switch(use_function,
                              "modelGeneVar" = "bio",
                              "modelGeneVarByPoisson" = "bio",
                              "modelGeneCV2" = "ratio",
                              "modelGeneCV2WithSpikes" = "ratio",
                              "modelGeneVarWithSpikes" = "bio")

            data <- do.call(use_function, list(x = exprs_values, ...))

            # Create component environment
            env <- new.env()
            env$data <- as.data.frame(data)
            env$default <- default
            env$fit <- S4Vectors::metadata(data)
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            timestamp <- Sys.time()
            expanded_component <- list(knitr::knit_expand(file = system.file("templates", "feature_selection_page.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp))

            dashboard@pages[["feature_selection_page"]] <- list(title = title, layout = "empty", menu = menu, components = expanded_component, max_components = 1, sidebar = NULL)
            return(dashboard)
          })

#' @name feature-selection-page
#' @rdname feature-selection-page
#' @export
setMethod("add_feature_selection_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard,
                   object,
                   exprs_values = "logcounts", # assay
                   ...) {

            assertive.sets::assert_is_subset(exprs_values, SummarizedExperiment::assayNames(object))
            exprs_values <- SummarizedExperiment::assay(object, exprs_values)

            dashboard <- add_feature_selection_page(
              dashboard = dashboard,
              exprs_values = exprs_values,
              ...
            )
          })

#' @rdname feature-selection-page
#' @export
setMethod("add_feature_selection_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard,
                   object,
                   assay = "RNA",
                   slot = "logcounts",
                   ...) {

            assertive.sets::assert_is_subset(assay, Seurat::Assays(object))
            exprs_values <- Seurat::GetAssayData(seu, assay = assay, slot = slot)

            dashboard <- add_feature_selection_page(
              dashboard = dashboard,
              exprs_values = exprs_values,
              ...
            )
          })
