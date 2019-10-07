#' @name dimred-sample-page
#' @rdname dimred-sample-page
#' @aliases add_dimred_sample_page
#' @return An object of class \linkS4class{i2dash::i2dashboard}.
#' @export
setMethod("add_dimred_sample_page",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, use_dimred, sample_metadata, group_by, title = "Dim. reduction & sample metadata", labels = NULL, show_group_sizes = TRUE, show_silhouette = FALSE, menu = NULL) {

            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_any_of(use_dimred, c("data.frame", "matrix"))
            assertive.types::assert_is_any_of(sample_metadata, c("data.frame", "matrix"))
            assertive.types::assert_is_character(group_by)
            assertive.sets::assert_is_subset(group_by, colnames(sample_metadata))
            assertive.types::assert_is_factor(sample_metadata[[group_by]])

            # Create component environment
            env <- new.env()
            env$reduced_dim <- use_dimred[, 1:2]
            env$metadata <- sample_metadata
            env$grouping <- group_by
            env$labels <- labels
            env$multiple_meta <- ncol(sample_metadata) > 1

            # save environment object
            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            expanded_components <- list()
            timestamp <- Sys.time()

            # Add dimension reduction component
            dim_reduction <- knitr::knit_expand(file = system.file("templates", "dimred_sample_page", "scatterplot_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
            expanded_components <- append(expanded_components, dim_reduction)
            free_comps <- 3

            if(show_group_sizes){
              barplot_grouping_component <- knitr::knit_expand(file = system.file("templates", "dimred_sample_page", "barplot_group_sizes.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
              expanded_components <- append(expanded_components, barplot_grouping_component)
              free_comps <- free_comps - 1
            }

            if(show_silhouette){
              silhouette_plot_component <- knitr::knit_expand(file = system.file("templates", "dimred_sample_page", "silhouette_plot.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
              expanded_components <- append(expanded_components, silhouette_plot_component)
              free_comps <- free_comps - 1
            }

            grouping_index <- which(colnames(sample_metadata) == group_by)
            remaining_metadata <- colnames(sample_metadata)[-grouping_index][1:free_comps]
            for(i in remaining_metadata){
              meta_component <- knitr::knit_expand(file = system.file("templates", "dimred_sample_page", "metadata_plot.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp, meta_column = i)
              expanded_components <- append(expanded_components, meta_component)
            }

            # Expand component
            timestamp <- Sys.time()

            dashboard@pages[["dimred_sample_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 4, sidebar = NULL)
            return(dashboard)
          })

#' @name dimred-sample-page
#' @rdname dimred-sample-page
#' @export
setMethod("add_dimred_sample_page",
          signature = signature(dashboard = "i2dashboard", object = "SingleCellExperiment"),
          function(dashboard, object, use_dimred, sample_metadata, ...){

            assertive.sets::assert_is_subset(use_dimred, SingleCellExperiment::reducedDimNames(object))
            assertive.sets::assert_is_subset(sample_metadata, colnames(SummarizedExperiment::colData(object)))

            use_dimred <- SingleCellExperiment::reducedDim(object, use_dimred)[, 1:2]

            SummarizedExperiment::colData(object) %>%
              as.data.frame() %>%
              dplyr::select(!!sample_metadata) -> sample_metadata

            dashboard <- add_dimred_sample_page(
              dashboard = dashboard,
              use_dimred = use_dimred,
              sample_metadata = sample_metadata,
              ...
            )
          })

#' @name dimred-sample-page
#' @rdname dimred-sample-page
#' @export
setMethod("add_dimred_sample_page",
          signature = signature(dashboard = "i2dashboard", object = "Seurat"),
          function(dashboard, object, use_dimred, sample_metadata, ...){

            assertive.sets::assert_is_subset(use_dimred, names(object@reductions))
            assertive.sets::assert_is_subset(sample_metadata, colnames(object@meta.data))

            use_dimred <- Seurat::Embeddings(object, reduction = use_dimred)[, 1:2]

            object@meta.data %>%
              as.data.frame() %>%
              dplyr::select(!!sample_metadata) -> sample_metadata

            dashboard <- add_dimred_sample_page(
              dashboard = dashboard,
              use_dimred = use_dimred,
              sample_metadata = sample_metadata,
              ...
            )
          })
