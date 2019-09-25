setGeneric("add_dim_reduction_page", function(object, sc_object, ...) standardGeneric("add_dim_reduction_page"))


#' Renders a page with four linked components at maximum. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. If \code{barplot_grouping} is \code{TRUE} the second component will be a bar plot, that shows the number of observations by groups defined in \code{grouping}. If \code{silhouette_plot} is \code{TRUE} the third component will be a silhoutte plot, that shows how well each observation has been classified. Therefore the Euclidean distance is applied to the coordinates from \code{reduced_dim} and grouped by the groups defined in \code{grouping}. The remaining component slots are filled with bar or boxplots, that visualize one of the columns from \code{metadata} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A data.frame or matrix containing coordinates of the reduced dimensions. Rownames are used as labels.
#' @param metadata A data.frame or matrix containing a maximum of four columns with cell metadata (e.g. cluster, timepoint, number of genes, etc). Should have the same number of rows/length as \code{reduced_dim}. Note: Factorial metadata needs to be of class factor in the data.frame.
#' @param grouping A character string identical to one of the column names in \code{metadata}.
#' @param title The title of the page.
#' @param labels (Optional) A vector with labels. Should be of the same length as the rownumber of \code{reduced_dim}.
#' @param barplot_grouping (Optional) A logical value. (Default: TRUE) If TRUE, a barplot with the number of observations from 'grouping' will be created.
#' @param silhouette_plot (Optional) A logical value. (Default: FALSE) If TRUE, a silhouette plot will be created.
#' @param menu (Optional) The name of the menu, under which the page should appear in the navigation.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dim_reduction_page",
          signature = signature(object = "i2dashboard", sc_object = "missing"),
          function(object, reduced_dim, metadata, grouping, title = NULL, labels = NULL, barplot_grouping = TRUE, silhouette_plot = FALSE, menu = NULL, sidebar = NULL) {

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Validate input
  if(!is.data.frame(reduced_dim) & !is.matrix(reduced_dim)) stop("'reduced_dim' should should be of class 'data.frame' or 'matrix'.")
  if(ncol(reduced_dim) < 2 ) stop("'reduced_dim' should contain at least two columns.")
  if(!is.data.frame(metadata) & !is.matrix(metadata)) stop("'metadata' should be of class 'data.frame' or 'matrix'.")
  if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")
  if(ncol(metadata) > 4 ) stop("'metadata' should contain 4 columns at maximum.")
  if(nrow(metadata) != nrow(reduced_dim)) stop("'metadata' and 'reduced_dim' should contain the same number of rows.")
  if(!grouping %in% colnames(metadata)) stop("'metadata' does not contain a column with this name.")
  if(is.factor(metadata$grouping)) stop("'grouping' in 'metadata' should contain factors and be of class 'factor'.")
  if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")
  if(!is.null(labels) & length(labels) != nrow(reduced_dim)) stop("The length of the vector 'labels' should be equal to the number of rows in 'reduced_dim'.")

  # Create component environment
  env <- new.env()
  env$reduced_dim <- reduced_dim[, 1:2]
  env$metadata <- metadata
  env$grouping <- grouping
  env$labels <- labels
  env$multiple_meta <- ncol(metadata) > 1

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  expanded_components <- list()
  timestamp <- Sys.time()

  # fill list "expanded_components" with components
  max_comp <- 0
  dim_reduction <- knitr::knit_expand(file = system.file("templates", "dimension_reduction_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, dim_reduction)
  max_comp <- max_comp + 1
  if(barplot_grouping){
    barplot_grouping_component <- knitr::knit_expand(file = system.file("templates", "dimension_reduction_barplot_grouping.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
    expanded_components <- append(expanded_components, barplot_grouping_component)
    max_comp <- max_comp + 1
  }

  if(silhouette_plot){
    silhouette_plot_component <- knitr::knit_expand(file = system.file("templates", "dimension_reduction_silhouette.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
    expanded_components <- append(expanded_components, silhouette_plot_component)
    max_comp <- max_comp + 1
  }

  grouping_index <- which(colnames(metadata) == grouping)
  meta_names <- colnames(metadata)[-grouping_index]
  for(i in meta_names){
    if(max_comp < 4){
      meta_component <- knitr::knit_expand(file = system.file("templates", "dimension_reduction_barplot_boxplot.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp, meta_column = i)
      expanded_components <- append(expanded_components, meta_component)
      max_comp <- max_comp + 1
    }
  }

  # Expand component
  timestamp <- Sys.time()

  object@pages[["dim_reduction_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 4, sidebar = sidebar)
  return(object)
})

#' Renders a page with four linked components at maximum. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. If \code{barplot_grouping} is \code{TRUE} the second component will be a bar plot, that shows the number of observations by groups defined in \code{grouping}. If \code{silhouette_plot} is \code{TRUE} the third component will be a silhoutte plot, that shows how well each observation has been classified. Therefore the Euclidean distance is applied to the coordinates from \code{reduced_dim} and grouped by the groups defined in \code{grouping}. The remaining component slots are filled with bar or boxplots, that visualize one of the columns from \code{metadata} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{SingleCellExperiment::SingleCellExperiment} object
#' @param reduced_dim A character of length 1 representing the name of a dimension reduction of \code{reducedDim}.
#' @param metadata A character or list representing the names of  columns of \code{colData}.
#' @param grouping A character of length 1 representing the name of columns of \code{colData} and used for expression grouping.
#' @param title The title of the page.
#' @param barplot_grouping (Optional) A logical value. (Default: TRUE) If TRUE, a barplot with the number of observations from 'grouping' will be created.
#' @param silhouette_plot (Optional) A logical value. (Default: FALSE) If TRUE, a silhouette plot will be created.
#' @param menu (Optional) The name of the menu, under which the page should appear in the navigation.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dim_reduction_page",
          signature = signature(object = "i2dashboard", sc_object = "SingleCellExperiment"),
          function(object, sc_object, reduced_dim, metadata, grouping, title = NULL, barplot_grouping = TRUE, silhouette_plot = FALSE, menu = NULL, sidebar = NULL) {
            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(colData(sc_object)))) stop("'colData' slot of the SingleCellExperiment object does not contain the column names from 'metadata'.")
            metadata <- as.data.frame(sc_object@colData[metadata])
            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) | length(reduced_dim) > 1) stop("'reduced_dim' should be a character of length 1.")
            if(!reduced_dim %in% reducedDimNames(sc_object)) stop("'reducedDim' slot of the SingleCellExperiment object does not contain the name provided in 'reduced_dim'.")
            reduced_dim <- reducedDim(sc_object, reduced_dim)

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(sc_object)

            object <- add_dim_reduction_page(object = object,
                                             reduced_dim = reduced_dim,
                                             metadata = metadata,
                                             grouping = grouping,
                                             title = title,
                                             labels = labels,
                                             menu = menu,
                                             sidebar = sidebar,
                                             barplot_grouping = barplot_grouping,
                                             silhouette_plot = silhouette_plot)
            return(object)

})

#' Renders a page with four linked components at maximum. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. If \code{barplot_grouping} is \code{TRUE} the second component will be a bar plot, that shows the number of observations by groups defined in \code{grouping}. If \code{silhouette_plot} is \code{TRUE} the third component will be a silhoutte plot, that shows how well each observation has been classified. Therefore the Euclidean distance is applied to the coordinates from \code{reduced_dim} and grouped by the groups defined in \code{grouping}. The remaining component slots are filled with bar or boxplots, that visualize one of the columns from \code{metadata} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param sc_object A valid \linkS4class{Seurat::Seurat} object
#' @param reduced_dim A character of length 1 representing the name of a \linkS4class{Seurat::DimReduc} object present in the slot \code{reductions} of the \linkS4class{Seurat::Seurat} object.
#' @param metadata A character or list representing the names of columns in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} object.
#' @param grouping A character of length 1 representing the name of a column in the slot \code{meta.data} in the \linkS4class{Seurat::Seurat} and used for expression grouping.
#' @param title The title of the page.
#' @param barplot_grouping (Optional) A logical value. (Default: TRUE) If TRUE, a barplot with the number of observations from 'grouping' will be created.
#' @param silhouette_plot (Optional) A logical value. (Default: FALSE) If TRUE, a silhouette plot will be created.
#' @param menu (Optional) The name of the menu, under which the page should appear in the navigation.
#' @param sidebar (Optional) The page layout (see below)
#'
#' @return A string containing markdown code for the rendered page.
#' @export
setMethod("add_dim_reduction_page",
          signature = signature(object = "i2dashboard", sc_object = "Seurat"),
          function(object, sc_object, reduced_dim, metadata, grouping, title = NULL, barplot_grouping = TRUE, silhouette_plot = FALSE, menu = NULL, sidebar = NULL) {

            # validate and extract metadata
            if(!is.character(metadata) & !is.list(metadata)) stop("'metadata' should be a character or a list.")
            if(!all(metadata %in% names(sc_object@meta.data))) stop("'meta.data' slot of the Seurat object does not contain the column names from 'metadata'.")
            metadata <- sc_object@meta.data[metadata]

            # validate input and extract dimension reduction
            if(!is.character(reduced_dim) | length(reduced_dim) > 1) stop("'reduced_dim' should be a character of length 1.")
            if(!reduced_dim %in% names(sc_object@reductions)) stop("'reductions' slot of the Seurat object does not contain the name provided in 'reduced_dim'.")
            reduced_dim <- Seurat::Embeddings(sc_object, reduction = reduced_dim)[,1:2]

            # validate grouping
            if(!is.character(grouping) | length(grouping) > 1) stop("'grouping' should be a character of length 1.")

            labels <- colnames(sc_object)

            object <- add_dim_reduction_page(object = object,
                                             reduced_dim = reduced_dim,
                                             metadata = metadata,
                                             grouping = grouping,
                                             title = title,
                                             labels = labels,
                                             menu = menu,
                                             sidebar = sidebar,
                                             barplot_grouping = barplot_grouping,
                                             silhouette_plot = silhouette_plot)
            return(object)
})



