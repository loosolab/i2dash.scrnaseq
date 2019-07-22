#' Renders a dimension reduction page
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A data.frame or matrix containing two columns for the x and y coordinates.
#' @param metadata A data.frame or matrix containing a maximum of four columns with cell metadata (e.g. cluster, timepoint, number of genes, etc). Should have the same rownumber as 'reduced_dim'. Note: Factorial metadata needs to be of class factor in the data.frame.
#' @param grouping A character string identical to one of the column names in 'metadata'.
#' @param title The title of the page.
#' @param labels (Optional) A vector with labels. Should be of the same length as the rownumber of 'reduced_dim'.
#' @param barplot_grouping (Default: TRUE) A logical value. (Default: TRUE) If TRUE, a barplot with the number of observations from 'grouping' will be created.
#' @param silhouette_plot (Optional) A logical value. (Default: FALSE) If TRUE, a silhouette plot will be created.
#' @param menu (Optional) The name of the menu, under which the page should appear in the navigation.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
add_dim_reduction_page <- function(object, reduced_dim, metadata, grouping, title = NULL, labels = NULL, barplot_grouping = TRUE, silhouette_plot = FALSE, menu = NULL, sidebar = NULL) {

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Validate input
  if(!is.data.frame(reduced_dim) & !is.matrix(reduced_dim)) stop("'reduced_dim' should should be of class 'data.frame' or 'matrix'.")
  if(!dim(reduced_dim)[[2]] == 2 ) stop("'reduced_dim' should contain two columns.")
  if(!is.data.frame(metadata) & !is.matrix(metadata)) stop("'metadata' should be of class 'data.frame' or 'matrix'.")
  if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")
  if(!dim(metadata)[[2]] < 5 ) stop("'metadata' should contain 4 columns at maximum.")
  if(dim(metadata)[[1]] != dim(reduced_dim)[[1]]) stop("'metadata' and 'reduced_dim' should contain the same number of rows.")
  if(!grouping %in% colnames(metadata)) stop("'metadata' does not contain a column with this name.")
  if(is.factor(metadata$grouping)) stop("'grouping' in 'metadata' should contain factors and be of class 'factor'.")
  if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")
  if(!is.null(labels) & length(labels) != dim(reduced_dim)[[1]]) stop("The length of the vector 'labels' should be equal to the number of rows in 'reduced_dim'.")

  comp_nr <- dim(metadata)[[2]]

  # Create component environment
  env <- new.env()

  env$reduced_dim <- reduced_dim
  env$metadata <- metadata
  env$grouping <- grouping
  env$labels <- labels
  env$multiple_meta <- if(comp_nr > 1) TRUE else FALSE

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  expanded_components <- list()
  timestamp <- Sys.time()

  # fill list "expanded_components" with components
  max_comp <- 0
  dim_reduction <- knitr::knit_expand(file = system.file("templates", "red_dim_1.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, dim_reduction)
  max_comp <- max_comp + 1
  if(barplot_grouping){
    barplot_grouping_component <- knitr::knit_expand(file = system.file("templates", "red_dim_2.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
    expanded_components <- append(expanded_components, barplot_grouping_component)
    max_comp <- max_comp + 1
  }

  if(silhouette_plot){
    silhouette_plot_component <- knitr::knit_expand(file = system.file("templates", "red_dim_4.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
    expanded_components <- append(expanded_components, silhouette_plot_component)
    max_comp <- max_comp + 1
  }

  grouping_index <- which(colnames(metadata) == grouping)
  meta_names <- colnames(metadata)[-grouping_index]
  for(i in meta_names){
    if(max_comp < 4){
      meta_component <- knitr::knit_expand(file = system.file("templates", "red_dim_3.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp, meta_column = i)
      expanded_components <- append(expanded_components, meta_component)
      max_comp <- max_comp + 1
    }
  }

  # Expand component
  timestamp <- Sys.time()

  object@pages[["dim_reduction_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 4, sidebar = sidebar)
  return(object)
  #return(list("title" = title, "layout" = "2x2_grid", menu = NULL, components = expanded_components))
}
