#' Renders a linked gene expression page.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A data.frame or matrix containing two columns for the x and y coordinates. The rownames are used as labels.
#' @param count_table A data.frame or matrix containing the genes of interest as rownames and cells/ samples as columnnamens.
#' @param metadata A data.frame or matrix with factorial cell metadata or a vector with values (e.g. cluster, timepoint, etc.). Should have the same rownumber as 'reduced_dim'.
#' @param grouping A character string identical to one of the column names in 'metadata'.
#' @param title The title of the page.
#' @param labels (Optional) A vector with labels. Should be of the same length as the rownumber of 'reduced_dim'.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
add_gene_expression_page <- function(object, reduced_dim, count_table, metadata, grouping, title = NULL, labels = NULL, menu = NULL, sidebar = NULL) {

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Validate input
  if(!is.data.frame(reduced_dim) & !is.matrix(reduced_dim)) stop("'reduced_dim' should should be of class 'data.frame' or 'matrix'.")
  if(!dim(reduced_dim)[[2]] == 2 ) stop("'reduced_dim' should contain two columns.")
  if(!is.data.frame(count_table) & !is.matrix(count_table)) stop("'count_table' should be of class 'data.frame' or 'matrix'.")
  if(!is.data.frame(metadata) & !is.matrix(metadata)) stop("'metadata' should be of class 'data.frame' or 'matrix'.")
  if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")
  if(dim(metadata)[[1]] != dim(reduced_dim)[[1]]) stop("'metadata' and 'reduced_dim' should contain the same number of rows.")
  if(!grouping %in% colnames(metadata)) stop("'metadata' does not contain a column with this name.")
  if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")
  if(!is.null(labels) & length(labels) != dim(reduced_dim)[[1]]) stop("The length of the vector 'labels' should be equal to the number of rows in 'reduced_dim'.")

  # Create component environment
  env <- new.env()

  env$reduced_dim <- reduced_dim
  env$count_table <- count_table
  env$metadata <- metadata
  env$grouping <- grouping
  env$labels <- labels
  env$multiple_meta <- if(dim(metadata)[[2]] > 1) TRUE else FALSE

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  expanded_components <- list()
  timestamp <- Sys.time()

  # fill list "expanded_components" with components
  scatterplot_component <- knitr::knit_expand(file = system.file("templates", "gene_expr_1.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, scatterplot_component)

  boxplot_component<- knitr::knit_expand(file = system.file("templates", "gene_expr_2.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, boxplot_component)

  # Expand component
  timestamp <- Sys.time()

  object@pages[["gene_expression_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 2, sidebar = sidebar)
  return(object)
}
