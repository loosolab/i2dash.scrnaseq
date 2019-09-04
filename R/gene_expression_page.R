#' Renders a page with two linked components. The first component is a scatterplot, showing samples in along coordinates from \code{reduced_dim}. The second component is a violin plot, that shows expression values from \code{expression} by groups defined in \code{grouping}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A data.frame or matrix containing coordinates of the reduced dimensions. Rownames are used as labels.
#' @param expression A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param metadata A data.frame with sample metadata or a factor. Should have the same number of rows/length as \code{reduced_dim}.
#' @param grouping A character string identical to one of the column names in \code{metadata} used for expression grouping.
#' @param title The title of the page.
#' @param labels (Optional) A vector with labels. Should be of the same length as the rownumber of 'reduced_dim'.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#' @param sidebar (Optional) The page layout (see below).
#'
#' @return A string containing markdown code for the rendered page.
#' @export
add_gene_expression_page <- function(object, reduced_dim, expression, metadata, grouping, title = NULL, labels = NULL, menu = NULL, sidebar = NULL) {

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Validate input
  if(!is.data.frame(reduced_dim) & !is.matrix(reduced_dim)) stop("'reduced_dim' should should be of class 'data.frame' or 'matrix'.")
  if(ncol(reduced_dim) < 2 ) stop("'reduced_dim' should contain at least two columns.")
  if(!is.data.frame(expression) & !is.matrix(expression)) stop("'expression' should be of class 'data.frame' or 'matrix'.")
  if(!is.data.frame(metadata) & !is.matrix(metadata) & !is.factor(metadata)) stop("'metadata' should be of class 'data.frame' or 'matrix' or a factor.")
  if(!is.factor(metadata)){
    if(is.null(colnames(metadata))) stop("'metadata' should contain colnames.")
    if(nrow(metadata) != nrow(reduced_dim)) stop("'metadata' and 'reduced_dim' should contain the same number of rows.")
    if(!grouping %in% colnames(metadata)) stop("'metadata' does not contain a column with this name.")
  } else {
    if(length(metadata) != nrow(reduced_dim)) stop("'metadata' should have the same length as the number of rows of 'reduced_dim'.")
  }
  if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")
  if(!is.null(labels) & length(labels) != nrow(reduced_dim)) stop("The length of the vector 'labels' should be equal to the number of rows in 'reduced_dim'.")

  # Create component environment
  env <- new.env()

  env$reduced_dim <- reduced_dim[, 1:2]
  env$expression <- expression
  env$metadata <- metadata
  env$grouping <- grouping
  env$labels <- labels
  if(!is.factor(metadata)) env$multiple_meta <- ncol(metadata) > 1 else env$multiple_meta <- FALSE

  # save environment object
  saveRDS(env, file = file.path(object@datadir, paste0(env_id, ".rds")))

  expanded_components <- list()
  timestamp <- Sys.time()

  # fill list "expanded_components" with components
  scatterplot_component <- knitr::knit_expand(file = system.file("templates", "gene_expression_dimred.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, scatterplot_component)

  violinplot_component<- knitr::knit_expand(file = system.file("templates", "gene_expression_violin_table.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)
  expanded_components <- append(expanded_components, violinplot_component)

  # Expand component
  timestamp <- Sys.time()

  object@pages[["gene_expression_page"]] <- list(title = title, layout = "2x2_grid", menu = menu, components = expanded_components, max_components = 2, sidebar = sidebar)
  return(object)
}
