#' Renders a page, where the user first can select the dimension reduction from \code{reduced_dim} (if a list is given). Then several features from \code{expression} can be selected and also the number of columns to render scatter plots along coordinates from \code{reduced_dim} for each selected feature and coloured by its expression values from \code{expression}.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param reduced_dim A named list of data.frames or matrices or a single data.frame or matrix containing coordinates of the reduced dimensions.
#' @param expression A data.frame or matrix containing expression data of features of interest in rows and samples in columns.
#' @param title The title of the page.
#' @param menu (Optional) The name of the menu, under which the page should appear.
#'
#' @return A string containing markdown code for the rendered page.
#' @export
add_multi_gene_expression_page <- function(object, reduced_dim, expression, title = NULL, menu = NULL) {

  # validate if interactive mode is used
  if(!object@interactive) warning("This page can only be used with the shiny based interactive mode. Consider setting the 'interactive' argument during creation of the 'i2dashboard' object to 'TRUE'.")

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # set in list
  if(!is.list(reduced_dim)) reduced_dim <- list(reduced_dim)
  # if no names provided, set names
  if(is.null(names(reduced_dim))) {
    varname <- c('reduced_dim')
    names(reduced_dim) <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep="_"), varname, length(reduced_dim)))
  }
  # validate input
  if(!is.data.frame(expression) & !is.matrix(expression)) stop("'expression' should be of class 'data.frame' or 'matrix'.")
  l <- c(ncol(expression))
  for(df in reduced_dim){
    if(!is.data.frame(df) & !is.matrix(df)) stop("'reduced_dim' should be of class 'data.frame' or 'matrix'.")
    if(!all(sapply(df, is.numeric))) stop("The data.frame in 'reduced_dim' should contain numerical values only.")
    if(ncol(df) < 2 ) stop("'reduced_dim' should contain at least two columns.")
    l <- c(l, nrow(df))
  }
  if(length(unique(l)) > 1) stop("The number of columns of 'expression' should be equal to the number of rows in 'reduced_dim'.")
  if(!is.null(title) & !is.character(title)) stop("'title' should be a character vector.")

  # Create component environment
  env <- new.env()
  env$selection_reduced_dim <- length(reduced_dim) > 1
  env$reduced_dim <- reduced_dim
  env$expression <- expression

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  expanded_components <- list()
  timestamp <- Sys.time()

  # fill list "expanded_components" with components
  multi_gene_expr_component <- knitr::knit_expand(file = system.file("templates", "multi_gene_expression.Rmd", package = "i2dash.scrnaseq"), env_id = env_id, date = timestamp)

  # Expand component
  timestamp <- Sys.time()

  object@pages[["multi_gene_expression_page"]] <- list(title = title, layout = "empty", menu = menu, components = multi_gene_expr_component, max_components = 1, sidebar = NULL)
  return(object)
}
