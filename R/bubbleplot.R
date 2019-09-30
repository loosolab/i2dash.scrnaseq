#' Renders a component containing a bubbleplot with optional selection options
#'
#' @param dashboard An object of class \linkS4class{i2dash::i2dashboard}.
#' @param x A data.frame (matrix) containing columns with numeric values that will be mapped to the x-axis.
#' @param y A data.frame (matrix) containing columns with numeric values that will be mapped to the y-axis.
#' @param size A ata.frame (matrix) containing columns with numeric values that describe the size of the bubbles.
#' @param colour_by An optional data.frame (matrix) containing columns with numeric or factorial values that will be used for colouring.
#' @param labels An optional vector with sample names.
#' @param title The title of the components junk.
#' @param x_title An optional title of the x-axis. If not provided the column names from \code{x} are used instead.
#' @param y_title An optional title of the y-axis. If not provided the column names from \code{y}  are used instead.
#' @return A string containing markdown code for the rendered component
#' @export
bubbleplot <- function(dashboard, x, y, size, colour_by = NULL, labels = NULL, title = NULL, x_title = NULL, y_title = NULL) {
  # Validate input
  assertive.types::assert_is_any_of(x, c("data.frame", "matrix"))
  assertive.types::assert_is_any_of(y, c("data.frame", "matrix"))
  assertive.types::assert_is_any_of(size, c("data.frame", "matrix"))

  # select columns only containing numeric or integer values
  x %<>%
    as.data.frame() %>%
    dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
  y %<>%
    as.data.frame() %>%
    dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
  size %<>%
    as.data.frame() %>%
    dplyr::select_if(function(col) is.integer(col) | is.numeric(col))

  # provide column names
  if(is.null(colnames(x))) colnames(x) <- paste0("X_", 1:ncol(x))
  if(is.null(colnames(y))) colnames(y) <- paste0("Y_", 1:ncol(y))
  if(is.null(colnames(size))) colnames(size) <- paste0("Size_", 1:ncol(size))

  # check correct dimensions
  if(nrow(x) != nrow(y)) stop("The number of rows in 'x' and 'y' is not equal.")
  if(nrow(x) != nrow(size)) stop("The number of rows in 'x' and 'size' is not equal.")

  # check optional parameters
  if(!is.null(colour_by)){
    assertive.types::assert_is_any_of(colour_by, c("data.frame", "matrix"))
    colour_by %<>%
      as.data.frame() %>%
      dplyr::select_if(function(col) is.integer(col) | is.numeric(col) | is.factor(col))
    if(is.null(colnames(colour_by))) colnames(colour_by) <- paste0("Color_", 1:ncol(colour_by))
    if(nrow(x) != nrow(colour_by)) stop("The number of rows in 'x' and 'colour_by' is not equal.")
  }
  if(!is.null(labels)) assertive.types::assert_is_any_of(labels, "vector")
  if(!is.null(labels)) assertive.types::is_character(title)
  if(!is.null(labels)) assertive.types::is_character(x_title)
  if(!is.null(labels)) assertive.types::is_character(y_title)

  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Create component environment
  env <- new.env()
  env$x <- x
  env$y <- y
  env$size <- size
  env$colour_by <- colour_by
  env$labels <- labels
  env$x_title <- x_title
  env$y_title <- y_title

  saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "bubbleplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
