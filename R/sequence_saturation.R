#' Renders a Sequence saturation plot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param x Numeric values mapped to the x-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param y Numeric values mapped to the y-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param colour_by A factor that will be mapped to colours. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param labels A list with sample names, that should be of the same length as x and y.
#' @param title A title that will be displayed on top.
#'
#' @return A string containing markdown code for the rendered textbox
sequence_saturation <- function(object, x, y, colour_by = NULL, labels = NULL, title = "Sequencing saturation") {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_sequence_saturation(object@workdir, env_id, x, y, colour_by, labels)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "sequence_saturation_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_sequence_saturation <- function(workdir, env_id, x, y, colour_by, labels) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE
  env$colour_by_selection <- FALSE

  # Create lists if needed
  if(!is.list(x)) x <- list(x)
  if(!is.list(y)) y <- list(y)
  if(!is.list(colour_by) & !is.null(colour_by)) colour_by <- list(colour_by)
  if(!is.list(labels) & !is.null(labels)) labels <- list(labels)

  # should I use magrittr::%<>% ?
  # name the lists
  library(magrittr)
  if(is.null(names(x))) x %<>% magrittr::set_names("x")
  if(is.null(names(y))) y %<>% magrittr::set_names("y")
  if(is.null(names(colour_by)) & !is.null(colour_by)) colour_by %<>% magrittr::set_names("colour")
  if(is.null(names(labels)) & !is.null(labels)) labels %<>% magrittr::set_names("labels")

  # Check existence of x and y
  if(is.null(x)) stop("x is required.")
  if(is.null(y)) stop("y is required.")

  # Check validity
  if(!all(sapply(x, is.numeric))) stop("x should only contain numeric values.")
  if(!all(sapply(y, is.numeric))) stop("y should only contain numeric values.")

  # Check if lengths in a list are the same and if x and y and label and color_by are the same length
  if(length(unique(sapply(x, length))) != 1) stop("list x should contain elements with the same length.")
  if(length(unique(sapply(y, length))) != 1) stop("list y should contain elements with the same length.")
  if(length(unique(sapply(colour_by, length))) != 1  & !is.null(colour_by)) stop("list colour_by should contain elements with the same length.")
  if(length(unique(sapply(labels, length))) != 1  & !is.null(labels)) stop("list labels should contain elements with the same length.")

  if(!identical(length(x[[1]]), length(y[[1]]))) stop("all arguments should be of the the same length.")
  if(!identical(length(x[[1]]), length(colour_by[[1]])) & !is.null(colour_by)) stop("all arguments should be of the the same length.")
  if(!identical(length(x[[1]]), length(labels[[1]])) & !is.null(labels)) stop("all arguments should be of the the same length.")

  # Add objects to env
  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$y <- y
  env$y_selection <- length(env$y) > 1

  env$colour_by <- colour_by
  env$colour_by_selection <- length(env$colour_by) > 1

  env$labels <- labels

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
  print("validation TRUE")
}
