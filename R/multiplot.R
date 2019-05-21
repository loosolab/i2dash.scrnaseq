#' Renders a Sequence saturation plot
#'
#' @param plot_title The title of the Component
#' @param x A list with the x-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param y A list with the y-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param colour_by A list with the color_by values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#'
#' @return A string containing markdown code for the rendered textbox
multiplot <- function(object, x, y, title = "Multiplot", colour_by = NULL) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_multiplot(object@workdir, env_id, x, y, colour_by)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "multiplot_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_multiplot <- function(workdir, env_id, x, y, colour_by) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE
  env$colour_by_selection <- FALSE

  # Create lists if needed
  if(!is.list(x)) x <- list(x = x)
  if(!is.list(y)) y <- list(y = y)
  if(!is.list(colour_by) & !is.null(colour_by)) colour_by <- list(colour_by)

  # Check validity
  if(!all(sapply(x, is.numeric))) stop("x should only contain numeric values.")
  if(!all(sapply(y, is.factor))) stop("y should only contain factorial values.")

  # Add objects to env
  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$y <- y
  env$y_selection <- length(env$y) > 1

  env$colour_by <- colour_by
  env$colour_by_selection <- length(env$colour_by) > 1

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
}
