#' Renders a Sequence saturation plot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param x Numeric values mapped to the x-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param y Numeric values mapped to the y-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param coulor_by A factor that will be mapped to colours.In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param title A title that will be displayed on top.
#'
#' @return A string containing markdown code for the rendered textbox
sequence_saturation <- function(object, x, y, colour_by = NULL, title = "Sequencing saturation") {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_sequence_saturation(object@workdir, env_id, x, y, colour_by)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "sequence_saturation_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_sequence_saturation <- function(workdir, env_id, x, y, colour_by) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE
  env$colour_by_selection <- FALSE

  # Create lists if needed
  if(!is.list(x)) x <- list(x = x)
  if(!is.list(y)) y <- list(y = y)
  if(!is.list(colour_by)) colour_by <- list(colour_by)

  # Check validity
  if(!all(sapply(x, is.numeric))) stop("x should only contain numeric values.")
  if(!all(sapply(y, is.numeric))) stop("y should only contain numeric values.")

  # Add objects to env
  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$y <- y
  env$y_selection <- length(env$y) > 1

  env$colour_by <- colour_by
  env$colour_by_selection <- length(env$colour_by) > 1

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
  print("validation TRUE")
}
