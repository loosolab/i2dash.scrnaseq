#' Renders a Sequence saturation plot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param red_dim List with reduced dimensions as data.frames.
#' @param coulor_by A factor that will be mapped to colours.In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param title A title that will be displayed on top.
#'
#' @return A string containing markdown code for the rendered textbox
sequence_saturation <- function(object, red_dim, colour_by = NULL, title = "Reduced dimensions plot") {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_sequence_saturation(object@workdir, env_id, red_dim, colour_by)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "reduced_dimensions_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_sequence_saturation <- function(workdir, env_id, red_dim, colour_by) {
  env <- new.env()
  env$red_dim_selection <- FALSE
  env$colour_by_selection <- FALSE

  # Create lists if needed
  if(!is.list(red_dim)) red_dim <- list(red_dim = red_dim)
  if(!is.list(colour_by) & !is.null(colour_by)) colour_by <- list(colour_by)

  # Check validity
  if(!all(sapply(red_dim, is.data.frame))) stop("red_dim should only contain data.frame elements.")

  # Add objects to env
  env$red_dim <- red_dim
  env$red_dim_selection <- length(env$red_dim) > 1

  env$colour_by <- colour_by
  env$colour_by_selection <- length(env$colour_by) > 1

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
  print("validation TRUE")
}
