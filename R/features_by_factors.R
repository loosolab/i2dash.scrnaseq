#' Renders a features by factor violin plot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param title A title that will be displayed on top.
#' @param x A list with the x-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param y A list with the y-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.(Needs to be categorial. Horizontal violinplots are not possible.)
#'
#' @return A string containing markdown code for the rendered textbox
features_by_factors <- function(object, x, y, title = "Features by factor") {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_features_by_factors(object@workdir, env_id, x, y)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "features_by_factors_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_features_by_factors <- function(workdir, env_id, x, y) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE

  # Create lists if needed
  if(!is.list(x)) x <- list(x = x)
  if(!is.list(y)) y <- list(y = y)

  # Check validity
  if(!all(sapply(y, is.numeric))) stop("y should only contain numeric values.")
  if(!all(sapply(x, is.factor))) stop("x should only contain factorial values.")

  # Add objects to env
  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$y <- y
  env$y_selection <- length(env$y) > 1

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
  print("validation TRUE")
}
