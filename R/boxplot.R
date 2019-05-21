#' Renders a boxplot for cluster characterization
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param x Numeric observations for the boxplot. In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param group_by A factor, by which observations can optionally be grouped. In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param title The title of the components junk.
#'
#' @return A string containing markdown code for the rendered component.
#'
#' @export
boxplot <- function(object, x, group_by, title = NULL) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  if(!is.list(x)) x <- list(x)
  if(!is.list(group_by)) group_by <- list(group_by)

  # Validate input
  if(!all(sapply(x, is.numeric))) stop("'x' should only contain numerical values.")
  if(!all(sapply(cluster, is.factor))) stop("'cluster' should only contain factorial values.")

  # Create component environment
  env <- new.env()
  env$x_selection <- FALSE
  env$group_by_selection <- FALSE

  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "boxplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
