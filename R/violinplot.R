#' Renders a vertical violin plot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param y Numeric observations for the violinplot. In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param group_by A factor, by which observations can be grouped. In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param title_y A title of the y-axis that describes the observations. In case of a named list this parameter is not needed because the names of the list will be used as title of the y axis.
#' @param title_group_by A title of the x-axis that describes the grouping factor. In case of a named list this parameter is not needed because the names of the list will be used as title of the x axis.
#' @param title The title of the components junk.
#'
#' @return A string containing markdown code for the rendered component
violinplot <- function(object, y, group_by = NULL,  title = NULL, title_y = NULL, title_group_by = NULL) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Create list if element is not a list already
  if(!is.list(y)) y <- list(y = y)
  if(!is.list(group_by) & !is.null(group_by)) group_by  <- list(group_by)

  # Validate input
  if(!all(sapply(y, is.numeric))) stop("'y' should only contain numerical values.")
  if(!all(sapply(group_by, is.factor)) & !is.null(group_by)) stop("'group_by' should only contain factorial values.")

  # Create component environment
  env <- new.env()

  env$y_selection <- FALSE
  env$group_by_selection <- FALSE

  env$y <- y
  env$y_selection <- length(env$y) > 1

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  env$title_y <- title_y
  env$title_group_by <- title_group_by

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "violinplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
