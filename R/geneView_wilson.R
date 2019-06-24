#' Method for geneView creation from 'wilson' package
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param data A vector with numerical values or a named list will be mapped to the x-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param group_by A vector with numerical values or a named list will be mapped to the y-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param title (Optional) The title of the components junk.
#' @param (...) Further parameters which are compatible with wilsons create_scatterplot() method. See \code{\link[wilson::create_scatterplot()]{wilson}}.
#'
#' @return A string containing markdown code for the rendered textbox
#' @export
scatterplot_wilson <- function(object, data, group_by, title = NULL, ...) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Create list if element is not a list already
  if(!is.list(data)) x <- list(data)
  if(!is.list(grouping)) grouping <- list(grouping)

  # Name the lists

  # Validate input

  # Check, if lengths in a list are the same and if x and y and label and color_by are the same length

  additional_arguments <- list(...)
  #if("data" %in% names(additional_arguments)) warning("The parameter 'data' will be used instead of 'data'")
  if("grouping" %in% names(additional_arguments)) warning("The parameter 'group_by' will be used instead of 'grouping'")
  valid_arguments <- names(as.list(args(wilson::create_scatterplot)))
  invalid_args <- setdiff(names(additional_arguments), valid_arguments)
  if(length(invalid_args) != 0) stop(paste0(" The following parameter is not a valid parameter of 'Wilson::create_scatterplot': ", invalid_args))
  #if(length(additional_arguments) == 0) additional_arguments <- NULL

  # Create component environment
  env <- new.env()

  env$data_selection <- FALSE
  env$group_by_selection <- FALSE

  env$data <- data
  env$data_selection <- length(env$data) > 1

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  env$additional_arguments <- additional_arguments

  # Save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "geneView_wilson.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
