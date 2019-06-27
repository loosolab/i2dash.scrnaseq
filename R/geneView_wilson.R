#' Method for geneView creation from 'wilson' package
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param countTable A matrix with features as rows and observations as columns. The rownames and columnnames should be provided and are used in buiding the heatmap.
#' @param group_by A vector with values or a named list will be mapped to the y-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param title (Optional) The title of the components junk.
#' @param (...) Further parameters which are compatible with wilsons create_scatterplot() method. See \code{\link[wilson::create_scatterplot()]{wilson}}.
#'
#' @return A string containing markdown code for the rendered textbox
#' @export
geneview_wilson <- function(object, countTable, group_by, title = NULL, ...) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Create list if element is not a list already
  if(!is.list(group_by)) group_by <- list(group_by)

  # Name the lists
  if(is.null(names(group_by))) group_by %<>% magrittr::set_names("grouping")

  # Validate input
  if(length(unique(sapply(group_by, length))) != 1) stop("The list 'group_by' should contain elements with the same length.")
  if(!identical(ncol(countTable), length(group_by[[1]])) & !is.null(expression)) stop("The number of columns in 'countTable' should be equal to the length of the vector 'group_by'.")

  additional_arguments <- list(...)
  if("data" %in% names(additional_arguments)) warning("The parameter 'countTable' will be used instead of 'data'")
  if("grouping" %in% names(additional_arguments)) warning("The parameter 'group_by' will be used instead of 'grouping'")
  valid_arguments <- names(as.list(args(wilson::create_scatterplot)))
  invalid_args <- setdiff(names(additional_arguments), valid_arguments)
  if(length(invalid_args) != 0) stop(paste0(" The following parameter is not a valid parameter of 'Wilson::create_scatterplot': ", invalid_args))
  #if(length(additional_arguments) == 0) additional_arguments <- NULL

  # Create component environment
  env <- new.env()
  env$countTable <- countTable
  env$group_by_selection <- FALSE

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
