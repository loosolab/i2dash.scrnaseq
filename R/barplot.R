#' Renders a horizontal barplot
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param group_by A vector with factorial values or characters or a named list, which will be used for grouping the observations. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: If parameter 'x' is not provided (NULL), a barplot with the number of each 'level' is used as observation.
#' @param x (Optional) A vector with numerical values or a named list, which represents the observations for the horizontal barplot (x-axis). In case of a named list, a dropdown menu will be provided in the interactive mode.
#' @param title (Optional) The title of the components junk.
#' @param title_x (Optional) The title of the x-axis.
#' @param title_group_by (Optional) The title of the y-axis.
#'
#' @return A string containing markdown code for the rendered component.
#' @export
barplot <- function(object, group_by, x = NULL, title = NULL, title_x = NULL, title_group_by = NULL) {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # Create list if element is not a list already
  if(!is.list(group_by)) group_by <- list(group_by)
  if(!is.list(x) & !is.null(x)) x <- list(x)

  # name the lists
  if(is.null(names(group_by))) x %<>% magrittr::set_names("sample")
  if(is.null(names(x)) & !is.null(x)) x %<>% magrittr::set_names("values")

  # Validate input
  if(any(sapply(group_by, is.character))){
    clust_names <- names(group_by[sapply(group_by, class) == 'character'])
    for (name in clust_names){
      group_by[[name]] <- as.factor(group_by[[name]])
    }
  }
  if(!all(sapply(group_by, is.factor))) stop("'group_by' should only contain factorial values.")
  if(!all(sapply(x, is.factor)) & !is.null(x)) stop("'x' should only contain factorial values.")

  # Create component environment
  env <- new.env()

  env$group_by_selection <- FALSE
  env$x_selection <- FALSE
  env$title_group_by <- title_group_by
  env$title_x <- title_x

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  env$x <- x
  env$x_selection <- length(env$x) > 1

  # save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "barplot.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
