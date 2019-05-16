#' Renders a boxplot for cluster characterization
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param cluster Values for the membership to clusters. In case of a nested list, a dropdown menu will be provided in the interactive mode.
#' @param x Numeric values mapped to the x-axis. In case of a nested list, a dropdown menu will be provided in the interactive mode. If x is NULL then a barplot for "Number of cells" per cluster will be created. If x is not NULL a barplot for "Fraction per cell" per cluster will be created.
#' @param title A title that will be displayed on top.
#'
#' @return A string containing markdown code for the rendered textbox
cluster_characterization_boxplot <- function(object, x, cluster, title = "Characterization of clusters") {
  # Create random env id
  env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

  # validate input, create environment variables, save environment object
  .validate_input_cluster_characterizatio_boxplot(object@workdir, env_id, x, cluster)

  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "cluster_characterization_boxplot_template.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.validate_input_cluster_characterizatio_boxplot <- function(workdir, env_id, x, cluster) {
  env <- new.env()
  env$x_selection <- FALSE
  env$cluster_selection <- FALSE

  # Check existence of cluster
  if(is.null(cluster)) stop("cluster is required.")
  if(is.null(x)) stop("x is required.")

  # Create lists if needed
  if(!is.list(x)) x <- list(x)
  if(!is.list(cluster)) cluster <- list(cluster)

  # should I use magrittr::%<>% ?
  # name the lists
  library(magrittr)
  if(is.null(names(cluster))) x %<>% magrittr::set_names("clustering.1")
  if(is.null(names(x))) x %<>% magrittr::set_names("x")

  # Check validity
  if(!all(sapply(cluster, is.factor))) stop("'cluster' should only contain factorial values.")
  if(!all(sapply(x, is.numeric))) stop("'x' should only contain numerical values.")

  # Check if lengths in a list are the same and if x and y and label and color_by are the same length
  #if(length(unique(sapply(x, length))) != 1) stop("list x should contain elements with the same length.")
  #if(length(unique(sapply(y, length))) != 1) stop("list y should contain elements with the same length.")

  #if(!identical(length(x[[1]]), length(y[[1]]))) stop("all arguments should be of the the same length.")
  #if(!identical(length(x[[1]]), length(colour_by[[1]])) & !is.null(colour_by)) stop("all arguments should be of the the same length.")
  #if(!identical(length(x[[1]]), length(labels[[1]])) & !is.null(labels)) stop("all arguments should be of the the same length.")

  # Add objects to env
  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$cluster <- cluster
  env$cluster_selection <- length(env$cluster) > 1

  # save environment as rds-object
  saveRDS(env, file = file.path(workdir, "envs", paste0(env_id, ".rds")))
  print("validation TRUE")
}
