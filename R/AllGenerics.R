#' @importClassesFrom i2dash i2dashboard
NULL

#' Add a gene expression page.
#'
#' This function adds a page with two linked components to the \code{report} object: A scatterplot, showing samples in along two-dimensional coordinates, and a violin plot, showing feature expression values by groups defined in \code{group_by}.
#'
#' @name add_gene_expression_page
#' @rdname add_gene_expression_page
#' @exportMethod add_gene_expression_page
setGeneric("add_gene_expression_page", function(report, object, ...) standardGeneric("add_gene_expression_page"))
