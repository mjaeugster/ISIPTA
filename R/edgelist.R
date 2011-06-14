
#' Create an edgelist matrix from a data frame.
#'
#' @param x Data set (\\code{data.frame} in long format) to convert
#'   into the edgelist matrix.
#' @param edge.var Edge variable within the data set.
#' @param node.var Node variable within the data set.
#' @return Edgelist matrix with two columns (vertices) and one row
#'   per edge.
#' @export
as.edgelist <- function(x, edge.var, node.var) {
  stopifnot(any(edge.var %in% names(x)))

  fromto <- split(x, x[, edge.var])
  fromto <- lapply(fromto,
                   function(x) {
                     ret <- NULL

                     if ( length(x[, node.var]) > 1 ) {
                       y <- combn(as.character(x[, node.var]), 2)

                       ret <- data.frame(from = y[1, ], to = y[2, ])
                       ret[[edge.var]] <- x[1, edge.var]

                       ret
                     }

                     ret
                   })
  fromto <- do.call(rbind, fromto)

  rownames(fromto) <- NULL
  attr(fromto, "nodes") <- as.character(unique(x[, node.var]))

  fromto
}

