#' Differentiability of f
#' @param f Function to be checked (R function or an expression)
#' @param point Point around which to check analyticity (named vector)
#' @param variables Name of variables (default for dim 2)
#' @param h step size for differentiation
#' @return List with verified results
#' @export

verify_derivability <- function(f, point, variables, h = 0.0001) {
  derivable <- TRUE
  derivees <- list()

  for (var in variables) {
    point_plus <- point
    point_plus[var] <- point_plus[var] + h

    point_moins <- point
    point_moins[var] <- point_moins[var] - h

    val_plus <- tryCatch(do.call(f, as.list(point_plus)), error = function(e) NA)
    val_moins <- tryCatch(do.call(f, as.list(point_moins)), error = function(e) NA)

    if (is.na(val_plus) || is.na(val_moins) || !is.finite(val_plus) || !is.finite(val_moins)) {
      derivable <- FALSE
      break
    }

    derivees[[var]] <- (val_plus - val_moins) / (2 * h)
  }

  return(list(derivable = derivable, derivees = derivees))
}
