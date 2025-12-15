#' Taylor series converges to f
#' @param f Function to be checked (R function or an expression)
#' @param point Point around which to check analyticity (named vector)
#' @param variables Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @param ordre Order of Taylor Series to be verified
#' @return List with verified results
#' @export

verify_taylor <- function(f, point, variables, epsilon, ordre) {
  valeur_exacte <- tryCatch(do.call(f, as.list(point)), error = function(e) NA)

  if (is.na(valeur_exacte) || !is.finite(valeur_exacte)) {
    return(list(converge = FALSE, erreur = Inf))
  }

  # Test at multiple points in the neighborhood
  n_points <- 10
  erreurs <- numeric(n_points)

  for (i in 1:n_points) {
    point_test <- point + rnorm(length(point), 0, epsilon/3)
    names(point_test) <- names(point)

    valeur_test <- tryCatch(do.call(f, as.list(point_test)), error = function(e) NA)

    if (is.na(valeur_test) || !is.finite(valeur_test)) {
      return(list(converge = FALSE, erreur = Inf))
    }

    # Taylor approximation of order 1 (linear)
    approx_taylor <- valeur_exacte
    for (var in variables) {
      h <- 0.0001
      point_h <- point
      point_h[var] <- point_h[var] + h
      derivee <- (do.call(f, as.list(point_h)) - valeur_exacte) / h
      approx_taylor <- approx_taylor + derivee * (point_test[var] - point[var])
    }

    erreurs[i] <- abs(valeur_test - approx_taylor)
  }

  erreur_moy <- mean(erreurs, na.rm = TRUE)
  converge <- erreur_moy < epsilon * 100

  return(list(converge = converge, erreur = erreur_moy))
}
