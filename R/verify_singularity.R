#' Singularity verification
#' @param f Function to be checked (R function or an expression)
#' @param point Point around which to check analyticity (named vector)
#' @param variables Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @return List with verified results
#' @export

verify_singularities <- function(f, point, variables, epsilon) {
  n_tests <- 30
  valeurs <- numeric(n_tests)

  for (i in 1:n_tests) {
    point_test <- point + rnorm(length(point), 0, epsilon)
    names(point_test) <- names(point)

    valeurs[i] <- tryCatch(do.call(f, as.list(point_test)), error = function(e) NA)
  }

  # Detect singularities
  a_singularites <- any(is.na(valeurs)) ||
    any(!is.finite(valeurs)) ||
    (sd(valeurs, na.rm = TRUE) > 1000 * abs(mean(valeurs, na.rm = TRUE)))

  return(list(a_singularites = a_singularites, valeurs = valeurs))
}
