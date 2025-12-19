#' Singularity verification of f
#' @param f Function to be checked (R function or an expression)
#' @param point Point aat which f shoul/shouldn't be defined
#' @param variables Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @return List with verified results
#' @noRd

verify_singularities <- function(f, point, variables, epsilon) {
  n_tests <- 30
  valeurs <- numeric(n_tests)

  for (i in 1:n_tests) {
    point_test <- point + rnorm(length(point), 0, epsilon)
    names(point_test) <- names(point)

    valeurs[i] <- tryCatch(do.call(f, as.list(point_test)), error = function(e) NA) #f(point_test)
  }

  # Detect singularities
  a_singularites <- any(is.na(valeurs)) ||
    any(!is.finite(valeurs)) ||
    (sd(valeurs, na.rm = TRUE) > 1000 * abs(mean(valeurs, na.rm = TRUE))) #small sd is good +> f varies slowly

  return(list(a_singularites = a_singularites, valeurs = valeurs))
}
