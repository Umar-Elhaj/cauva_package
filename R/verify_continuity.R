#' Continuity of f
#' @param f Function to be checked (R function or an expression)
#' @param point Point around which to check analyticity (named vector)
#' @param variables Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @return List with verified results
#' @export

verify_continuity <- function(f, point, variables, epsilon) {
  n_tests <- 20 # we will test 20 points around x
  continu <- TRUE
  valeur_centre <- tryCatch(do.call(f, as.list(point)), error = function(e) NA) # if error, value becomes NA

  if (is.na(valeur_centre) || !is.finite(valeur_centre)) {
    return(list(continu = FALSE, valeur_centre = valeur_centre))
  }

  # Weierstrass theorem
  for (i in 1:n_tests) {
    point_voisin <- point + rnorm(length(point), 0, epsilon/2)
    names(point_voisin) <- names(point)

    valeur_voisin <- tryCatch(do.call(f, as.list(point_voisin)), error = function(e) NA)

    if (is.na(valeur_voisin) || !is.finite(valeur_voisin)) {
      continu <- FALSE
      break
    }

    if (abs(valeur_voisin - valeur_centre) > epsilon * 10) {
      continu <- FALSE
      break
    }
  }

  return(list(continu = continu, valeur_centre = valeur_centre))
}
