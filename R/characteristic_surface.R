#' Condition 3: Non-characteristic surface
#' @param edp_coefficients PDEs coefficients to be verified (R function or an expression or constant)
#' @param point_initial Point around which to check analyticity (named vector)
#' @param variable_temps time variable
#' @param variables_spatiales Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @return List with verified results
#' @export

verify_non_characteristic_surface <- function(edp_coefficients,
                                              point_initial,
                                              variable_temps = "t",
                                              variables_spatiales = c("x", "y"),
                                              epsilon = 0.001) {

  cat("=== Verification: Initial surface is non-characteristic ===\n\n")

  # For a surface t = t0 (constant), we must verify that
  # the coefficient of the highest time derivative is non-zero

  # Example: for u_tt - c^2*u_xx = 0
  # The coefficient A of u_tt must be != 0

  resultats <- list(
    non_caracteristique = FALSE,
    coefficient_temps = NA,
    details = ""
  )

  # Calculate the coefficient of the highest temporal derivative
  # at the initial point

  if ("A" %in% names(edp_coefficients)) {
    # Case where coefficient A is explicitly given
    coeff_A <- edp_coefficients$A

    if (is.function(coeff_A)) {
      valeur_A <- tryCatch(
        do.call(coeff_A, as.list(point_initial)),
        error = function(e) NA
      )
    } else {
      valeur_A <- coeff_A
    }

    resultats$coefficient_temps <- valeur_A

    cat(sprintf("Coefficient of highest temporal derivative: A = %g\n", valeur_A))

    # Non-nullity test
    if (is.na(valeur_A)) {
      cat("[X] Coefficient A is not defined at the initial point\n")
      resultats$non_caracteristique <- FALSE
      resultats$details <- "Undefined coefficient"
    } else if (abs(valeur_A) < epsilon) {
      cat(sprintf("[X] Coefficient A is approximately 0 (|A| = %g < %g)\n", abs(valeur_A), epsilon))
      cat("[X] The surface is CHARACTERISTIC\n")
      resultats$non_caracteristique <- FALSE
      resultats$details <- "Null coefficient"
    } else {
      cat(sprintf("[OK] Coefficient A = %g != 0\n", valeur_A))
      cat("[OK] The surface is NON-CHARACTERISTIC\n")
      resultats$non_caracteristique <- TRUE
      resultats$details <- "Non-null coefficient"
    }

  } else {
    cat("[!] Coefficient structure not recognized\n")
    cat("[!] Cannot verify automatically\n")
    cat("--> Manual verification required\n")
    resultats$details <- "Manual verification required"
  }

  # Conclusion
  cat("\n=== RESULT ===\n")
  if (resultats$non_caracteristique) {
    cat("[OK] The initial surface is NON-CHARACTERISTIC\n")
    cat("[OK] Condition 3 of Cauchy-Kowalevski SATISFIED\n")
    resultats$condition_3_satisfaite <- TRUE
  } else {
    cat("[X] The initial surface is CHARACTERISTIC or undetermined\n")
    cat("[X] Condition 3 of Cauchy-Kowalevski NOT SATISFIED\n")
    cat("[!] The Cauchy-Kowalevski theorem does NOT apply\n")
    resultats$condition_3_satisfaite <- FALSE
  }

  return(invisible(resultats))
}
