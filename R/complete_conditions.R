#' Complete verification of the 3 conditions and theorem application
#' @param edp_fonction The PDE fonction to be check
#' @param edp_coefficients PDEs coefficients to be verified (R function or an expression or constant)
#' @param point_test_edp Point around which to check CK conditions
#' @param variables_edp Variables of the PDE Equation
#' @param phi Function to be checked (R function or an expression)
#' @param psi du/dxi Function to be checked if PDE is of order 2 (R function or an expression)
#' @param points_test_CI Point around which to check analyticity (named vector)
#' @param variables_spatiales Name of variables (default for dim 2)
#' @param variable_temps time variable
#' @param epsilon Neighborhood radius to test
#' @param ordre_taylor Order of Taylor Series to be verified
#' @return List with verified results
#' @export

verify_complete_CK_conditions <- function(
  edp_fonction,
  edp_coefficients,
  point_test_edp,
  variables_edp,

  phi,
  psi = NULL,
  points_test_CI,
  variables_spatiales,

  variable_temps = "t",
  epsilon = 0.001,
  ordre_taylor = 3
) {

  cat("\n\n")
  cat("+=========================================================+\n")
  cat("+   COMPLETE VERIFICATION: CAUCHY-KOWALEVSKI THEOREM     +\n")
  cat("+=========================================================+\n")

  resultats_globaux <- list()

  # CONDITION 1: Analytic equation
  cat("\n+=========================================================+\n")
  cat("| CONDITION 1: Analytic equation                          |\n")
  cat("+=========================================================+\n")

  resultats_globaux$condition_1 <- tryCatch({
    verify_analyticity_CK(
      f = edp_fonction,
      point = point_test_edp,
      variables = variables_edp,
      epsilon = epsilon,
      ordre_taylor = ordre_taylor
    )
  }, error = function(e) {
    cat(sprintf("[!] Error during verification: %s\n", e$message))
    return(list(est_analytique = FALSE, raison = paste("Error:", e$message)))
  })

  # CONDITION 2: Analytic initial conditions
  cat("\n+=========================================================+\n")
  cat("| CONDITION 2: Analytic initial conditions                |\n")
  cat("+=========================================================+\n")

  resultats_globaux$condition_2 <- tryCatch({
    verify_initial_conditions_CK(
      phi = phi,
      psi = psi,
      points_test = points_test_CI,
      variables_spatiales = variables_spatiales,
      epsilon = epsilon,
      ordre_taylor = ordre_taylor
    )
  }, error = function(e) {
    cat(sprintf("[!] Error during verification: %s\n", e$message))
    return(list(condition_2_satisfaite = FALSE, raison = paste("Error:", e$message)))
  })

  # CONDITION 3: Non-characteristic surface
  cat("\n+=========================================================+\n")
  cat("| CONDITION 3: Non-characteristic surface                 |\n")
  cat("+=========================================================+\n")

  resultats_globaux$condition_3 <- tryCatch({
    verify_non_characteristic_surface(
      edp_coefficients = edp_coefficients,
      point_initial = points_test_CI[[1]],
      variable_temps = variable_temps,
      epsilon = epsilon
    )
  }, error = function(e) {
    cat(sprintf("[!] Error during verification: %s\n", e$message))
    return(list(condition_3_satisfaite = FALSE, raison = paste("Error:", e$message)))
  })

  # GENERAL CONCLUSION
  cat("\n\n")
  cat("+=========================================================+\n")
  cat("+                    GENERAL CONCLUSION                   +\n")
  cat("+=========================================================+\n\n")

  # ROBUST extraction of results with default values
  cond1_ok <- FALSE
  if (!is.null(resultats_globaux$condition_1) &&
      !is.null(resultats_globaux$condition_1$est_analytique)) {
    cond1_ok <- resultats_globaux$condition_1$est_analytique
  }

  cond2_ok <- FALSE
  if (!is.null(resultats_globaux$condition_2) &&
      !is.null(resultats_globaux$condition_2$condition_2_satisfaite)) {
    cond2_ok <- resultats_globaux$condition_2$condition_2_satisfaite
  }

  cond3_ok <- FALSE
  if (!is.null(resultats_globaux$condition_3) &&
      !is.null(resultats_globaux$condition_3$condition_3_satisfaite)) {
    cond3_ok <- resultats_globaux$condition_3$condition_3_satisfaite
  }

  cat("Summary of conditions:\n\n")
  cat(sprintf("  [%s] Condition 1: Analytic equation\n", if(cond1_ok) "OK" else "X"))
  cat(sprintf("  [%s] Condition 2: Analytic initial conditions\n", if(cond2_ok) "OK" else "X"))
  cat(sprintf("  [%s] Condition 3: Non-characteristic surface\n\n", if(cond3_ok) "OK" else "X"))

  if (cond1_ok && cond2_ok && cond3_ok) {
    cat("=========================================================\n")
    cat("  [OK][OK][OK] ALL CONDITIONS ARE SATISFIED [OK][OK][OK]\n")
    cat("=========================================================\n\n")
    cat("*** The CAUCHY-KOWALEVSKI THEOREM applies!\n\n")
    cat("--> The PDE admits a UNIQUE ANALYTIC SOLUTION\n")
    cat("  in a neighborhood of the initial point.\n\n")
    resultats_globaux$theoreme_applicable <- TRUE
  } else {
    cat("=========================================================\n")
    cat("  [X] SOME CONDITIONS ARE NOT SATISFIED\n")
    cat("=========================================================\n\n")
    cat("[!] The CAUCHY-KOWALEVSKI THEOREM does NOT apply\n\n")

    # Details on unsatisfied conditions
    if (!cond1_ok) {
      cat("  -> Condition 1 failed: The equation is not analytic\n")
    }
    if (!cond2_ok) {
      cat("  -> Condition 2 failed: Initial conditions are not analytic\n")
    }
    if (!cond3_ok) {
      cat("  -> Condition 3 failed: The surface is characteristic\n")
    }
    cat("\n")

    resultats_globaux$theoreme_applicable <- FALSE
  }

  return(invisible(resultats_globaux))
}
