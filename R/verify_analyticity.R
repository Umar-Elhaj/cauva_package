#' Solve Cauchy-Kovalevskaya theorem equation
#' Condition 1:
#' Analyticity checking function for the Cauchy-Kowalevski theorem
#' Order 2: Equation of the form F(x, y, u, u_x, u_y, u_xx, u_xy, u_yy) = 0
#' @param f Function to be checked (R function or an expression)
#' @param point Point around which to check analyticity (named vector)
#' @param variables Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @param ordre_taylor Order of Taylor series development to be verified
#' @return List with verified results
#' @export


verify_analyticity_CK <- function(f, point, variables = c("x", "y", "u", "ux", "uy", "uxx", "uxy", "uyy"),
                                  epsilon = 0.001, ordre_taylor = 5) {

  cat("=== ANALYTICITY VERIFICATION - CAUCHY-KOWALEVSKI THEOREM ===\n\n")

  resultats <- list(
    analytique = FALSE,
    tests_passes = c(),
    tests_echoues = c(),
    details = list()
  )

  # Test 1: Continuity in a neighborhood
  cat("Test 1: Continuity in a neighborhood...\n")
  test_continuite <- verify_continuity(f, point, variables, epsilon)

  if (test_continuite$continu) {
    resultats$tests_passes <- c(resultats$tests_passes, "Continuity")
    cat("  [OK] Continuous function at point\n\n")
  } else {
    resultats$tests_echoues <- c(resultats$tests_echoues, "Continuity")
    cat("  [X] Non continuous function at point\n\n")
    return(resultats)
  }

  # Test 2: Differentiability (existence of partial derivatives)
  cat("Test 2: Differentiability (existence of partial derivatives)...\n")
  test_derivabilite <- verify_derivability(f, point, variables, epsilon)

  if (test_derivabilite$derivable) {
    resultats$tests_passes <- c(resultats$tests_passes, "Derivability")
    cat("  [OK] Partial derivatives exist\n\n")
  } else {
    resultats$tests_echoues <- c(resultats$tests_echoues, "Derivability")
    cat("  [X] Problem with partial derivatives\n\n")
    return(resultats)
  }

  # Test 3: Convergence of Taylor series
  cat("Test 3: Convergence of Taylor series...\n")
  test_taylor <- verify_taylor(f, point, variables, epsilon, ordre_taylor)

  if (test_taylor$converge) {
    resultats$tests_passes <- c(resultats$tests_passes, "Taylor Series")
    cat("  [OK] Taylor series converges\n\n")
  } else {
    resultats$tests_echoues <- c(resultats$tests_echoues, "Taylor Series")
    cat("  [X] Taylor series diverges\n\n")
  }

  # Test 4: Singularity check
  cat("Test 4: Singularity check...\n")
  test_singularites <- verify_singularities(f, point, variables, epsilon)

  if (!test_singularites$a_singularites) {
    resultats$tests_passes <- c(resultats$tests_passes, "No singularities")
    cat("  [OK] No singularities detected\n\n")
  } else {
    resultats$tests_echoues <- c(resultats$tests_echoues, "Singularities present")
    cat("  [X] Singularities detected\n\n")
  }

  # Conclusion
  resultats$analytique <- length(resultats$tests_echoues) == 0
  resultats$details <- list(
    continuite = test_continuite,
    derivabilite = test_derivabilite,
    taylor = test_taylor,
    singularites = test_singularites
  )

  cat("=== FINAL RESULT ===\n")
  if (resultats$analytique) {
    cat("[OK] The function is ANALYTIC at the given point\n")
    cat("  The Cauchy-Kowalevski theorem applies!\n")
  } else {
    cat("[X] The function is NOT analytic\n")
    cat("  Failed tests:", paste(resultats$tests_echoues, collapse=", "), "\n")
  }

  return(invisible(resultats))
}

# Auxiliary functions


