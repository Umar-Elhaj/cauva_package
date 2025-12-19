#' Initial conditions Phi(u(t0,x)) and Psi(du/dt(t0,x)) are analytic
#' @param phi Function to be checked (R function or an expression)
#' @param psi du/dxi Function to be checked if PDE is of order 2 (R function or an expression)
#' @param points_test Point around which to check analyticity (named vector)
#' @param variables_spatiales Name of variables (default for dim 2)
#' @param epsilon Neighborhood radius to test
#' @param ordre_taylor Order of Taylor Series to be verified
#' @return List with verified results
#' @export

ck_initial <- function(phi, psi = NULL,
                      points_test,
                      variables_spatiales = c("x"),
                      epsilon = 0.001,
                      ordre_taylor = 3) {

  resultats <- list(
    phi_analytique = TRUE,
    psi_analytique = TRUE,
    details = list()
  )#result intialy true

  # Verify phi(x)
  cat("Test of phi(x) [condition on u(t0, x)]...\n\n")

  for (i in seq_along(points_test)) {
    point <- points_test[[i]]
    cat(sprintf("  Test point %d: %s\n", i, paste(names(point), "=", point, collapse=", ")))

    test_phi <- tryCatch({
      ck_analyticity(
        f = phi,
        point = point,
        variables = variables_spatiales,
        epsilon = epsilon,
        ordre_taylor = ordre_taylor
      ) #calling the function ck_analyticity to verify if test_phi is analytic
    }, error = function(e) {
      cat(sprintf(" [!] Error during test: %s\n", e$message))
      return(list(analytique = FALSE, raison = paste("Error:", e$message)))
    })

    resultats$details[[paste0("phi_point_", i)]] <- test_phi

    # Robust verification with NULL handling
    if (is.null(test_phi) || is.null(test_phi$analytique)) {
      resultats$phi_analytique <- FALSE
      cat("    [X] Test of phi failed (invalid result)\n\n")
    } else if (!test_phi$analytique) {
      resultats$phi_analytique <- FALSE
      cat("    [X] phi is NOT analytic at this point\n\n")
    } else {
      cat("    [OK] phi is analytic at this point\n\n")
    }
  }

  # Verify psi(x) if provided
  if (!is.null(psi)) {
    cat("\nTest of psi(x) [condition on du/dt(t0, x)]...\n\n")

    for (i in seq_along(points_test)) {
      point <- points_test[[i]]
      cat(sprintf("  Test point %d: %s\n", i, paste(names(point), "=", point, collapse=", ")))

      test_psi <- tryCatch({
        ck_analyticity(
          f = psi,
          point = point,
          variables = variables_spatiales,
          epsilon = epsilon,
          ordre_taylor = ordre_taylor
        )
      }, error = function(e) {
        cat(sprintf("    [!] Error during test: %s\n", e$message))
        return(list(analytique = FALSE, raison = paste("Error:", e$message)))
      })

      resultats$details[[paste0("psi_point_", i)]] <- test_psi

      # Robust verification with NULL handling
      if (is.null(test_psi) || is.null(test_psi$analytique)) {
        resultats$psi_analytique <- FALSE
        cat("    [X] Test of psi failed (invalid result)\n\n")
      } else if (!test_psi$analytique) {
        resultats$psi_analytique <- FALSE
        cat("    [X] psi is NOT analytic at this point\n\n")
      } else {
        cat("    [OK] psi is analytic at this point\n\n")
      }
    }
  }

  # Conclusion
  cat("=========================================================\n")
  if (resultats$phi_analytique && (is.null(psi) || resultats$psi_analytique)) {
    cat("[OK] Initial conditions are ANALYTIC\n")
    cat("[OK] Condition 2 of Cauchy-Kowalevski SATISFIED\n")
    resultats$condition_2_satisfaite <- TRUE
  } else {
    cat("[X] Initial conditions are NOT all analytic\n")
    cat("[X] Condition 2 of Cauchy-Kowalevski NOT SATISFIED\n")
    resultats$condition_2_satisfaite <- FALSE
  }
  cat("=========================================================\n\n")

  return(invisible(resultats))
}
