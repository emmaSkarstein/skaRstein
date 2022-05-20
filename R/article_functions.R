# Useful functions for the article (e.g, functions to make publication-ready tables from R-output etc.)

#' Return estimates of relevant coefficients from INLA object
#'
#' @param inla.object INLA (measurement error) model
#'
#' @return A table with the estimated means and standard errors (and quantiles)
#  of all estimated parameters in the model whose name contains "beta" or "alpha".
#' @export
#'
#' @examples
return_estimates <- function(inla.object){
  fixed <- data.frame(inla.object$summary.fixed)
  fixed <- fixed[, names(fixed) != "kld"]
  fixed$coefficient.name <- rownames(fixed)
  hyperpar <- data.frame(inla.object$summary.hyperpar)
  hyperpar$coefficient.name <- rownames(hyperpar)

  # Extract all coefficients that contain "beta" (the coefficients of the model of interest)
  betas <- rbind(dplyr::filter(fixed, grepl("beta", coefficient.name)),
                 dplyr::filter(hyperpar, grepl("beta", coefficient.name)))
  # Extract all coefficients that contain "alpha" (the coefficients of the imputation model)
  alphas <- dplyr::filter(fixed, grepl("alpha", coefficient.name))

  return(rbind(alphas, betas))
}

#' Prepare INLA summary as a clean table
#'
#' @param inla.object INLA (measurement error) model
#'
#' @return A deep cleaned version of the INLA model summary that is suitable for including directly in article.
#' @export
#'
#' @examples
prepare_table <- function(inla.object){
  estimates <- return_estimates(inla.object)
  est_table <- estimates[c("mean", "sd")] %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    dplyr::mutate(Estimate = paste0(mean, " (", sd, ")"))
  beta_rows <- grepl("beta", row.names(est_table))
  greekletter <- ifelse(beta_rows, "beta", "alpha")
  subscript <- sub(".*[.]", "", row.names(est_table))
  row.names(est_table) <- paste0("$", "\\", greekletter, "_{", subscript, "}", "$")

  final.table <- est_table %>% select(Estimate)

  return(final.table)
}
