# Useful functions for the article (e.g, functions to make publication-ready tables from R-output etc.)

#' Return estimates of relevant coefficients from INLA object
#'
#' @param inla.object INLA (measurement error) model
#'
#' @return A table with the posterior means and standard errors (and quantiles)
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
#' @return A deep cleaned version of the INLA model summary. Rows are given names from the INLA names, and numbers are rounded and padded with zeros when necessary.
#' @export
#'
#' @examples
prepare_table <- function(inla.object){
  # Extract estimates from inla.object
  estimates <- return_estimates(inla.object)

  # Identify alpha/beta rows
  beta_rows <- grepl("beta", row.names(estimates))

  # Extract posterior mean and sd, and round off
  est_table <- estimates[c("mean", "sd")] |>
    dplyr::mutate_if(is.numeric, round, digits = 2) |> # Round
    dplyr::mutate_if(is.numeric, format, nsmall = 2) |> # Format (so zeros at end don't get removed)
    dplyr::mutate(beta_indicator = ifelse(beta_rows, "beta", "alpha")) |>
    dplyr::arrange(beta_indicator)

  # Format rownames
  subscript <- sub(".*[.]", "", row.names(est_table))
  row.names(est_table) <- paste0("$", "\\", est_table$beta_indicator,
                                 "_{", subscript, "}", "$")

  # Sort so betas are first, and select relevant columns
  final_table <- est_table |>
    dplyr::arrange(desc(beta_indicator)) |>
    dplyr::select(mean, sd)

  return(final_table)
}

#' Make the body of a Latex table from INLA object
#'
#' @param inla.object INLA (measurement error) model
#'
#' @return A cleaned INLA parameter summary, with "&" and "\\\\" inserted where necessary to create the body of a LaTeX table, but without the surrounding environment (so that can be specified manually in the manuscript).
#' @export
#'
#' @examples
make_latex_table_body <- function(inla.object){
  clean_data <- prepare_table(inla.object) |>
    glue::glue_data("{rownames(.)} & {mean} & {sd} \\\\")
  return(clean_data)
}
