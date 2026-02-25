## Total stratum level biomass
## @export
## @description
## Total biomass on the stratum level. Estimated from count
## data and allometric growth coefficients. \strong{Note:}
## Does not take into account variance in growth parameters
## or detection probability
## @inheritParams strat_biomass
## @return A data.frame including total stratum biomass (total_biomass),
## the average variance in total_biomass (var), number of PSUs sampled (n),
## number of SSUs sampled (nm), number of possible PSUs (N), number of
## possible SSUs (NM)
#' @importFrom dplyr mutate rename
#' @importFrom magrittr %>%
#' @importFrom rlang .data

strat_total_biomass = function(x, ntot) {
  
  res <- strat_biomass(x, ntot) %>%
    dplyr::mutate(
      var = .data$var * (.data$NM^2),
      biomass = .data$biomass * .data$NM
    ) %>%
    dplyr::rename(total_biomass = "biomass") %>%
    as.data.frame()
    
  return(res)
}

## Total sampling domain level biomass
## @export
## @description
## Total biomass on the domain level. Estimated from count
## data and allomeric growth coefficients. \strong{Note:}
## Does not take into account variance in growth parameters
## or detection probability.
## @inheritParams strat_density
## @return A data.frame including total sampling domain biomass (total_biomass)
## the average variance in total_biomass (var), number of PSUs sampled (n),
## number of SSUs sampled (nm), number of possible PSUs (N), number of
## possible SSUs (NM)
domain_total_biomass = function(x, ntot) {
  
  res <- domain_biomass(x, ntot) %>%
    dplyr::mutate(
      var = .data$var * (.data$NM^2),
      biomass = .data$biomass * .data$NM
    ) %>%
    dplyr::rename(total_biomass = "biomass") %>%
    as.data.frame()
    
  return(res)
}
