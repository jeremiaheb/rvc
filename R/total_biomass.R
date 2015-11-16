#' Total stratum level biomass
#' @export
#' @description
#' Total biomass on the stratum level. Estimated from count
#' data and allometric growth coefficients. \strong{Note:}
#' Does not take into account variance in growth parameters
#' or detection probability
#' @inheritParams strat_biomass
#' @return A data.frame including total stratum biomass (total_biomass),
#' the average variance in total_biomass (var), number of PSUs sampled (n),
#' number of SSUs sampled (nm), number of possible PSUs (N), number of
#' possible SSUs (NM)
strat_total_biomass = function(x, ntot) {
  ## Stratum biomass per SSU
  sbiom = strat_biomass(x, ntot)
  ## Multiply by number of SSUs
  sbiom$biomass = with(sbiom, biomass*NM)
  sbiom$var = with(sbiom, var*NM^2)
  ## Rename biomass to total_biomass
  names(sbiom)[names(sbiom) == "biomass"] = "total_biomass"

  return(sbiom)
}

#' Total sampling domain level biomass
#' @export
#' @description
#' Total biomass on the domain level. Estimated from count
#' data and allomeric growth coefficients. \strong{Note:}
#' Does not take into account variance in growth parameters
#' or detection probability.
#' @inheritParams strat_density
#' @return A data.frame including total sampling domain biomass (total_biomass)
#' the average variance in total_biomass (var), number of PSUs sampled (n),
#' number of SSUs sampled (nm), number of possible PSUs (N), number of
#' possible SSUs (NM)
domain_total_biomass = function(x, ntot) {
  ## Domain biomass per SSU
  dbiom = domain_biomass(x, ntot)
  ## Multiply by number of SSUs
  dbiom$biomass = with(dbiom, NM*biomass)
  dbiom$var = with(dbiom, NM^2*var)
  ## Rename biomass to total_biomass
  names(dbiom)[names(dbiom) == "biomass"] = "total_biomass"

  return(dbiom)
}
