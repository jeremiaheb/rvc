## Abundance function
## Acts as wrappers for density

#' Stratum abundance
#' @export
#' @description Number of individuals in a stratum based on count. \strong{Note:} Does
#' not take into account detection probability.
#' @inheritParams strat_density
#' @return A data.frame with abundance per stratum by species, its variance (var),
#' the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
#' possible number of SSUs (NM), and the total possible number of PSUs (N)
strat_abundance = function(x, ntot) {
  ## Calculate stratum level density
  sdens = strat_density(x, ntot)
  ## Convert density to abundance
  sdens$density = with(sdens, density*NM)
  sdens$var = with(sdens, var*NM^2)
  ## Rename density and return
  names(sdens)[names(sdens) == "density"] = "abundance"

  return(sdens)
}

#' Domain Abundance
#' @export
#' @description Number of individuals in a sampling domain based on count. \strong{Note:} Does
#' not take into account detection probability.
#' @inheritParams domain_density
#' @return A data.frame with abundance per sampling domain by species, its variance (var),
#' the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
#' possible number of SSUs (NM), and the total possible number of PSUs (N)
domain_abundance = function(x, ntot) {
  ## Calculate domain density
  dabun = domain_density(x, ntot)
  ## Convert density to abundance
  dabun$density = with(dabun, density*NM)
  dabun$var = with(dabun, var*NM^2)
  ## Rename desnity and return
  names(dabun)[names(dabun) == "density"] = "abundance"

  return(dabun)
}
