## Abundance function
## Acts as wrappers for density

#' Stratum abundance
#' @export
#' @description Number of individuals per stratum based on count. \strong{Note:} Does
#' not take into account detection probability.
#' @inheritParams strat_density
#' @return A data.frame with abundance per stratum by species, its variance (var),
#' the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
#' possible number of SSUs (NM), and the total possible number of PSUs (N)
strat_abundance = function(x, ntot) {
  ## Calculate stratum level density
  sdens = strat_density(x, ntot)
  ## Convert density to abundance
  sdens$density = with(sdens, density*N)
  sdens$var = with(sdens, var*N^2)
  ## Rename density and return
  names(sdens)[names(sdens) == "density"] = "abundance"

  return(sdens)
}
