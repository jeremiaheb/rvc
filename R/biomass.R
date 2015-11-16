## Biomass functions

#' SSU Biomass
#' @export
#' @description
#' Biomass (in kg) per secondary sampling unit
#' @param x
#' A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
#' STATION_NR, SPECIES_CD, NUM, LEN
#' @param growth_parameters
#' A list of allometric growth parameters, containing: a - the linear coefficient in g/mm, and b - the
#' exponential coefficient.
#' @return A data.frame containing a column biomass with the biomass per secondary sampling unit
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'  W(kg) = (a(g/mm)(L(cm)*10)^b)/1000
#' }
ssu_biomass = function(x, growth_parameters) {
  if(is.null(growth_parameters$a) || is.null(growth_parameters$b))
    stop("cannot find growth parameters named 'a' and 'b'")
  ## Subset x by LEN >= 0
  x = subset(x, LEN >= 0)
  ## Put biomass into the NUM column
  x$NUM = with(growth_parameters, (x$NUM * a * (x$LEN*10)^b)/1000)
  ## Use ssu_density function to calculate biomass
  ssbiom = ssu_density(x)
  ## Change the name of the density column to biomass
  names(ssbiom)[names(ssbiom) == 'density'] = 'biomass'

  return(ssbiom)
}

#' PSU biomass
#' @export
#' @description
#' Biomass (in kg) per secondary sampling unit for each
#' primary sampling unit
#' @param x
#' A data.frame which is the output of ssu_biomass
#' @return A data.frame with the average biomass per secondary sampling unit
#' for each primary sampling unit, and the variance thereof
psu_biomass = function(x) {
  ## Wrap the psu_density function renaming density to biomass
  .wrapFunction(x, "biomass", "density", psu_density)
}

#' Stratum level biomass
#' @export
#' @description
#' Biomass (in kg) per secondary sampling unit for each
#' stratum
#' @param x
#' A data.frame which is the output of psu_biomass
#' @inheritParams strat_density
#' @return A data.frame with the average biomass per secondary sampling unit for each stratum,
#' its variance (var), the number of PSUs sampled (n), the number of SSUs sampled (nm),
#' the number of possible PSUs (N), and the number of possible SSUs (NM)
strat_biomass = function(x, ntot) {
  ## Wrap the strat_density function renaming density to biomass
  .wrapFunction(x, "biomass", "density", strat_density, ntot)
}

#' Domain level biomass
#' @export
#' @description
#' Biomass (in kg) per secondary sampling unit for each stratum
#' @param x
#' A data.frame which is the output of strat_biomass
#' @inheritParams strat_density
#' @return  A data.frame with the average biomass per secondary sampling unit for each sampling domain,
#' its variance (var), the number of PSUs sampled (n), the number of SSUs sampled (nm),
#' the number of possible PSUs (N), and the number of possible SSUs (NM)
domain_biomass = function(x, ntot) {
  ## Wrap the domain_density function, renaming density to biomass
  .wrapFunction(x, "biomass", "density", domain_density, ntot)
}
