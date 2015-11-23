## Biomass functions

## SSU Biomass
## @export
## @description
## Biomass (in kg) per secondary sampling unit
## @param x
## A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
## STATION_NR, SPECIES_CD, NUM, LEN
## @param growth_parameters
## A data.frame or list of allometric growth parameters. If a list: must contain variables named 'a' and 'b'
## representing the linear and exponential coefficient of the allometric growth equation (see details). If a
## data.frame: must contain columns named a and b (or WLEN_A and WLEN_B) as well as a SPECIES_CD column.
## \strong{NOTE:} using a list means that only one set of growth parameters will be used for all species,
## while passing in a data.frame will allow the function to lookup the growth_parameters for each species
## @return A data.frame containing a column biomass with the biomass per secondary sampling unit
## @details
## The form of the allometric growth equation used in calculating biomass is:
## \deqn{
##  W(kg) = (a(g/mm)(L(cm)*10)^b)/1000
## }
ssu_biomass = function(x, growth_parameters) {
  ## Subset x by LEN >= 0
  x = subset(x, LEN >= 0)
  ## If variables/items named a and b, change to WLEN_A and WLEN_B
  ngp = names(growth_parameters)
  names(growth_parameters)[ngp == 'a'] = "WLEN_A"
  names(growth_parameters)[ngp == 'b'] = "WLEN_B"
  ## If growth parameters is a data.frame, merge with x
  if(is.data.frame(growth_parameters)){
    ## Check that WLEN_A and WLEN_B columns are available
    if(!all(c("WLEN_A", "WLEN_B", "SPECIES_CD") %in% names(growth_parameters))){
      stop("could not find columns 'WLEN_A'|'a', 'WLEN_B'|'b' or 'SPECIES_CD' in growth_parameters")
    }
    ## Merge by species code
    else {
      merged = merge(x, growth_parameters, by = "SPECIES_CD")
    }
  }
  ## If its a list, create columns for growth_parameters
  else {
    ## Make sure items named WLEN_A and WLEN_B exist
    if(!all(c("WLEN_A","WLEN_B") %in% names(growth_parameters))){
      stop("could not find growth parameters named 'a' and 'b' or 'WLEN_A', 'WLEN_B' in growth_parameters")
    }
    ## Create a new data frame with the growth_parameters added on
    merged = x
    merged$WLEN_A = with(growth_parameters, rep(WLEN_A, nrow(x)))
    merged$WLEN_B = with(growth_parameters, rep(WLEN_B, nrow(x)))
  }
  ## Put biomass into the NUM column
  merged$NUM = with(merged, (NUM * WLEN_A * (LEN*10)^WLEN_B)/1000)
  ## Use ssu_density function to calculate biomass
  ssbiom = ssu_density(merged)
  ## Change the name of the density column to biomass
  names(ssbiom)[names(ssbiom) == 'density'] = 'biomass'

  return(ssbiom)
}

## PSU biomass
## @export
## @description
## Biomass (in kg) per secondary sampling unit for each
## primary sampling unit
## @param x
## A data.frame which is the output of ssu_biomass
## @return A data.frame with the average biomass per secondary sampling unit
## for each primary sampling unit, and the variance thereof
psu_biomass = function(x) {
  ## Wrap the psu_density function renaming density to biomass
  .wrapFunction(x, "biomass", "density", psu_density)
}

## Stratum level biomass
## @export
## @description
## Biomass (in kg) per secondary sampling unit for each
## stratum
## @param x
## A data.frame which is the output of psu_biomass
## @inheritParams strat_density
## @return A data.frame with the average biomass per secondary sampling unit for each stratum,
## its variance (var), the number of PSUs sampled (n), the number of SSUs sampled (nm),
## the number of possible PSUs (N), and the number of possible SSUs (NM)
strat_biomass = function(x, ntot) {
  ## Wrap the strat_density function renaming density to biomass
  .wrapFunction(x, "biomass", "density", strat_density, ntot)
}

## Domain level biomass
## @export
## @description
## Biomass (in kg) per secondary sampling unit for each stratum
## @param x
## A data.frame which is the output of strat_biomass
## @inheritParams strat_density
## @return  A data.frame with the average biomass per secondary sampling unit for each sampling domain,
## its variance (var), the number of PSUs sampled (n), the number of SSUs sampled (nm),
## the number of possible PSUs (N), and the number of possible SSUs (NM)
domain_biomass = function(x, ntot) {
  ## Wrap the domain_density function, renaming density to biomass
  .wrapFunction(x, "biomass", "density", domain_density, ntot)
}
