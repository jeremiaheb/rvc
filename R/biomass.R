## Biomass functions

## SSU Biomass
## @export
## @description
## Biomass (in kg) per secondary sampling unit
## @param x
## A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
## STATION_NR, SPECIES_CD, NUM, LEN
## @param growth_parameters
## A data.frame or list of allometric growth parameters. If a list: must contain variables named 'WLEN_A' and
## 'WLEN_B' representing the linear and exponential coefficient of the allometric growth equation. If a
## data.frame: must contain columns named  WLEN_A and WLEN_B as well as a SPECIES_CD column.
## \strong{NOTE:} using a list means that only one set of growth parameters will be used for all species,
## while passing in a data.frame will allow the function to lookup the growth_parameters for each species
## @return A data.frame containing a column biomass with the biomass per secondary sampling unit
## @details
## The form of the allometric growth equation used in calculating biomass is:
## \deqn{
##  W(kg) = (a(g/mm)(L(cm)*10)^b)/1000
## }
#' @importFrom dplyr filter inner_join mutate rename
#' @importFrom magrittr %>%
#' @importFrom rlang .data

ssu_biomass = function(x, growth_parameters) {
  ## Subset x by LEN >= 0
  res <- x %>%
    dplyr::filter(.data$LEN >= 0)
  
  ## If growth parameters is a data.frame, join with x
  if(is.data.frame(growth_parameters)){
    res <- res %>%
      dplyr::inner_join(growth_parameters, by = "SPECIES_CD")
  } else {
    ## If its a list, create columns for growth_parameters
    res <- res %>%
      dplyr::mutate(
        WLEN_A = growth_parameters$WLEN_A,
        WLEN_B = growth_parameters$WLEN_B
      )
  }
  
  ## Put biomass into the NUM column
  res <- res %>%
    dplyr::mutate(
      NUM = (.data$NUM * .data$WLEN_A * (.data$LEN * 10)^.data$WLEN_B) / 1000
    )
  
  ## Use ssu_density function to calculate biomass and rename column
  ssbiom <- ssu_density(res) %>%
    dplyr::rename(biomass = "density") %>%
    as.data.frame()

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
  res <- x %>%
    dplyr::rename(density = "biomass") %>%
    psu_density() %>%
    dplyr::rename(biomass = "density") %>%
    as.data.frame()
    
  return(res)
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
  res <- x %>%
    dplyr::rename(density = "biomass") %>%
    strat_density(ntot) %>%
    dplyr::rename(biomass = "density") %>%
    as.data.frame()
    
  return(res)
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
  res <- x %>%
    dplyr::rename(density = "biomass") %>%
    domain_density(ntot) %>%
    dplyr::rename(biomass = "density") %>%
    as.data.frame()
    
  return(res)
}
