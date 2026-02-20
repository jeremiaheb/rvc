## Abundance function
## Acts as wrappers for density

## PSU abundance
## @export
## @description Number of individuals in each PSU. \strong{Note:} Does
## not take into account detection probability.
## @inheritParams strat_density
## @return A data.frame with abundance per PSU by species, its variance (var),
## the number of SSUs per PSU (m)
#' @importFrom dplyr mutate rename
#' @importFrom rlang .data
psu_abundance = function(x) {
  ## Convert density to abundance and rename
  res = x %>%
    dplyr::mutate(
      var = .data$var * (.data$m^2),
      density = .data$density * .data$m
    ) %>%
    dplyr::rename(abundance = "density") %>%
    as.data.frame()
    
  return(res)
}

## Stratum abundance
## @export
## @description Number of individuals in a stratum based on count. \strong{Note:} Does
## not take into account detection probability.
## @inheritParams strat_density
## @return A data.frame with abundance per stratum by species, its variance (var),
## the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
## possible number of SSUs (NM), and the total possible number of PSUs (N)
strat_abundance = function(x, ntot) {
  ## Calculate stratum level density, convert to abundance, and rename
  res = strat_density(x, ntot) %>%
    dplyr::mutate(
      var = .data$var * (.data$NM^2),
      density = .data$density * .data$NM
    ) %>%
    dplyr::rename(abundance = "density") %>%
    as.data.frame()
    
  return(res)
}

## Domain Abundance
## @export
## @description Number of individuals in a sampling domain based on count. \strong{Note:} Does
## not take into account detection probability.
## @inheritParams strat_density
## @return A data.frame with abundance per sampling domain by species, its variance (var),
## the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
## possible number of SSUs (NM), and the total possible number of PSUs (N)
domain_abundance = function(x, ntot) {
  ## Calculate domain density, convert to abundance, and rename
  res = domain_density(x, ntot) %>%
    dplyr::mutate(
      var = .data$var * (.data$NM^2),
      density = .data$density * .data$NM
    ) %>%
    dplyr::rename(abundance = "density") %>%
    as.data.frame()
    
  return(res)
}
