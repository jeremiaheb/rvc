#' @export
getStratumDensity = function(x, species, ntot, ...){
  ## Filter data by species and ... arguments
  filtered = .apply_filters(x, species, ...)
  ## Return stratum level density
  return(strat_density(psu_density(ssu_density(filtered)), ntot))
}

.apply_filters = function(x, species, ...){
  filtered = strata_filter(
    protected_filter(
    year_filter(
    region_filter(
    length_filter(
    count_filter(
      species_filter(x, species_cd = species)
    ), ...
    ), ...
    ), ...
    ), ...
    ), ...
  )

  return(filtered)
}
