#' @export
getStratumDensity = function(x, species, ...) {
  ## Filter data by species and ... arguments
  filtered = .apply_filters(x[['sample_data']], species, ...)
  ## Return stratum level density
  return(strat_density(psu_density(ssu_density(filtered)), x[['stratum_data']]))
}

#' @export
getDomainDensity = function(x, species, ...) {
  ## Filter data by species and ... arguments
  filtered = .apply_filters(x[['sample_data']], species, ...)

  return(domain_density(strat_density(psu_density(ssu_density(filtered)), x[['stratum_data']]),
                        x[['stratum_data']]))
}

.apply_filters = function(x, species, ...){
  filtered = strata_filter(
    protected_filter(
    year_filter(
    region_filter(
    length_filter(
    count_filter(
      species_filter(x, species_cd = species), ...
    ), ...
    ), ...
    ), ...
    ), ...
    ), ...
  )

  return(filtered)
}
