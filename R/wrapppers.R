#' @export
getStratumDensity = function(x, species, ...) {
  ## Get species codes from common/scientific names or codes
  species_cd = .getSpecies_cd(species, x$taxonomic_data)
  ## Filter data by species and ... arguments
  filtered = .apply_filters(x[['sample_data']], species_cd, ...)
  ## Return stratum level density
  return(strat_density(psu_density(ssu_density(filtered)), x[['stratum_data']]))
}

#' @export
getDomainDensity = function(x, species, ...) {
  ## Get species codes from common/scientific names or codes
  species_cd = .getSpecies_cd(species, x$taxonomic_data)
  ## Apply filters to stratum to get appropriate weighting
  filtered_strat = strata_filter(protected_filter(x[['stratum_data']], ...), ...)
  ## Filter data by species and ... arguments
  filtered = .apply_filters(x[['sample_data']], species_cd, ...)
  ## If when_present is true, only include stata where species observed
  if(hasArg("when_present") && list(...)$when_present){
    filtered_strat = subset(filtered_strat, STRAT %in% unique(filtered$STRAT))
  }
  return(domain_density(strat_density(psu_density(ssu_density(filtered)), x[['stratum_data']]),
                        filtered_strat))
}

## Applies all filters to
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

## Get the species code of a species given its
## scientific/common name or species codes
## common names and species codes are not
## case sensitive
.getSpecies_cd = function(species, taxonomic_data) {
  ## get species codes from taxonomic_data
  species_cd = with(taxonomic_data, SPECIES_CD[SPECIES_CD %in% toupper(species) |
                                                 SCINAME %in% species |
                                                 toupper(COMNAME) %in% toupper(species)])
  return(species_cd)
}
