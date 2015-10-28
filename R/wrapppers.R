#' Stratum level density
#' @export
#' @description
#' Calculates stratum level density (individuals/secondary sampling unit)
#' @param x
#' An RVC data list containing three data.frames: sample_data - sample
#' counts and informations; stratum_data - information on the number
#' of possible primary sampling units per stratum (ntot); taxonomic_data -
#' taxonomic and life history information for species in the reef visual census.
#' @param species
#' A character vector containing scientific names, common names, or species codes
#' (first three letters of the genus name and first four of the species name)
#' or a combination thereof.
#' @param ...
#' Additional arguments to filter the data:
#' \itemize{
#'  \item{strata}{a character vector of strata codes}
#'  \item{status}{a numeric vector of protected statuses.}
#'  \item{is_protected}{a boolean indicating whether only protected areas should be included (TRUE),
#'  only unportected areas (FALSE), or both (NULL, the default)}
#'  \item{years}{a numeric vector of years}
#'  \item{regions}{a character vector of region codes: FLA KEYS, DRTO, SEFCRI}
#'  \item{len_geq}{a number that the length, in centimeters, should be greater than or equal to}
#'  \item{len_lt}{a number that the length, in centimeters, should be less than}
#'  \item{cnt_geq}{a number that the individual count should be greater than or equal to}
#'  \item{cnt_lt}{a number that the individual count should be less than}
#' }
#' @return A data.frame with the density for each stratum, the average variance in density (var),
#' the number of primary sampling units sampled (n), the number of secondary sampling units sampled
#' (nm), the number of possible primary sampling units (N), and the number of possible secondary sampling
#' units (NM)
getStratumDensity = function(x, species, ...) {
  ## Apply filters to data
  filtered = .wrapperProto(x, species, ...)
  ## Return stratum level density
  return(strat_density(psu_density(ssu_density(filtered$sample_data)), filtered$stratum_data))
}

#' Domain level density
#' @export
#' @description
#' Calculates samping domain level density (individuals/secondary sampling unit)
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the density for each sampling domain, the average variance in density (var),
#' the number of primary sampling units sampled (n), the number of secondary sampling units sampled
#' (nm), the number of possible primary sampling units (N), and the number of possible secondary sampling
#' units (NM)
getDomainDensity = function(x, species, ...) {
  ## Apply filters to data
  filtered = .wrapperProto(x, species, ...)
  ## Return domain level density
  return(domain_density(strat_density(psu_density(ssu_density(filtered$sample_data)), filtered$stratum_data),
                        filtered$stratum_data))
}

## Filters all data for each wrapper function
.wrapperProto = function(x, species, ...){
  ## Try to get sample, stratum, and taxonomic data from x
  sample_data = x[['sample_data']]
  stratum_data = x[['stratum_data']]
  taxonomic_data = x[['taxonomic_data']]
  ## Get species codes
  species_cd = .getSpecies_cd(species, taxonomic_data)
  ## Apply filters to sample data
  sample_data = .apply_filters(sample_data, species_cd, ...)
  ## Apply filters to stratum data
  stratum_data = strata_filter(protected_filter(stratum_data, ...), ...)
  if(hasArg("when_present") && list(...)$when_present) {
    stratum_data = subset(stratum_data, STRAT %in% unique(sample_data$STRAT))
  }
  return(list(sample_data = sample_data, stratum_data = stratum_data, taxonomic_data = taxonomic_data))
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
