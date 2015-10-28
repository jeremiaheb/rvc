#' Stratum level density
#' @export
#' @description
#' Calculates stratum level density (individuals/secondary sampling unit)
#' @param x
#' An list containing three data.frames:
#' \itemize{
#'  \item{sample_data: }{sample
#'  counts and information}
#'  \item{stratum_data: }{strata information. Including the number
#'  of possible primary sampling units per stratum (NTOT)}
#'  \item{taxonomic_data: }{taxonomic and life history information}
#' }
#' @param species
#' A character vector containing scientific names, common names, or species codes for the desired species,
#' or a combination thereof
#' @param length_bins
#' A numeric vector of lengths, in centimenters, by which to bin the data. If NULL (default), will not bin the data
#' @param ...
#' Optional filters to apply to the data:
#' \describe{
#'  \item{strata}{Character vector of strata codes}
#'  \item{status}{Numeric vector of protected statuses.}
#'  \item{is_protected}{Boolean indicating whether only protected areas should be included (TRUE),
#'  only unportected areas (FALSE), or both (NULL, the default)}
#'  \item{years}{Numeric vector of years}
#'  \item{regions}{Character vector of region codes: (e.g. "FLA KEYS", "DRTO", "SEFCRI")}
#'  \item{when_present}{Boolean indicating whether to only include records where individuals present (TRUE),
#'  or not (FALSE). \strong{NOTE:} Using this option with multiple species will result in including samples
#'  where any of the species are present}
#' }
#' @return A data.frame with density for each stratum.
getStratumDensity = function(x, species, length_bins = NULL, ...) {
  ## Base Case: no length bins
  if(is.null(length_bins)){
    ## Apply filters to data
    filtered = .wrapperProto(x, species, ...)
    ## Calculate stratum level density
    out = strat_density(psu_density(ssu_density(filtered$sample_data)), filtered$stratum_data)
  }
  ## Recusrive Case: Yes, length bins
  else {
    out = .funByLen(x, species, length_bins, getStratumDensity, ...)
  }
  ## Return stratum level density
  return(out)
}

#' Domain level density
#' @export
#' @description
#' Calculates samping domain level density (individuals/secondary sampling unit)
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the density for each sampling domain.
getDomainDensity = function(x, species, length_bins = NULL, ...) {
  ## Base Case: No length bins
  if(is.null(length_bins)){
    ## Apply filters to data
    filtered = .wrapperProto(x, species, ...)
    out = domain_density(strat_density(psu_density(ssu_density(filtered$sample_data)), filtered$stratum_data),
                         filtered$stratum_data)
  }
  ## Recursive Case: Lenth bins present
  else {
    out = .funByLen(x, species, length_bins, getDomainDensity, ...)
  }
  ## Return domain level density
  return(out)
}

#' Stratum level abundance
#' @export
#' @description
#' Calculates stratum level abundance
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the abundance for each stratum
getStratumAbundance = function(x, species, length_bins = NULL, ...){
  ## Base Case: No length bins
  if(is.null(length_bins)){
    ## Apply filters to data
    filtered = .wrapperProto(x, species, ...)
    out = strat_abundance(psu_density(ssu_density(filtered$sample_data)), filtered$stratum_data)
  }
  ## Recursive Case: Lenth bins present
  else {
    out = .funByLen(x, species, length_bins, getStratumAbundance, ...)
  }
  ## Return domain level density
  return(out)
}

## A wrapper function to handle all of the filtering and calculation but taking different callbacks
## depending on the level and statistic
## x: a list of RVC data
## species: common/scientific/species_cd
## length_bins: numeric vector of length bins
## wrapper: a symbol of the wrapper function
## fun: a callback to a low-level function which the wrapper should
## call
## ... : optional arguments to filter the data
.wrapperProto = function(x, species, ...){
  ## Try to get sample, stratum, and taxonomic data from x
  sample_data = x[['sample_data']]
  stratum_data = x[['stratum_data']]
  taxonomic_data = x[['taxonomic_data']]
  ## Get species codes
  species_cd = .getSpecies_cd(species, taxonomic_data)
  ## Apply filters to sample data
  sample_data = .apply_sample_filters(sample_data, species_cd, ...)
  ## Apply filters to stratum data
  stratum_data = .apply_stratum_filters(stratum_data, sample_data, ...)
  return(list(sample_data = sample_data, stratum_data = stratum_data, taxonomic_data = taxonomic_data))
}

## Applies all filters to sample data
.apply_sample_filters = function(x, species, ...){
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

## apply filters to stratum data
.apply_stratum_filters = function(stratum_data, sample_data, ...){
  ## Apply filters to stratum data
  stratum_data = strata_filter(protected_filter(stratum_data, ...), ...)
  if(hasArg("when_present") && list(...)$when_present) {
    stratum_data = subset(stratum_data, STRAT %in% unique(sample_data$STRAT))
  }
  return(stratum_data)
}

##
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

## Calls the callback function multiple times on each length increment
## @inheritParams getStratumDensity
## @param cb
## A callback to one of the wrapper functions, e.g. getStratumDensity, getDomainDensity
## @param ...
## Optional parameters passed to the callback
.funByLen = function(x, species, length_bins, cb, ...) {
  ## Make an empty list
  n = length(length_bins)
  l = list();
  ## callback value below lowest bin value
  l[[1]] = cb(x, species, len_lt = length_bins[1], ...)
  l[[1]]$LEN = rep(paste("<", length_bins[1]), nrow(l[[1]]))
  ## Calculate between bin values
  if (n > 1){
    ## TODO: Implement with apply function instead of for loop
    for(i in 1:(n-1)){
      l[[i+1]] = cb(x, species, len_geq = length_bins[i], len_lt = length_bins[i+1], ...)
      l[[i+1]]$LEN = rep(paste('[', length_bins[i], ', ', length_bins[i+1], ')', sep = ""), nrow(l[[i+1]]))
    }
  }
  ## callback to above highest bin value
  l[[n+1]] = cb(x, species, len_geq = length_bins[n], ...)
  l[[n+1]]$LEN = rep(paste(">=", length_bins[n]), nrow(l[[n+1]]))
  ## callback to all combined
  l[[n+2]] = cb(x, species, ...)
  l[[n+2]]$LEN = rep("all", nrow(l[[n+2]]))

  return(do.call(rbind, l))
}

