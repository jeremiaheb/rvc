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
#' @param merge_protected
#' A boolean indicating whether protected and unprotected areas should be merged (TRUE, the default),
#' or should be calculated seperately (FALSE)
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
getStratumDensity = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## The function that computes stratumDensity given appropriately filered
  ## sample and stratum data
  f = function(sample, ntot, ...){
    strat_density(psu_density(ssu_density(sample)), ntot)
  }
  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumDensity, f, ...)

  return(out)
}

#' Domain level density
#' @export
#' @description
#' Calculates samping domain level density (individuals/secondary sampling unit)
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the density for each sampling domain.
getDomainDensity = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    domain_density(strat_density(psu_density(ssu_density(sample_data)), stratum_data), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainDensity, f, ...)

  return(out)
}

#' Stratum level abundance
#' @export
#' @description
#' Calculates stratum level abundance
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the abundance for each stratum
getStratumAbundance = function(x, species, length_bins = NULL, merge_protected = TRUE, ...){
  ## Function to compute stratumAbundance
  f = function(sample_data, stratum_data, ...){
    strat_abundance(psu_density(ssu_density(sample_data)), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumAbundance, f, ...)

  return(out)
}

#' Domain level abundance
#' @export
#' @description
#' Calculates sampling domain level abundance
#' @inheritParams getStratumDensity
#' @return
#' A data.frame with the abundance for each sampling domain
getDomainAbundance = function(x, species, length_bins = NULL, merge_protected = TRUE, ...){
  ## Function to wrap
  f = function(sample_data, stratum_data, ...){
    domain_abundance(strat_density(psu_density(ssu_density(sample_data)), stratum_data), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainAbundance, f, ...)

  return(out)
}

#' Stratum level biomass per SSU
#' @export
#' @description
#' Calculates stratum level biomass per secondary sampling unit
#' @inheritParams getStratumDensity
#' @param growth_parameters
#' List of allometric growth parameters: a - the linear coeeficient in kg/cm, b - the
#' exponential coefficient. \strong{Note:} The same growth parameters will be used for all
#' species. This will hopefully be changed in future versions.
#' @return
#' A data.frame with biomass per secondary sampling unit for each stratum
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'  W(kg) = a(kg/cm)L(cm)^b
#' }
getStratumBiomass = function(x, species, growth_parameters, length_bins = NULL, merge_protected = TRUE, ...) {
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
    strat_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

#' Sampling domain biomass per SSU
#' @export
#' @description
#' Calculates sampling domain level biomass per secondary sampling unit
#' @inheritParams getStratumBiomass
#' @return
#' A data.frame with the biomass per secondary sampling unit for each sampling domain
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'  W(kg) = a(kg/cm)L(cm)^b
#' }
getDomainBiomass = function(x, species, growth_parameters, length_bins = NULL, merge_protected = TRUE, ...){
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
    domain_biomass(strat_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

#' Stratum level total biomass
#' @export
#' @description
#' Calculates total biomass for each stratum
#' @inheritParams getDomainBiomass
#' @return
#' A data.frame with total biomass per stratum
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'  W(kg) = a(kg/cm)L(cm)^b
#' }
getStratumTotalBiomass = function(x, species, growth_parameters, length_bins = NULL, merge_protected = TRUE, ...){
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
   strat_total_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumTotalBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

#' Sampling domain level total biomass
#' @export
#' @description
#' Calculates total biomass for each sampling domain
#' @inheritParams getDomainBiomass
#' @return
#' A data.frame with total biomass per sampling domain
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'  W(kg) = a(kg/cm)L(cm)^b
#' }
getDomainTotalBiomass = function(x, species, growth_parameters, length_bins = NULL, merge_protected = TRUE, ...){
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
    domain_total_biomass(
      strat_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data),
      stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainTotalBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

###############################################################################
############################# Helper Functions ################################
###############################################################################


## A helper function to handle all of the filtering and calculation but taking different callbacks
## depending on the level and statistic
## x: a list of RVC data
## species: common/scientific/species_cd
## length_bins: numeric vector of length bins
## wrapper: a symbol, the name of the calling wrapper function
## fun: a function computing the statistic that the wrapper wraps
## ... : optional arguments to the various filters
.wrapperProto = function(x, species, length_bins, merge_protected, wrapper, fun, ...){
  ###################################################################
  ##################### Get the data ################################
  ###################################################################

  ## Try to get sample, stratum, and taxonomic data from x
    sample_data = x[['sample_data']]
    stratum_data = x[['stratum_data']]
    taxonomic_data = x[['taxonomic_data']]

    if(is.null(sample_data) | is.null(stratum_data) | is.null(taxonomic_data)){
      msg = 'could not find all of the data.frames -- sample_data, stratum_data, and taxonomic_data -- in object x'
      stop(msg)
    }

  #######################################################################
  #Get species codes, if no species found raise error. If mismatch warn #
  #######################################################################

  species_cd = .getSpecies_cd(species, taxonomic_data)

  if(length(species_cd) < 1){
    sl = paste(species, collpase = ", ")
    msg = paste('no species found with name(s):', sl)
    stop(msg)}
  else if (length(species_cd) != length(species)) {
    msg = paste('length mismatch between species provided and species found.',
                '\n', length(species), ' species provided ', length(species_cd),
                ' species found.', sep="")
    warning(msg)
  }

  ##########################################################################
  #################### Calculate Statistics ################################
  ##########################################################################

  ## Base Case: merge_protected
  if(merge_protected){
    ## Base Case: No length bins
    if(is.null(length_bins)){
      ## Apply filters to sample data
      sample_data = .apply_sample_filters(sample_data, species_cd, ...)
      ## Apply filters to stratum data
      stratum_data = .apply_stratum_filters(stratum_data, sample_data, ...)

      out = fun(sample_data, stratum_data, ...)
    }
    ## Recursive Case: Lenth bins present
    else {
      out = .funByLen(x, species, length_bins, wrapper, ...)
    }
  ## Recursive Case: merge_protected is FALSE
  } else {
    out = .funByProt(x, species, length_bins, wrapper, ...)
  }

  ## Return statistic
  return(out)
}

## Applies filters to sample data
## x: unfiltered sample_data
## species: species codes by which to filter the data
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

## Applies filter to stratum data
## stratum_data: unfiltered stratum data
## sample_data: filtered sample data
.apply_stratum_filters = function(stratum_data, sample_data, ...){
  ## Apply filters to stratum data
  stratum_data = strata_filter(protected_filter(stratum_data, ...), ...)
  if(hasArg("when_present") && list(...)$when_present) {
    ## Filter out strata in stratum_data that are not present in sample_data
    stratum_data = .with_strata(stratum_data, sample_data)
  }
  return(stratum_data)
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
  l[[1]]$length_class = rep(paste("<", length_bins[1]), nrow(l[[1]]))
  ## Calculate between bin values
  if (n > 1){
    ## TODO: Implement with apply function instead of for loop
    for(i in 1:(n-1)){
      l[[i+1]] = cb(x, species, len_geq = length_bins[i], len_lt = length_bins[i+1], ...)
      l[[i+1]]$length_class = rep(paste('[', length_bins[i], ', ', length_bins[i+1], ')', sep = ""), nrow(l[[i+1]]))
    }
  }
  ## callback to above highest bin value
  l[[n+1]] = cb(x, species, len_geq = length_bins[n], ...)
  l[[n+1]]$length_class = rep(paste(">=", length_bins[n]), nrow(l[[n+1]]))
  ## callback to all combined
  l[[n+2]] = cb(x, species, ...)
  l[[n+2]]$length_class = rep("all", nrow(l[[n+2]]))

  return(do.call(rbind, l))
}

## Calls the callback for each protected status
## @inheritParams stratumDensity
## @param cb
## A callback to one of the wrapper functions (e.g. stratumDensity, domainAbundance)
## @length_bins
## A numeric vector of lengths with which to bin the data
## @param ...
## Optional arguments passed to the callback
.funByProt = function(x, species, length_bins, cb, ...) {
  ## An empty list to hold the different cases
  l = list()
  ## Case 1: Protected only
  l[[1]] = cb(x, species, length_bins, is_protected = TRUE, ...)
  l[[1]]$protected_status = rep("protected", nrow(l[[1]]))
  ## Case 2: Unprotected only
  l[[2]] = cb(x, species, length_bins, is_protected = FALSE, ...)
  l[[2]]$protected_status = rep("unprotected", nrow(l[[2]]))
  ## Case 3: Combined
  l[[3]] = cb(x, species, length_bins, ...)
  l[[3]]$protected_status = rep("all", nrow(l[[3]]))

  ## Rbind and return
  return(do.call(rbind, l))
}

