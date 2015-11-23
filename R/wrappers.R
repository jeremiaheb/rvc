#' Stratum level density
#' @export
#' @description
#' Calculates average density (individual/secondary sampling unit) for each
#' stratum
#' @param x
#' A list containing three data.frames:
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
#' Numeric vector, data.frame, or keyword. \itemize{
#' \item{vector: }{A number of numeric vector of lengths, in cm, of breakpoints by which the data will
#' be binned. The same vector will be applied to all species}
#' \item{data.frame: }{A lookup table containing two columns. The first column with scientific names
#'  or species codes, and the second containing lengths, in cm, by which to break up the data for each species}
#'  \item{keyword: }{"lc" - breaks the data at minimum length at capture for each species.
#'  "lm" - breaks the data at median length at maturity for each species. \strong{NOTE:} Generates
#'  length at capture and length at maturity from taxonomic data table. Can only be used if
#'  length at capture or length at maturity is available for the species in the taxonomic_data table. See
#'  \code{\link{getTaxonomicData}}}
#' }
#' @param merge_protected
#' A boolean indicating whether protected and unprotected areas should be merged (TRUE, the default),
#' or should be calculated seperately (FALSE)
#' @param ...
#' Optional filters to apply to the data:
#' \describe{
#'  \item{strata}{Character vector of strata codes to include. Strata codes vary by region}
#'  \item{status}{Numeric vector of protected statuses}
#'  \item{is_protected}{Boolean indicating whether only protected areas should be included (TRUE),
#'  only unprotected areas (FALSE), or both (NULL, the default). Must be NULL if merge_protected is FALSE}
#'  \item{years}{Numeric vector of years to include}
#'  \item{regions}{Character vector of region codes: (e.g. "FLA KEYS", "DRY TORT", "SEFCRI") to include}
#'  \item{when_present}{Boolean indicating whether to only include records where individuals present (TRUE),
#'  or not (FALSE)}
#' }
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{density}{Average density per secondary sampling unit}
#' \item{var}{Variance in average density per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @examples
#' ## Get RVC data from 2012 in the Florida Keys
#' fk2012 = getRvcData(years = 2011, regions = "FLA KEYS")
#'
#' ## Calculate stratum density for Red Grouper
#' getStratumDensity(fk2012, species = "Red Grouper")
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainDensity}}
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
#' Calculates average density (individual/secondary sampling unit) for each
#' sampling domain (year/region)
#' @inheritParams getStratumDensity
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{density}{Average density per secondary sampling unit}
#' \item{var}{Variance in average density per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumDensity}}
#' @examples
#' ## Get data for the Dry Tortugas in 2008
#' dt2008 = getRvcData(years = 2008, regions = "DRY TORT")
#'
#' ## Calculate density for Black Grouper
#' getDomainDensity(dt2008, species = "Myc bona")
#'
#' ## Calculate density for Black Grouper in and outside of protected areas
#' getDomainDensity(dt2008, species = "Black Grouper", merge_protected = FALSE)
#'
#' ## Calculate density for Black Grouper above and below 60cm
#' getDomainDensity(dt2008, species = "Mycteroperca bonaci", length_bins = 60)
getDomainDensity = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    domain_density(strat_density(psu_density(ssu_density(sample_data)), stratum_data), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainDensity, f, ...)

  return(out)
}

#' Domain level occurrence
#' @export
#' @description
#' Calculates occurrence per secondary sampling unit for each sampling
#' domain level
#' @inheritParams getDomainDensity
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{occurrence}{Average occurrence per secondary sampling unit}
#' \item{var}{Variance in average occurrence per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumOccurrence}}
#' @examples
#' ## Get RVC data for 2012 in the Florida Keys
#' fk2012 = getRvcData(years = 2012, regions = "FLA KEYS")
#'
#' ## Calculate occurrence for Bluehead Wrasse
#' getDomainOccurrence(fk2012, species = "Tha bifa")
getDomainOccurrence = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    domain_occurrence(strat_occurrence(psu_occurrence(ssu_occurrence(sample_data)), stratum_data), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainOccurrence, f, ...)

  return(out)
}

#' Stratum level occurrence
#' @export
#' @description
#' Calculates occurrence per secondary sampling unit for each stratum
#' @inheritParams getDomainDensity
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{occurrence}{Average occurrence per secondary sampling unit}
#' \item{var}{Variance in average occurrence per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainOccurrence}}
#' @examples
#' ## Get RVC data for Southeast Florida in 2013
#' sf2013 = getRvcData(years = 2013, regions = "SEFCRI")
#'
#' ## Calculate stratum occurrence of white grunt
#' getStratumOccurrence(sf2013, species = "Hae plum")
getStratumOccurrence = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    strat_occurrence(psu_occurrence(ssu_occurrence(sample_data)), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumOccurrence, f, ...)

  return(out)
}


#' Stratum level abundance
#' @export
#' @description
#' Estimates abundance (total number of individuals) for each stratum
#' @inheritParams getStratumDensity
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{abundance}{Estimated abundance per stratum}
#' \item{var}{Variance in estimated abundance}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @details
#' This estimate of abundance does not take into account detection probability. It
#' is simply the count per secondary sampling unit extrapolated for the entire sampling
#' domain. In most circumstances this will yield an underestimate of abundance.
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainAbundance}}
#' @examples
#' ## Get RVC data for the Florida Keys in 2000
#' fk2000 = getRvcData(years = 2000, regions = "FLA KEYS")
#'
#' ## Estimate Yellowtail Abundance per stratum
#' getStratumAbundance(fk2000, species = "Ocy chry")
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
#' Estimates abundance (total number of individuals) for each sampling domain
#' (year/region)
#' @inheritParams getStratumDensity
#' @details
#' This estimate of abundance does not take into account detection probability. It
#' is simply the count per secondary sampling unit extrapolated for the entire sampling
#' domain. In most circumstances this will yield an underestimate of abundance.
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{abundance}{Estimated abundance per sampling domain}
#' \item{var}{Variance in estimated abundance}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumAbundance}}
#' @examples
#' ## Get RVC data from the florida keys in 2000
#' fk2000 = getRvcData(years = 2000, regions = "FLA KEYS")
#'
#' ## Calculate yellowtail abundance
#' getDomainAbundance(fk2000, species = "Yellowtail Snapper")
#'
#' ## Calculate yellowtail abundance in/outside of protected areas
#' getDomainAbundance(fk2000, species = "Ocy chry", merge_protected = FALSE)
#'
#' ## Calculate yellowtail abundance above/below 25cm
#' getDomainAbundance(fk2000, species = "Ocyurus chrysurus", length_bins = 25)
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
#' Estimates biomass per secondary sampling unit for each stratum
#' @inheritParams getStratumDensity
#' @param growth_parameters
#' A data.frame or list of allometric growth parameters.\itemize{
#' \item{list: }{A list with variables named 'a' and 'b'
#' representing the linear and exponential coefficient of the allometric growth equation (see details)}
#' \item{data.frame: }{A data.frame with columns named a and b (or WLEN_A and WLEN_B) as well as a SPECIES_CD
#' column. If growth_parameters is NULL (default) function will attempt to use parameters from taxonomic data
#' retrieved from server}}
#' \strong{NOTE:} using a list means that only one set of growth parameters will be used for all species,
#' while passing in a data.frame will allow the function to lookup the growth_parameters for each species
##' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{biomass}{Average biomass per secondary sampling unit}
#' \item{var}{Variance in average biomass per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'   W[kg] = (a*(L[cm]*10[mm/cm])^b)/1000[kg/g]
#' }
#' The 'a' and 'b' parameters provided in growth_parameters should be for
#' converting from millimeters to grams, even though the final output is in kilograms.
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainBiomass}} \code{\link{getStratumTotalBiomass}}
#' @examples
#' ## Get Data from 2006 in the Dry Tortugas
#' dt2006 = getRvcData(years = 2006, region = "DRY TORT")
#'
#' ## Calculate biomass per ssu for Red Grouper for each stratum
#' getStratumBiomass(dt2006, species = "Epi mori", growth_parameters = list(a = 1.13e-5, b = 3.035))
getStratumBiomass = function(x, species, growth_parameters = NULL, length_bins = NULL, merge_protected = TRUE, ...) {
  ## If growth_parameters is NULL, get them from taxonomic_data
  growth_parameters = .getGrowthParameters(x, species, growth_parameters)
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
    strat_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

#' Domain level biomass per SSU
#' @export
#' @description
#' Estimates biomass per secondary sampling unit for each sampling domain
#' @inheritParams getStratumBiomass
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{biomass}{Average biomass per secondary sampling unit}
#' \item{var}{Variance in average biomass per secondary sampling unit}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'   W[kg] = (a*(L[cm]*10[mm/cm])^b)/1000[kg/g]
#' }
#' The 'a' and 'b' parameters provided in growth_parameters should be for
#' converting from millimeters to grams, even though the final output is in kilograms.
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumBiomass}} \code{\link{getDomainTotalBiomass}}
#' @examples
#' ## Get Data from 2006 in the Dry Tortugas
#' dt2006 = getRvcData(years = 2006, region = "DRY TORT")
#'
#' ## Calculate biomass per ssu for Red Grouper
#' getDomainBiomass(dt2006, species = "Epi mori", growth_parameters = list(a = 1.13e-5, b = 3.035))
getDomainBiomass = function(x, species, growth_parameters = NULL, length_bins = NULL, merge_protected = TRUE, ...){
  ## If growth_parameters is NULL, get them from taxonomic_data
  growth_parameters = .getGrowthParameters(x, species, growth_parameters)
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
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{total_biomass}{Total biomass for each sampling domain}
#' \item{var}{Variance in total biomass for each sampling domain}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'   W[kg] = (a*(L[cm]*10[mm/cm])^b)/1000[kg/g]
#' }
#' The 'a' and 'b' parameters provided in growth_parameters should be for
#' converting from millimeters to grams, even though the final output is in kilograms.
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainTotalBiomass}} \code{\link{getStratumBiomass}}
#' @examples
#' ## Get RVC data for the Florida Keys in 2012
#' fk2012 = getRvcData(years = 2012, regions = "FLA KEYS")
#'
#' ## Calculate total biomass of Yellowtail Snapper for each stratum
#' getStratumTotalBiomass(fk2012, species = "Ocy chry", growth_parameters = list(a = 7.75e-5, b = 2.718))
getStratumTotalBiomass = function(x, species, growth_parameters = NULL, length_bins = NULL, merge_protected = TRUE, ...){
  ## If growth_parameters is NULL, get them from taxonomic_data
  growth_parameters = .getGrowthParameters(x, species, growth_parameters)
  ## function to wrap
  f = function(sample_data, stratum_data, growth_parameters, ...){
   strat_total_biomass(psu_biomass(ssu_biomass(sample_data, growth_parameters)), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumTotalBiomass, f, ...,
                      growth_parameters = growth_parameters)

  return(out)
}

#' Domain level total biomass
#' @export
#' @description
#' Calculates total biomass for each sampling domain
#' @inheritParams getDomainBiomass
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{total_biomass}{Total biomass for each sampling domain}
#' \item{var}{Variance in total biomass for each sampling domain}
#' \item{n}{Number of primary sampling units sampled}
#' \item{nm}{Number of secondary sampling units sampled}
#' \item{N}{Number of possible primary sample units}
#' \item{NM}{Number of possible secondary sampling units}
#' \item{length_class}{The length class or bin. Only present if length_bins is not NULL.
#' The notation, [lower, upper), is inclusive of the lower bound, but exclusive of the upper bound}
#' \item{protected_status}{The protected status. Only present if merge_protected is FALSE}
#' }
#' @details
#' The form of the allometric growth equation used in calculating biomass is:
#' \deqn{
#'   W[kg] = (a*(L[cm]*10[mm/cm])^b)/1000[kg/g]
#' }
#' The 'a' and 'b' parameters provided in growth_parameters should be for
#' converting from millimeters to grams, even though the final output is in kilograms.
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumTotalBiomass}} \code{\link{getDomainBiomass}}
#' @examples
#' ## Get RVC data for the Florida Keys in 2012
#' fk2012 = getRvcData(years = 2012, regions = "FLA KEYS")
#'
#' ## Calculate total biomass of Yellowtail Snapper for each stratum
#' getDomainTotalBiomass(fk2012, species = "Ocy chry", growth_parameters = list(a = 7.75e-5, b = 2.718))
getDomainTotalBiomass = function(x, species, growth_parameters = NULL, length_bins = NULL, merge_protected = TRUE, ...){
  ## If growth_parameters is NULL, get them from taxonomic_data
  growth_parameters = .getGrowthParameters(x, species, growth_parameters)
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

#' Stratum level length frequency
#' @export
#' @description
#' Calculates length frequency for each stratum
#' @inheritParams getDomainDensity
#' @param length_bins
#' A numeric vector of lengths, in centimenters, by which to bin the data.
#' If NULL (default), will not bin the data.
#' \strong{Note:} Lengths below the minimum and above the maximum
#' value in length_bins will be assigned <NA> (with no differentiation between
#' below and above the provided bins). It is suggested that
#' the bins match or exceed the range of lengths for the species in which you
#' are interested
#' @return
#' A data.frame with:
#' \describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{STRAT}{A code for the stratum}
#' \item{PROT}{A boolean indicating protected status: 1 - Protected, 2 - Unprotected}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{length_class}{The actual measured lengths, in cm, if length_bins = NULL, or an interval, if length_bins
#' is not NULL}
#'\item{frequency}{The relative frequency of each length class within each stratum}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getDomainLengthFrequency}}
#' @examples
#' ## Get rvc data from the Florida Keys in 2005
#' fk2005 = getRvcData(years = 2005, regions = "FLA KEYS")
#'
#' ## Calculate stratum length frequencies for Mangrove Snapper
#' getStratumLengthFrequency(fk2005, species = "LUT GRIS")
#'
#' ## Calculate length frequencies for mangrove snappers in
#' ## 1cm bins
#' getStratumLengthFrequency(fk2005, species = "LUT GRIS", length_bins = seq(0,100,1))
getStratumLengthFrequency = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Function to wrap
  f = function(sample_data, stratum_data, ...) {
    ## Handle length bins here instead of in wrapperProto
    if(!is.null(length_bins)){
      sample_data$LEN = cut(sample_data$LEN, length_bins, right = FALSE)
    }
    strat_length_frequency(sample_data, stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumLengthFrequency, f, ...)

  return(out)
}

#' Domain level length frequency
#' @export
#' @description
#' Calculates length frequency for each sampling domain
#' @inheritParams getDomainDensity
#' @param length_bins
#' A numeric vector of lengths, in centimenters, by which to bin the data.
#' If NULL (default), will not bin the data.
#' \strong{Note:} Lengths below the minimum and above the maximum
#' value in length_bins will be assigned <NA> (with no differentiation between
#' below and above the provided bins). It is suggested that
#' the bins match or exceed the range of lengths for the species in which you
#' are interested
#' @return
#' A data.frame with:
#' \describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name}
#' \item{length_class}{The actual measured lengths, in cm, if length_bins = NULL, or an interval, if length_bins
#' is not NULL}
#'\item{frequency}{The relative frequency of each length class within each sampling domain}
#' }
#' @seealso \code{\link{getRvcData}} \code{\link{getStratumLengthFrequency}}
#' @examples
#' ## Get rvc data from the Florida Keys in 2005
#' fk2005 = getRvcData(years = 2005, regions = "FLA KEYS")
#'
#' ## Calculate stratum length frequencies for Mangrove Snapper
#' getDomainLengthFrequency(fk2005, species = "LUT GRIS")
#'
#' ## Calculate length frequencies for mangrove snappers in
#' ## 1cm bins
#' getDomainLengthFrequency(fk2005, species = "LUT GRIS", length_bins = seq(0,100,1))
getDomainLengthFrequency = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Function to wrap
  f = function(sample_data, stratum_data, ...) {
    ## Handle length bins here instead of in wrapperProto
    if(!is.null(length_bins)){
      sample_data$LEN = cut(sample_data$LEN, length_bins, right = FALSE)
    }
    domain_length_frequency(strat_length_frequency(sample_data, stratum_data), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainLengthFrequency, f, ...)

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
    if(is.null(length_bins) | identical(wrapper, getStratumLengthFrequency) |
       identical(wrapper, getDomainLengthFrequency)){
      ## Apply filters to sample data
      sample_data = .apply_sample_filters(sample_data, species_cd, ...)
      ## Apply filters to stratum data
      stratum_data = .apply_stratum_filters(stratum_data, sample_data, ...)

      out = fun(sample_data, stratum_data, ...)
    }
    ## Recursive Case: Lenth bins present
    else {
      out = .funByLen(x, species_cd, length_bins, wrapper, ...)
    }
  ## Recursive Case: merge_protected is FALSE
  } else {
    out = .funByProt(x, species_cd, length_bins, wrapper, ...)
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
  ## If length_bins is NA, it will break the subsequent checks
  ## so need to reset as 0 and print warning message
  if(all(is.na(length_bins))){
    length_bins = 0
    warning(paste("could not find breakpoints for species",species))
  }
  ## If length_bins is "lc" use lookup from taxonomic_data
  if(all(toupper(length_bins) == "LC")){
    length_bins = x$taxonomic_data[c("SPECIES_CD", "LC")]
  }
  ## If length_bins is "lm" use lookup from taxonomic_data
  if(all(toupper(length_bins) == "LM")){
    length_bins = x$taxonomic_data[c("SPECIES_CD", "LM")]
  }
  ## If length_bins is a data.frame run .funByProt
  ## for each species
  if(is.data.frame(length_bins)){
    ## Get species codes for species in length_bins column 1
    length_bins[,1] = .getSpecies_cd(length_bins[,1], x$taxonomic_data)
    ## Check that all species in SPECIES_CD
    if(!all(species %in% length_bins[,1])){
      missing = species[!(species %in% length_bins[,1])]
      stop(paste('could not find species', paste(missing, collapse = ", "), "in the first column of length_bins"))
    }
    ## For each species in species, fun .funByProt with just that species
    ## and its lookup
    l = list()
    for (i in seq_along(species)){
      l[[i]] = .funByLen(x, species[i], length_bins[length_bins[,1] == species[i],2], cb, ...)
    }
  } else {
    ## Make an empty list
    n = length(length_bins)
    l = list();
    ## callback value below lowest bin value
    l[[1]] = cb(x, species, len_lt = length_bins[1], ...)
    l[[1]]$length_class = rep(paste("<", length_bins[1]), nrow(l[[1]]))
    ## Calculate between bin values
    if (n > 1){
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
  }

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

## Helper function that pull growth parameters from taxonomic data table
## raises error if not all are found
## @param x List returned by getRvcData
## @param species list of species scientific/common names or species codes
## @param growth_parameters A list or data.frame of growth parameters,
## if NULL function uses taxonomic data, else passes on parameters
.getGrowthParameters = function(x, species, growth_parameters){
  if(is.null(growth_parameters)){
    message('no growth parameters given. Using growth parameters from taxonomic data (?getTaxonomicData)')
    growth_parameters = x$taxonomic_data
  }
  return(growth_parameters)
}
