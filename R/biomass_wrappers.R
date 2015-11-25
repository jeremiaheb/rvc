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
  ## Check species names
  x = .checkSpeciesMatch(x, species, growth_parameters)
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
  ## Check species names
  x = .checkSpeciesMatch(x, species, growth_parameters)
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
  ## Check species names
  x = .checkSpeciesMatch(x, species, growth_parameters)
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
  ## Check species names
  x = .checkSpeciesMatch(x, species, growth_parameters)
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
