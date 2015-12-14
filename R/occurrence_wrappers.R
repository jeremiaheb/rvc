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
#' of the species name. If group is passed as an argument, SPECIES_CD will be changed to GROUP}
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
    domain_occurrence(strat_occurrence(psu_occurrence(species_group(ssu_occurrence(sample_data), ...)), stratum_data), stratum_data)
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
#' of the species name. If group is passed as an argument, SPECIES_CD will be changed to GROUP}
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
    strat_occurrence(psu_occurrence(species_group(ssu_occurrence(sample_data), ...)), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getStratumOccurrence, f, ...)

  return(out)
}
