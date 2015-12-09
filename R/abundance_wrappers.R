
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
#' of the species name. If group is passed as an argument, SPECIES_CD will be changed to GROUP}
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
    strat_abundance(psu_density(species_group(ssu_density(sample_data), ...)), stratum_data)
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
#' of the species name. If group is passed as an argument, SPECIES_CD will be changed to GROUP}
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
    domain_abundance(strat_density(psu_density(species_group(ssu_density(sample_data), ...)), stratum_data), stratum_data)
  }
  ## Wrap function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainAbundance, f, ...)

  return(out)
}
