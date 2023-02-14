#' Domain level Mean Length (Lbar)
#' @export
#' @description
#' Calculates mean length (cm)
#' sampling domain (year/region)
#' @inheritParams getStratumDensity
#' @return A data.frame with:
#'\describe{
#' \item{YEAR}{The year}
#' \item{REGION}{A code for the region: DRY TORT - Dry Tortugas, SEFCRI - Southeast Peninsular Florida,
#' FLA KEYS - Florida Keys}
#' \item{SPECIES_CD}{The species code. The first three letters of the genus name and first four
#' of the species name. If group is passed as an argument, SPECIES_CD will be changed to GROUP}
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

getDomainLbar = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    domain_lbar(strat_lbar(psu_lbar(species_group(ssu_lbar(sample_data), ...)), stratum_data), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainLbar, f, ...)

  return(out)
}
