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

