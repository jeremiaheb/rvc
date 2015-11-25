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
