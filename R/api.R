## API connections

#' Download sample data from server
#' @export
#' @description
#' Download sample data from server and save as a data.frame
#' @param years
#' A numeric vector of years for which to get sample data
#' @param regions
#' A character vector of region codes for which to get sample data (e.g. "FLA KEYS",
#' "DRY TORT", "SEFCRI")
#' @param server
#' A string containing the domain url at which to access the server
#' @return
#' A data.frame of analysis ready data and the following columns:
#' \describe{
#' \item{PRIMARY_SAMPLE_UNIT}{A code indicating the primary sample unit in which a sample was collected}
#' \item{YEAR}{A number indicating the calendar year}
#' \item{MONTH}{A number indicating the month of the year}
#' \item{DAY}{A number indicating the day of the month (EST)}
#' \item{STATION_NR}{A number indicating the secondary sampling unit within a given primary sample unit}
#' \item{LAT_DEGREES}{Latitude of secondary sampling unit in decimal degrees}
#' \item{LON_DEGREES}{Longitude of secondary sampling unit in decimal degrees}
#' \item{DEPTH}{Average depth, in meters, of secondary sampling unit}
#' \item{UNDERWATER_VISIBILITY}{Visibility, in meters, at secondary sampling unit}
#' \item{MAPGRID_NR}{A number indicating the primary sample unit}
#' \item{HABITAT_CD}{A code indicating the habitat type}
#' \item{ZONE_NR}{A code indicating the distance offshore: 1 - Inshore, 2 - Midchannel, 3 - Offshore, 4 - Fore-reef}
#' \item{SUBREGION_NR}{A number indicating the subregion}
#' \item{MPA_NR}{A number identifying the marine protected area in which the sample was collected. Zero indicates unprotected status}
#' \item{SPECIES_NR}{A number indicating the species for a sample}
#' \item{SPECIES_CD}{A code indicating the species for a sample. Consists of the first three letters of the generic name and the first four of the specific name}
#' \item{LEN}{The length, in cm, of a sample}
#' \item{NUM}{The number of individuals of a given species and length observed in a secondary sampling unit}
#' \item{TIME_SEEN}{A number indicating when, during sampling, an individual was observed. 1: In the first five minutes, 2: From 5-10 minutes, 3: After 10 minutes}
#' \item{PROT}{A boolean value indicating whether a sample was in a protected area (1), or not (0)}
#' \item{STRAT}{A code indicating the stratum in which a sample was taken. Differs by region}
#' \item{REGION}{A code indicating the region in which a sample was taken. DRY TORT: Dry Tortugas, FLA KEYS: Florida Keys, and SEFCRI: Southeast Peninsular Florida}
#' }
#' @seealso \code{\link{getStratumData}} \code{\link{getTaxonomicData}} \code{\link{getRvcData}}
getSampleData = function(years, regions, server = 'http://www.sefsc.noaa.gov/rvc_analysis20/') {
  message('downloading sample data ...')
  return(.getData(years, regions, server, '/samples/index.zip?', TRUE, FALSE))
}

#' Download stratum data from server
#' @export
#' @description
#' Download stratum data from server and save as data.frame
#' @inheritParams getSampleData
#' @return
#' A data.frame with stratum data, including thr columns:
#' \describe{
#' \item{REGION}{A code indicating the region. DRY TORT: Dry Tortugas, FLA KEYS: Florida Keys, and SEFCRI: Southeast Peninsular Florida}
#' \item{YEAR}{A number indicating the calendar year}
#' \item{PROT}{A boolean indicating the protected status: 1 - protected, 2 - unprotected}
#' \item{STRAT}{A code indicating the stratum. Differs by region}
#' \item{NTOT}{The number of possible primary sample units for a given year, region, stratum, and protected status}
#' \item{GRID_SIZE}{The length (in meters) to a side of a primary sample unit for a given year, region, stratum, and protected status}
#' }
#' @seealso \code{\link{getSampleData}} \code{\link{getTaxonomicData}} \code{\link{getRvcData}}
getStratumData = function(years, regions, server = 'http://www.sefsc.noaa.gov/rvc_analysis20/') {
  message('downloading stratum data ...')
  return(.getData(years, regions, server, '/strata/index.csv?', FALSE, TRUE))
}

#' Downloads RVC data and saves to a list
#' @export
#' @description Dowloads sample, stratum, and taxonomic/life-history data for
#' provided years and regions, and saves them to a list which can be used in
#' summary statistic procuding functions in the package
#' @inheritParams getSampleData
#' @return A list containing three data frames: sample_data - containing
#' sample information, stratum_data - containing stratum information,
#' taxonomic_data - containing taxonomic and life history information
#' @details To understand more about the meaning of the different variables
#' in the returned list please see the documentation for
#' \code{\link{getSampleData}}, \code{\link{getStratumData}},
#' \code{\link{getTaxonomicData}}
#' @seealso \code{\link{getStratumData}} \code{\link{getTaxonomicData}} \code{\link{getSampleData}}
getRvcData = function(years, regions, server = 'http://www.sefsc.noaa.gov/rvc_analysis20/'){
  taxonomic_data = getTaxonomicData(server)
  stratum_data = getStratumData(years, regions, server)
  sample_data = getSampleData(years, regions, server)

  return(list(sample_data = sample_data, stratum_data = stratum_data, taxonomic_data = taxonomic_data))
}

#' Download taxonomic data from server
#' @export
#' @description
#' Download taxonomic and life history data from server
#' @inheritParams getSampleData
#' @return
#' A data.frame with taxonomic data and life history data for
#' all species in the RVC with columns:
#' \describe{
#' \item{SPECIES_CD}{The first three letters of the generic name and first four of the specific name}
#' \item{SCINAME}{The scientific name}
#' \item{COMNAME}{The common name}
#' \item{LC}{Minimum length at capture, in centimeters}
#' \item{LM}{Median length at maturity, in centimeters}
#' \item{WLEN_A}{The linear coefficient of the allometric growth equation in grams per millimeter (g/mm)}
#' \item{WLEN_B}{The exponential coefficient of the allometric growth equation}
#' }
#' @seealso \code{\link{getStratumData}} \code{\link{getSampleData}} \code{\link{getRvcData}}
getTaxonomicData = function(server = 'http://www.sefsc.noaa.gov/rvc_analysis20/') {
  message('downloading taxonomic data')
  ## Test that server can be accessed
  if(!RCurl::url.exists(server))stop("could not access server")
  ## The url to get the taxonomic data
  u = paste(server, '/taxa/index.csv', sep = "")
  return(.download_csv(u, FALSE))
}

#' Download benthic data from server
#' @export
#' @description
#' Download benthic data from server to a data.frame
#' @inheritParams getSampleData
#' @return
#' A data.frame with taxonomic data and life history data for
#' all species in the RVC with columns:
#' \describe{
#' \item{REGION}{A code indicating the region. DRTO: Dry Tortugas, FLA KEYS: Florida Keys, and SEFCRI: Southeast Peninsular Florida}
#' \item{YEAR}{A number indicating the calendar year}
#' \item{PRIMARY_SAMPLE_UNIT}{A code indicating the primary sample unit in which a sample was collected}
#' \item{STATION_NR}{A number indicating the secondary sampling unit within a given primary sample unit}
#' \item{DEPTH}{Average depth, in meters, of secondary sampling unit}
#' \item{MAX_HARD_RELIEF}{The maximum height, in meters, of hard relief (e.g. hard corals, rock)}
#' \item{MAX_SOFT_RELIEF}{The maximum height, in meters, of soft relief (e.g. soft corals, sponges)}
#' \item{AVG_HARD_RELIEF}{The average height, in meters, of hard relief}
#' \item{HARD_REL_PCT_0}{Percentage of hard relief less than 0.2 meters}
#' \item{HARD_REL_PCT_1}{Percentage of hard relief between 0.2 and 0.5 meters}
#' \item{HARD_REL_PCT_2}{Percentage of hard relief between 0.5 and 1.0 meters}
#' \item{HARD_REL_PCT_3}{Percentage of hard relief between 1.0 and 1.5 meters}
#' \item{HARD_REL_PCT_4}{Percentage of hard relief greater than 1.5 meters}
#' \item{PCT_SAND}{Percentage of abiotic cover that is sand}
#' \item{PCT_HARD_BOTTOM}{Percentage of abiotic cover that is hard bottom}
#' \item{PCT_RUBBLE}{Percentage of abiotic cover that is rubble}
#' \item{PCT_CORAL}{Percentage of biotic hardbottom that is coral}
#' \item{PCT_OCTO}{Percentage of biotic hardbottom that is octocoral}
#' \item{PCT_SPONGE}{Percentage of biotic hardbottom that is sponge}
#' }
getBenthicData = function(years, regions, server = 'http://www.sefsc.noaa.gov/rvc_analysis20/') {
  message('downloading benthic data ...')
  return(.getData(years, regions, server, '/benthic/index.csv?', FALSE, TRUE))
}

#' Download, unzip, and write a file to a csv
#' @param u
#' A string. The URL at which to find the file
#' @param zipped
#' A boolean indiating whether a file is zipped file
#' or not
.download_csv = function(u, zipped) {
  ## Read data to a temporary file
  temp = tempfile()
  suppressWarnings(download.file(u, temp, quiet = TRUE, mode = 'wb'))
  ## Check that file has content
  if(file.info(temp)$size<1){stop("there was an error downloading the data")}
  ## If zipped, unzip and read; otherwise, read directly as csv
  if(zipped) {
    out = read.csv(unzip(temp, exdir = tempdir()))
  } else {
    out = read.csv(temp)
  }
  message(paste("downloaded file from", u))
  return(out)
}

#' A prototype for downloading csv files
#' @param years
#' A numeric vector of years for which to get sample data
#' @param regions
#' A character vector of region codes for which to get sample data (e.g. "FLA KEYS",
#' "DRY TORT", "SEFCRI")
#' @param server
#' A string containing the domain url at which to access the server
#' @param path
#' A string of the relative path to this element of the api
#' @param zipped
#' A boolean indiating whether a file is zipped file
#' or not
#' @param quiet
#' A boolean indicating whether messages should be printed or not
.getData = function(years, regions, server, path, zipped, quiet) {
  ## Test that server can be accessed
  if(!RCurl::url.exists(server))stop("could not access server")
  ## Create url for each query, escape spaces and other URI unsafe characters
  combined = cbind(years = years, regions = rep(regions, each = length(years)))
  queries = unlist(lapply(paste(server, path, 'year=', combined[,1], '&region=', combined[,2], sep=""), URLencode))
  ## Figure out which queries are valid
  keep = unlist(lapply(queries, RCurl::url.exists))
  valid_queries = queries[keep]
  ## If no queries are valid, return error
  if(length(valid_queries) == 0){
    msg = paste("The following combinations of region/year are not available:",
                paste(combined[!keep,2], "/", combined[!keep,1], collapse = ", ", sep = ""))
    stop(msg)
  }
  ## Download data and store into a list
  data = lapply(valid_queries, .download_csv, zipped)
  if(!quiet & sum(keep) != nrow(combined)){
    msg = paste("The following combinations of region/year are not available:",
                paste(combined[!keep,2], "/", combined[!keep,1], collapse = ", ", sep = ""))
    warning(msg)
  }
  ## Return merged data
  return(plyr::rbind.fill(data))
}
