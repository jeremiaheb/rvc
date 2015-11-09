## API connections

#' Download sample data from server
#' @export
#' @description
#' Download sample data from server and save as a data.frame
#' @param years
#' A numeric vector of years for which to get sample data
#' @param regions
#' A character vector of region codes for which to get sample data (e.g. "FLA KEYS",
#' "DRTO", "SEFCRI")
#' @param server
#' A string containing the domain url at which to access the server
#' @return
#' A data.frame with sample data for years and regions provided
getSampleData = function(years, regions, server = "http://localhost:3000") {
  message('dowloading sample data ...')
  return(.getData(years, regions, server, '/samples/index.zip?', TRUE, FALSE))
}

#' Download stratum data from server
#' @export
#' @description
#' Download stratum data from server and save as data.frame
#' @inheritParams getSampleData
#' @return
#' A data.frame with stratum data for years and regions provided
getStratumData = function(years, regions, server = 'http://localhost:3000') {
  message('downloading stratum data ...')
  return(.getData(years, regions, server, '/strata/index.csv?', FALSE, TRUE))
}

#' Download taxonomic data from server
#' @export
#' @description
#' Download taxonomic and life history data from server
#' @inheritParams getSampleData
#' @return
#' A data.frame with taxonomic data and life history data for
#' all species in the RVC
getTaxonomicData = function(server = 'http://localhost:3000') {
  message('downloading stratum data')
  ## Test that server can be accessed
  if(!RCurl::url.exists(server))stop("could not access server")
  ## The url to get the taxonomic data
  u = paste(server, '/taxa/index.csv', sep = "")
  return(.download_csv(u, FALSE))
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
  download.file(u, temp, quiet = TRUE)
  ## Check that file has content
  if(file.size(temp)<1){stop("there was an error downloading the data")}
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
#' "DRTO", "SEFCRI")
#' @param server
#' A string containing the domain url at which to access the server
#' @param path
#' A string of the relative path to this element of the api
#' @param zipped
#' A boolean indiating whether a file is zipped file
#' or not
#' @param quiet
#' A boolean indicating whether messages should be printed or not
.getData = function(years, regions, server = "http://localhost:3000", path, zipped, quiet) {
  ## Test that server can be accessed
  if(!RCurl::url.exists(server))stop("could not access server")
  ## Create url for each query, escape spaces and other URI unsafe characters
  combined = cbind(years = years, regions = rep(regions, each = length(years)))
  queries = unlist(lapply(paste(server, path, 'year=', combined[,1], '&region=', combined[,2], sep=""), URLencode))
  ## Figure out which queries are valid
  keep = unlist(lapply(queries, RCurl::url.exists))
  valid_queries = queries[keep]
  ## If no queries are valid, return error
  if(length(valid_queries) == 0)stop("could not access data")
  ## Download data and store into a list
  data = lapply(valid_queries, .download_csv, zipped)
  if(!quiet & sum(keep) != nrow(combined)){
    msg = paste("The following combinations of region/year could not be found:",
                paste(combined[!keep,2], "/", combined[!keep,1], collapse = ", ", sep = ""))
    message(msg)
  }
  ## Return row bound data
  return(do.call(rbind, data))
}
