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
getSampleData = function(years, regions, server = "http://localhost:3000") {
  ## Test that server can be accessed
  if(!RCurl::url.exists(server))stop("could not access server")
  ## Create url for each query, escape spaces and other URI unsafe characters
  combined = cbind(years = years, regions = rep(regions, each = length(years)))
  queries = unlist(lapply(paste(server, '/samples/index.zip?', 'year=', combined[,1], '&region=', combined[,2], sep=""), URLencode))
  ## Figure out which queries are valid
  keep = unlist(lapply(queries, RCurl::url.exists))
  valid_queries = queries[keep]
  ## If no queries are valid, return error
  if(length(valid_queries) == 0)stop("could not access data")
  ## Download data and store into a list
  message("downloading data ...")
  data = lapply(valid_queries, download_csv, zipped = TRUE)
  message("done")
  if(sum(keep) != nrow(combined)){
    msg = paste("The following combinations of region/year could not be found:",
                paste(combined[!keep,2], "/", combined[!keep,1], collapse = ", ", sep = ""))
    message(msg)
  }
  ## Return row bound data
  return(do.call(rbind, data))
}

#' Download, unzip, and write a file to a csv
#' @param u
#' A string. The URL at which to find the file
#' @param zipped
#' A boolean indiating whether a file is zipped file
#' or not
download_csv = function(u, zipped) {
  message('.')
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
  message('.')

  return(out)
}
