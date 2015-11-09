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
  RCurl::url.exists(server)
  ## Empty list to hold daata
  l = list()
  ## Escape spaces
  regions_esc = URLencode(regions)

  message("downloading files ...")
  for(yr in seq_along(years)){
    for(rg in seq_along(regions_esc)){
      message(paste("downloading sample data for year:", years[yr],"and region:", regions[rg]))
      ## Set up uri to retrieve file
      uri = paste(server, "/samples/index.zip?","year=",years[yr],"&","region=",regions_esc[rg], sep="")
      temp = tempfile() # temp file to store zip
      ## Download file
      suppressWarnings(download.file(uri, temp, quiet = TRUE, mode = "wb"))
      ## If successfully downloaded, unzip and store as data.frame, else warn
      if(file.size(temp) > 0){
        message("downloaded. unzipping ...")
        l[[length(l)+1]] = read.csv(unzip(temp, exdir = tempdir()))
        unlink(temp) #clean up temporary file
        message(paste("finished processing sample data for year:", years[yr],"and region:", regions[rg]))
      } else {
        msg = paste("failed to retrieve data for year:", years[yr],"and region:", regions[rg])
        message(msg)
        warning(msg)
      }
    }
  }
  ## Rbind data and return
  sample_data = do.call(rbind, l)
  message("done")
  return(sample_data)
}
