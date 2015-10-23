## Filter data before analysis

#' Filter data by species
#' @export
#' @description Subsets data by species
#' @param x
#' A data.frame containing a SPECIES_CD column
#' @param species_cd
#' A character vector containing the species codes by which to
#' subset data. NULL (default) will return the original data.
#'  \strong{Note:} Species codes contain the
#' first three letters of the genus name, a space, and the
#' first four letters of the species name.
#' @return A data.frame of the original data subset by
#' species code
species_filter = function(x, species_cd = NULL) {
  if(!is.null(species_cd)){
    x = subset(x, SPECIES_CD %in% toupper(species_cd))
  }
  return(x)
}

#' Filter data by stratum
#' @export
#' @description Subsets data by species
#' @param x
#' A data.frame containing a STRAT column
#' @param strata
#' A character vector containing the stratum codes by which to subset the data.
#' NULL (default) will return the original data
#' @return A data.frame of the original data subset by
#' strata
strata_filter = function(x, strata = NULL) {
  if (!is.null(strata)) {
    x = subset(x, STRAT %in% toupper(strata))
  }
  return(x)
}

#' Filter data by protected status
#' @export
#' @description Subsets data by protected status
#' @param x
#' A data.frame containing a PROT column
#' @param status
#' A numeric vector specifying the protected status.
#' Generally, 0 for unprotected, can be 1 or 2 for
#' different levels of protection. Defuault value of NULL
#' will return all protected statuses
#' @param is_protected
#' A boolean indicating whether to select only
#' protected (TRUE), unprotected (FALSE), or both (NULL, the default)
#' @return A data.frame of the original data subset by
#' protected status
protected_filter = function(x, status = NULL, is_protected = NULL){
  ## If status is not null, subset by provided statuses
  if(!is.null(status)){
    x = subset(x, PROT %in% status)
  }
  ## If is_protected is not null, subset by protected status
  if(!is.null(is_protected)){
    if (is_protected) {
      x = subset(x, PROT > 0) # Only protected areas
    } else {
      x = subset(x, PROT < 1) # Only unprotected areas
    }
  }

  return(x)
}

#' Filters data by year
#' @export
#' @description
#' Subsets data by year
#' @param x
#' A data.frame containing a YEAR column
#' @param years
#' A numeric vector of years. NULL (defualt)
#' returns original data
#' @return A data.frame of the original data
#' subset by years
year_filter = function(x, years = NULL) {
  if(!is.null(years)){
    x = subset(x, YEAR %in% years)
  }

  return(x)
}

#' Filters data by region
#' @export
#' @description
#' Subsets data by region
#' @param x
#' A data.frame containing a region column
#' @param regions
#' A character vector of regions codes. NULL (default)
#' returns original data
#' @return A data.frame of the original data
#' subset by region
region_filter = function(x, regions = NULL) {
  if(!is.null(regions)) {
    x = subset(x, REGION %in% regions)
  }

  return(x)
}

#' Filters data by length
#' @export
#' @description
#' Subsets data by length
#' @param x
#' A data.frame containing a LEN column
#' @param greater_than_or_equal_to
#' A number which indicates the minimum length
#' in centimeters (inclusive) by which to subset the data.
#' NULL (default) will return the original data
#' @param less_than
#' A number which indicates the maximum length
#' in centimeters (exclusive) by which to subset the data.
#' NULL (default) will return the original data
#' @return A data.frame with the original data subset by length
length_filter = function(x, greater_than_or_equal_to = NULL, less_than = NULL) {
  if(!is.null(greater_than_or_equal_to)){
    x = subset(x, LEN >= greater_than_or_equal_to)
  }
  if(!is.null(less_than)) {
    x = subset(x, LEN < less_than)
  }

  return(x)
}

#' Filters data by count
#' @export
#' @description
#' Subsets data by count
#' @param x
#' A data.frame containing a NUM column
#' @param greater_than_or_equal_to
#' A number which indicates the minimum count
#' to include
#' @param less_than
#' A number which indicates the maximum count
#' to include
#' @return A data.frame subset by count
count_filter = function(x, greater_than_or_equal_to = NULL, less_than = NULL) {
  if(!is.null(greater_than_or_equal_to)){
    x = subset(x, NUM >= greater_than_or_equal_to)
  }
  if(!is.null(less_than)) {
    x = subset(x, NUM < less_than)
  }

  return(x)
}
