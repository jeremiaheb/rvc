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
#' @param ...
#' Anonymous arguments to be passed to other filters
#' @return A data.frame of the original data subset by
#' species code
species_filter = function(x, species_cd = NULL, ...) {
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
#' @inheritParams species_filter
#' @return A data.frame of the original data subset by
#' strata
strata_filter = function(x, strata = NULL, ...) {
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
#' @inheritParams species_filter
#' @return A data.frame of the original data subset by
#' protected status
protected_filter = function(x, status = NULL, is_protected = NULL, ...){
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
#' @inheritParams species_filter
#' @return A data.frame of the original data
#' subset by years
year_filter = function(x, years = NULL, ...) {
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
#' @inheritParams species_filter
#' @return A data.frame of the original data
#' subset by region
region_filter = function(x, regions = NULL, ...) {
  if(!is.null(regions)) {
    x = subset(x, REGION %in% regions)
  }

  return(x)
}

#' Filters data by length
#' @export
#' @description
#' Sets count to zero for records where length is not within specified parameters
#' @param x
#' A data.frame containing a LEN and NUM column
#' @param len_geq
#' A number which indicates the minimum length
#' in centimeters (inclusive).
#' NULL (default) will return the original data
#' @param len_lt
#' A number which indicates the maximum length
#' in centimeters (exclusive).
#' NULL (default) will return the original data
#' @inheritParams species_filter
#' @return A data.frame with the original data subset by length
length_filter = function(x, len_geq = NULL, len_lt = NULL, ...) {
  if(!is.null(len_geq)){
    ## If length >= len_greq, keep NUM, else set NUM to 0
    x$NUM = ifelse(x$LEN >= 0 & x$LEN >= len_geq, x$NUM, 0)
  }
  if(!is.null(len_lt)) {
    ## If length is measured (unmeasured lengths are coded as -999 in old records)
    ## and is less than len_lt, keep it, else set NUM to 0
    x$NUM = ifelse(x$LEN >= 0 & x$LEN < len_lt, x$NUM, 0)
  }

  return(x)
}

#' Filters data by count
#' @export
#' @description
#' Subsets data by count
#' @param x
#' A data.frame containing a NUM column
#' @param cnt_geq
#' A number which indicates the minimum count
#' to include
#' @param cnt_lt
#' A number which indicates the maximum count
#' to include
#' @param when_present
#' A boolean. Selects samples where NUM > 0 if TRUE. False (default)
#' does not subset data.
#' @inheritParams species_filter
#' @return A data.frame subset by count
count_filter = function(x, cnt_geq = NULL, cnt_lt = NULL, when_present = FALSE, ...) {
  if(!is.null(cnt_geq)){
    x = subset(x, NUM >= cnt_geq)
  }
  if(!is.null(cnt_lt)) {
    x = subset(x, NUM < cnt_lt)
  }
  if(when_present) {
    x = subset(x, NUM > 0)
  }

  return(x)
}
