## Filter data before analysis

#' Filter data by species
#' @export
#' @description Subsets data by species
#' @param x
#' A data.frame containing a SPECIES_CD column
#' @param species_cd
#' A character vector containing the species codes by which to
#' subset data. \strong{Note:} Species codes contain the
#' first three letters of the genus name, a space, and the
#' first four letters of the species name. They are
#' not case sensitive in this function
species_filter = function(x, species_cd) {
  return(subset(x, SPECIES_CD %in% toupper(species_cd)))
}

#' Filter data by stratum
#' @export
#' @description Subsets data by species
#' @param x
#' A data.frame containing a STRAT column
#' @param strata
#' A character vector containing the stratum codes by which to subset the data.
strata_filter = function(x, strata) {
  return(subset(x, STRAT %in% toupper(strata)))
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
