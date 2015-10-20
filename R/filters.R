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
