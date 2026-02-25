## Groups species based on lookup table
## @param x
## Output from ssu_density or ssu_biomass
## @param taxonomic_data
## Taxonomic data.frame
## @param ...
## A list of arguments, possibly including an
## argument named group with a data.frame to use as a
## lookup table
## @return a data.frame with SPECIEC_CD changed to represent the
## group and density/biomass summed by SPECIES_CD
#' @importFrom dplyr group_by summarize mutate across all_of
#' @importFrom rlang .data

species_group = function(x, ...) {
  ## Get arguments as list
  dots = list(...)
  
  ## Aggregate by group if group argument present
  if("group" %in% names(dots)){
    group = dots$group
    
    ## Change species code to group using base R match
    x$SPECIES_CD = group[match(x$SPECIES_CD, group[,1]), 2]
    
    ## Figure out what stat is being calculated
    stat = ifelse("density" %in% names(x), "density",
           ifelse("biomass" %in% names(x), "biomass",
           ifelse("occurrence" %in% names(x), "occurrence", NULL)))
           
    by_cols = .aggBy('ssu')
    
    if (stat == "occurrence") {
      x <- x %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) %>%
        dplyr::summarize(
          occurrence = ifelse(sum(.data$occurrence) > 0, 1, 0),
          .groups = "drop"
        ) %>%
        as.data.frame()
    } else if (stat %in% c("density", "biomass")) {
      # Because biomass is temporarily renamed to density in the original logic,
      # we can handle both exactly the same way using the dynamically detected column name
      x <- x %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) %>%
        dplyr::summarize(
          # Use the := operator with !!sym() to dynamically name and evaluate the column
          !!stat := sum(!!rlang::sym(stat)),
          .groups = "drop"
        ) %>%
        as.data.frame()
    }
  }
  return(x)
}
