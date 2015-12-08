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
species_group = function(x, taxonomic_data, ...) {
  ## Get arguments as list
  dots = list(...)
  ## Aggregate by group if group argument present
  if("group" %in% names(dots)){
    group = dots$group
    if(!is.data.frame(group)){stop("group must be a data.frame")}
    ## Change species code to group
    x$SPECIES_CD = group[match(x$SPECIES_CD, group[,1]),2]
    ## Sum by group
    summarize = get('summarize', asNamespace('plyr'))
    return(plyr::ddply(x, .aggBy('ssu'), summarize, density = sum(density)))
  } else {
    return(x)
  }
}
