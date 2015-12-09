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
species_group = function(x, ...) {
  ## Get arguments as list
  dots = list(...)
  ## Aggregate by group if group argument present
  if("group" %in% names(dots)){
    group = dots$group
    ## Change species code to group
    x$SPECIES_CD = group[match(x$SPECIES_CD, group[,1]),2]
    ## If calculating biomass, temporarily rename biomass columns
    isBiomass = FALSE
    if("biomass" %in% names(x)){
      isBiomass = TRUE
      names(x)[names(x) == "biomass"] = "density"
    }
    ## Sum by group
    summarize = get('summarize', asNamespace('plyr'))
    x = plyr::ddply(x, .aggBy('ssu'), summarize, density = sum(density))
    if(isBiomass){
      names(x)[names(x) == "density"] = "biomass"
    }
  }
  return(x)

}
