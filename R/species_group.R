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
    ## Figure out what stat is being calculated so it can be properly grouped
    ## NOTE: Only available for density/biomass/abundance/occurrence
    stat = ifelse("density" %in% names(x),
                  "density",
           ifelse("biomass" %in% names(x),
                   "biomass",
           ifelse("occurrence" %in% names(x),
                   "occurrence",
                   NULL)))
    ## Aggregate by group and recalculate density/biomass/occurrence
    summarize = get('summarize', asNamespace("plyr"))
    if(stat == "occurrence"){
      x = plyr::ddply(x, .aggBy('ssu'), summarize, occurrence = ifelse(sum(occurrence) > 0, 1, 0))
    }
    if(stat == "biomass"){
      names(x)[names(x) == "biomass"] = "density"
    }
    if(stat == "biomass" | stat == "density"){
      x = plyr::ddply(x, .aggBy('ssu'), summarize, density = sum(density))
    }
    if(stat == "biomass"){
      names(x)[names(x) == "density"] = "biomass"
    }
  }
  return(x)

}
