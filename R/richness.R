## Diversity functions

#' Species richness at the SSU level
#' @export
#' @description Calculates the number of species observed at the
#' secondary sampling unit level
#' @inheritParams ssu_density
#' @return A data.frame with species richness per SSU
ssu_richness = function(x){
  ## Columns by which to aggregate data
  by = .aggBy("ssu")
  by = by[by != "SPECIES_CD"]
  ## Get the summarize function from plyr
  summarize = get('summarize', asNamespace('plyr'))
  ## Calculate richness
  return(plyr::ddply(x, by, summarize, richness = length(unique(SPECIES_CD[NUM > 0]))))
}

#' Species richness at the PSU level
#' @export
#' @description Calculates the average number of species observed
#' per SSU for each PSU
#' @inheritParams ssu_density
#' @return A data.frame with average species richness per SSU for
#' each PSU
psu_richness = function(x){
  return(.wrapRichness(x, psu_density))
}

#' Species richness at the stratum level
#' @export
#' @inheritParams strat_density
#' @description
#' Calculates the average number of species observed
#' per SSU for each stratum
#' @return A data.frame with average species richness per SSU
#' for each stratum
strat_richness = function(x, ntot){
  return(.wrapRichness(x, strat_density, ntot))
}

#' Species richness at the sampling domain level
#' @export
#' @description
#' Calculates the average number of species observed
#' per SSU for each sampling domain
#' @return A data.frame with average species richness per SSU
#' for each sampling domain
domain_richness = function(x, ntot) {
  return(.wrapRichness(x, domain_density, ntot))
}

## Helper function to wrap richness functions
## x: input data
## fun: function to wrap with
## ...: arguments to pass to fun
.wrapRichness = function(x, fun, ...){
  ## Create a dummy column for SPECIES_CD
  x$SPECIES_CD = rep(1, nrow(x))
  out = .wrapFunction(x, "richness", "density", fun, ...)
  ## Remove dummy column
  keep = names(out) != "SPECIES_CD"
  return(out[keep])
}
