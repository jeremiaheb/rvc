## SSU Occurrence
## @export
## @description
## Calculates whether or not a particular species is present in a secondary sampling unit
## @inheritParams ssu_density
## @return A data.frame with a column, occurrence, indicating whether or not
## a species was present in a particular secondary sampling unit
ssu_occurrence = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("ssu")
  ## Get summarize function from plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Aggregate and return data
  return(plyr::ddply(x, by, summarize,
                     occurrence = ifelse(sum(NUM)>0,1,0)
                     ))
}

## PSU Occurrence
## @export
## @description
## Average occurrence per secondary sampling unit at the primary sampling unit level
## @param x
## A data.frame which is the output of ssu_occurrence
## @return A data.frame with a column, occurrence, of the average occurrence
## per secondary sampling unit (~177m^2) for a primary sampling unit,
## its associated between SSU variance (var), and the number of secondary sampling units
## sampled per primary sampling unit (m)
psu_occurrence = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("psu")
  ## Get the summarize function from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Aggregate and return the data
  return(plyr::ddply(x, by, summarize,
                     occurrence = mean(occurrence),
                     m = length(STATION_NR),
                     var = ifelse(m > 1, m/(m-1) * occurrence * (1 - occurrence), NA) # Discrete variance
                     ))
}

## Stratum occurrence
## @export
## @description
## Average occurrence per secondary sampling unit at the stratum level
## @param x
## A data.frame which is the output of psu_occurrence
## @inheritParams strat_density
## @return A data.frame with columns: occurrence, the average occurrence per SSU; var, the
## average variance in occurrence; n, the number of sampled primary sampling units;
## nm, the number of sampled secondary sampling units; N, the number of possible
## primary sampling units; NM, the number of possible secondary sampling units
strat_occurrence = function(x, ntot) {
  ## Wrap the domain_density function, renaming the occurrence column to/from density
  return(.wrapFunction(x, "occurrence", "density", strat_density, ntot))
}

## Domain Occurrence
## @export
## @description
## Average occurrence per secondary sampling unit at the sampling domain level
## @param x
## A data.frame which is the output of strat_occurrence
## @inheritParams strat_density
## @return A data.frame with columns: occurrence, the average occurrence per SSU; var, the
## average variance in occurrence; n, the number of sampled primary sampling units;
## nm, the number of sampled secondary sampling units; N, the number of possible
## primary sampling units; NM, the number of possible secondary sampling units
domain_occurrence = function(x, ntot) {
  ## Wrap the domain_density function, renaming the occurrence column to/from density
  return(.wrapFunction(x, "occurrence", "density", domain_density, ntot))
}
