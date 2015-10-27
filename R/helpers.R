## Helper functions shared within package

## Helper function to produce aggregate by variables
.aggBy = function(level){
  by = switch(level,
              domain = c("YEAR", "REGION", "SPECIES_CD"),
              strat = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD"),
              psu = c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT",
                      "SPECIES_CD"),
              ssu = c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT",
                      "STATION_NR", "SPECIES_CD"),
              stop('invalid level')
  )
  return(by)
}

## Helper to wrap one function in another and rename a column from
## input/output
## x: the data.frame upon which fun will be called
## outName: name of the column outside the wrapped function
## inName: name of the column inside the wrapped function
.wrapFunction = function(x, outName, inName, fun, ...) {
  ## Change the outer name to the inner name
  names(x)[names(x) == outName] = inName
  ## Apply the function
  wrapped = fun(x, ...)
  ## Change the inner name to the outer name
  names(wrapped)[names(wrapped) == inName] = outName

  return(wrapped)
}

## Calculate weighting of strata, and merge with data
## x: A data.frame to merge with ntot. Must have
## YEAR, REGION, STRAT, PROT, and NTOT columns
## ntot: A data frame of strata info,
## including YEAR, REGION, STRAT, PROT, and NTOT columns
.getWeight = function(x, ntot){
  ## Calculate total for each domain
  tot = with(ntot, aggregate(list(TOT = NTOT), ntot[c('YEAR', 'REGION')], sum))
  ## Merge with ntot and calculate weighting
  whNtot = merge(ntot, tot)
  whNtot$wh = with(whNtot, NTOT/TOT)
  ## Make sure weights sum to 1 for each domain
  stopifnot(sum(whNtot$wh) == nrow(tot))
  ## Merge with data and return
  return(
    merge(x, whNtot[c("YEAR", "REGION", "STRAT", "PROT", "wh")])
  )
}
