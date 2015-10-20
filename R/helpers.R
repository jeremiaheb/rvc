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
