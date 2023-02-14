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

## Filter out strata in stratum_data data.frame by strata available in
## sample_data. Useful for cases where statistic is to be calculated for
## when a species is present.
## stratum_data: a data.frame of stratum nformation, including: YEAR, REGION,
## STRAT, and PROT columns
## sample_data: sample data information that has already been filtered by
## whatever criteria
## return: A data.frame of stratum_data filtered by the strata available in
## sample_data
.with_strata = function(stratum_data, sample_data){
  ## TODO: This is an ugly implemtation of subsetting by a combination
  ## of factors, find a better implementation
  keep = apply(unique(sample_data[c("YEAR", "REGION", "STRAT", "PROT")]),1,paste, collapse="")
  available = apply(stratum_data[c("YEAR", "REGION", "STRAT", "PROT")], 1, paste, collapse="")
  return(stratum_data[available %in% keep,])
}

## A helper function to handle all of the filtering and calculation but taking different callbacks
## depending on the level and statistic
## x: a list of RVC data
## species: common/scientific/species_cd
## length_bins: numeric vector of length bins
## wrapper: a symbol, the name of the calling wrapper function
## fun: a function computing the statistic that the wrapper wraps
## ... : optional arguments to the various filters
.wrapperProto = function(x, species, length_bins, merge_protected, wrapper, fun, ...){
  ###################################################################
  ##################### Get the data ################################
  ###################################################################

  ## Try to get sample, stratum, and taxonomic data from x
  sample_data = x[['sample_data']]
  stratum_data = x[['stratum_data']]
  taxonomic_data = x[['taxonomic_data']]

  if(is.null(sample_data) | is.null(stratum_data) | is.null(taxonomic_data)){
    msg = 'could not find all of the data.frames -- sample_data, stratum_data, and taxonomic_data -- in object x'
    stop(msg)
  }

  #######################################################################
  #Get species codes, if no species found raise error. If mismatch warn #
  #######################################################################

  species_cd = .getSpecies_cd(species, taxonomic_data)

  if(length(species_cd) < 1){
    sl = paste(species, collpase = ", ")
    msg = paste('no species found with name(s):', sl)
    stop(msg)}
  else if (length(species_cd) != length(species)) {
    msg = paste('length mismatch between species provided and species found.',
                '\n', length(species), ' species provided ', length(species_cd),
                ' species found.', sep="")
    warning(msg)
  }

  ## If group in args convert first column to species codes
  dots = list(...)
  if("group" %in% names(dots)){
    if(!is.data.frame(dots$group)){stop("group must be a data.frame")}
    sl2 = lapply(lapply(dots$group[,1], .getSpecies_cd, x$taxonomic_data), as.character)
    dots$group[,1] = unlist(lapply(sl2, function(x){ifelse(length(x) == 0, NA, x)}))
    missing = !(species_cd %in% dots$group[,1])
    ## If species are missing from lookup table raise error
    if(any(missing)){
      msg = paste("species argument contains species not found in group argument's lookup table:",
                  paste(species_cd[missing], collapse = ", "))
      stop(msg)
    }
    ## If extra species in lookup table, warn
    extra = !(dots$group[,1] %in% species_cd)
    if(any(extra)){
      msg = paste("group argument lookup table contains extra species not found in species arguments:",
          paste(dots$group[extra,1], collapse = ", "))
      warning(msg)
    }
  }

  ##########################################################################
  #################### Calculate Statistics ################################
  ##########################################################################

  ## Base Case: merge_protected
  if(merge_protected){
    ## Base Case: No length bins
    if(is.null(length_bins) | identical(wrapper, getStratumLengthFrequency) |
       identical(wrapper, getDomainLengthFrequency)){
      ## Apply filters to sample data
      sample_data = .apply_sample_filters(sample_data, species_cd, ...)
      ## Apply filters to stratum data
      stratum_data = .apply_stratum_filters(stratum_data, sample_data, ...)

      out = do.call(fun, c(list(sample_data, stratum_data), dots))
      if("group" %in% names(dots) && !("frequency" %in% names(out))){
        names(out)[names(out) == "SPECIES_CD"] = "GROUP"
      }
    }
    ## Recursive Case: Lenth bins present
    else {
      out = do.call(.funByLen, c(list(x, species_cd, length_bins, wrapper), dots))
    }
    ## Recursive Case: merge_protected is FALSE
  } else {
    out = do.call(.funByProt, c(list(x, species_cd, length_bins, wrapper), dots))
  }

  ## Return statistic
  return(out)
}

## Applies filters to sample data
## x: unfiltered sample_data
## species: species codes by which to filter the data
.apply_sample_filters = function(x, species, ...){
  filtered = strata_filter(
    protected_filter(
      year_filter(
        region_filter(
          length_filter(
            count_filter(
              species_filter(x, species_cd = species), ...
            ), ...
          ), ...
        ), ...
      ), ...
    ), ...
  )

  return(filtered)
}

## Applies filter to stratum data
## stratum_data: unfiltered stratum data
## sample_data: filtered sample data
.apply_stratum_filters = function(stratum_data, sample_data, ...){
  ## Apply filters to stratum data
  stratum_data = strata_filter(protected_filter(stratum_data, ...), ...)
  if(hasArg("when_present") && list(...)$when_present) {
    ## Filter out strata in stratum_data that are not present in sample_data
    stratum_data = .with_strata(stratum_data, sample_data)
  }
  return(stratum_data)
}


## Get the species code of a species given its
## scientific/common name or species codes
## common names and species codes are not
## case sensitive
.getSpecies_cd = function(species, taxonomic_data) {
  ## get species codes from taxonomic_data
  species_cd = with(taxonomic_data, SPECIES_CD[SPECIES_CD %in% toupper(species) |
                                                 SCINAME %in% species |
                                                 toupper(COMNAME) %in% toupper(species)])
  return(species_cd)
}

## Calls the callback function multiple times on each length increment
## @inheritParams getStratumDensity
## @param cb
## A callback to one of the wrapper functions, e.g. getStratumDensity, getDomainDensity
## @param ...
## Optional parameters passed to the callback
.funByLen = function(x, species, length_bins, cb, ...) {
  ## If length_bins is "lc" use lookup from taxonomic_data
  if(toupper(length_bins[1]) == "LC"){
    length_bins = x$taxonomic_data[c("SPECIES_CD", "LC")]
  }
  ## If length_bins is "lm" use lookup from taxonomic_data
  if(toupper(length_bins[1]) == "LM"){
    length_bins = x$taxonomic_data[c("SPECIES_CD", "LM")]
  }
  ## Recursive Case: If length_bins is a data.frame run .funByProt
  ## for each species
  if(is.data.frame(length_bins)){
    ## Get species codes for species in length_bins column 1
    length_bins[,1] = .getSpecies_cd(length_bins[,1], x$taxonomic_data)
    ## Check that all species in SPECIES_CD
    missing = !(species %in% length_bins[!is.na(length_bins[,2]),1])
    if(any(missing)){
      warning(paste('could not find breakpoints for species', paste(species[missing], collapse = ", ")))
      ## Scrub missing species
      species = species[!missing]
    }
    ## For each species in species, fun .funByProt with just that species
    ## and its lookup
    l = list()
    for (i in seq_along(species)){
      l[[i]] = .funByLen(x, species[i], length_bins[length_bins[,1] == species[i],2], cb, ...)
    }
  }
  ## Base Case: length_bins is not a data.frame
  else {
    ## Make an empty list
    n = length(length_bins)
    l = list();
    ## callback value below lowest bin value
    l[[1]] = cb(x, species, len_lt = length_bins[1], ...)
    l[[1]]$length_class = rep(paste("<", length_bins[1]), nrow(l[[1]]))
    ## Calculate between bin values
    if (n > 1){
      for(i in 1:(n-1)){
        l[[i+1]] = cb(x, species, len_geq = length_bins[i], len_lt = length_bins[i+1], ...)
        l[[i+1]]$length_class = rep(paste('[', length_bins[i], ', ', length_bins[i+1], ')', sep = ""), nrow(l[[i+1]]))
      }
    }
    ## callback to above highest bin value
    l[[n+1]] = cb(x, species, len_geq = length_bins[n], ...)
    l[[n+1]]$length_class = rep(paste(">=", length_bins[n]), nrow(l[[n+1]]))
    ## callback to all combined
    l[[n+2]] = cb(x, species, ...)
    l[[n+2]]$length_class = rep("all", nrow(l[[n+2]]))
  }

  return(do.call(rbind, l))
}

## Calls the callback for each protected status
## @inheritParams stratumDensity
## @param cb
## A callback to one of the wrapper functions (e.g. stratumDensity, domainAbundance)
## @length_bins
## A numeric vector of lengths with which to bin the data
## @param ...
## Optional arguments passed to the callback
.funByProt = function(x, species, length_bins, cb, ...) {
  ## An empty list to hold the different cases
  l = list()
  ## Calculate stastistic for each protected status
  prot = unique(x$stratum_data$PROT)
  n = length(prot)
  for(i in 1:n){
    l[[i]] = cb(x, species, length_bins, status = prot[i])
    l[[i]]$protected_status = rep(prot[i], nrow(l[[i]]))
  }

  ## Rbind and return
  return(do.call(rbind, l))
}

## Helper function that pull growth parameters from taxonomic data table
## raises error if not all are found
## @param x List returned by getRvcData
## @param species list of species scientific/common names or species codes
## @param growth_parameters A list or data.frame of growth parameters,
## if NULL function uses taxonomic data, else passes on parameters
.getGrowthParameters = function(x, species, growth_parameters){
  if(is.null(growth_parameters)){
    message('no growth parameters given. Using growth parameters from taxonomic data (?getTaxonomicData)')
    growth_parameters = x$taxonomic_data
  }
  ## If growth parameters has names a, or b change to WLEN_A and WLEN_B
  names(growth_parameters)[names(growth_parameters) == 'a'] = "WLEN_A"
  names(growth_parameters)[names(growth_parameters) == 'b'] = "WLEN_B"
  names(growth_parameters) = toupper(names(growth_parameters))
  ## Check that WLEN_A and WLEN_B variables are available
  if(!all(c("WLEN_A", "WLEN_B") %in% names(growth_parameters))){
    stop("could not find variables 'WLEN_A'|'a', 'WLEN_B'|'b' in growth_parameters")
  }
  ## If growth_parameters is a data.frame look for correct columns
  if(is.data.frame(growth_parameters)){
    ## Check that SPECIES_CD is in growth_parameters
    if(!("SPECIES_CD" %in% names(growth_parameters))){
      stop("could not find columns 'SPECIES_CD' in growth_parameters")
    }
    ## Make all species codes valid
    growth_parameters$SPECIES_CD = .getSpecies_cd(growth_parameters$SPECIES_CD, x$taxonomic_data)
  }
  return(growth_parameters)
}

## Checks species names in growth_parameters against species arguments
## Warns if doesn't match, and scrubs missing species from sample_data
## x: A list containing sample_data and taxonomic_data
## species: A character vector of species
## growth_parameters: A list or data.frame
.checkSpeciesMatch = function(x, species, growth_parameters) {
  ## Only check if growth_parameters is data.frame
  if(is.data.frame(growth_parameters)){
    spccd = .getSpecies_cd(species, x$taxonomic_data)
    missing = !(spccd %in% growth_parameters$SPECIES_CD[!is.na(growth_parameters$WLEN_A) &
                                                          !is.na(growth_parameters$WLEN_B)])
    if(any(missing)){
      warning(paste('growth_parameters for', paste(spccd[missing], collapse = ', '), 'unavailable'))
      x$sample_data = x$sample_data[x$sample_data$SPECIES_CD %in% spccd[!missing],]
    }
  }

  return(x)
}

