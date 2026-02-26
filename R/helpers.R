## Helper functions shared within package

## Helper function to produce aggregate by variables
.aggBy = function(level) {
  # Base R 'switch' is highly optimized for this exact use case
  cols <- switch(level,
         "ssu"    = c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT", "STATION_NR", "SPECIES_CD"),
         "psu"    = c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT", "SPECIES_CD"),
         "strat"  = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD"),
         "domain" = c("YEAR", "REGION", "SPECIES_CD"),
         stop("Invalid aggregation level. Must be 'ssu', 'psu', 'strat', or 'domain'.")
  )
  
  return(cols)
}

## Helper to wrap one function in another and rename a column from
## input/output
## x: the data.frame upon which fun will be called
## outName: name of the column outside the wrapped function
## inName: name of the column inside the wrapped function
.wrapFunction = function(x, inName, outName, fun, ...) {
  
  # 1. Rename inName to outName for the incoming data
  names(x)[names(x) == inName] <- outName
  
  # 2. Execute the mathematical function
  res <- fun(x, ...)
  
  # 3. Rename outName back to inName on the output data
  names(res)[names(res) == outName] <- inName
  
  return(res)
}

#' @importFrom dplyr group_by mutate ungroup select inner_join semi_join filter pull
#' @importFrom magrittr %>%
#' @importFrom rlang .data

.getWeight = function(x, ntot){
  
  whNtot <- ntot %>%
    dplyr::group_by(.data$YEAR, .data$REGION) %>%
    dplyr::mutate(
      TOT = sum(.data$NTOT),
      wh = .data$NTOT / .data$TOT
    ) %>%
    dplyr::ungroup()
    
  ## Make sure weights sum to 1 for each domain (Keeping the original safety check)
  domains = unique(ntot[c('YEAR', 'REGION')])
  stopifnot(all.equal(sum(whNtot$wh), nrow(domains)))
  
  ## Merge with data and return
  res <- x %>%
    dplyr::inner_join(
      whNtot %>% dplyr::select("YEAR", "REGION", "STRAT", "PROT", "wh"),
      by = c("YEAR", "REGION", "STRAT", "PROT")
    ) %>%
    as.data.frame()
    
  return(res)
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
  ## Solves the TODO: Uses a clean semi_join instead of concatenating strings
  res <- stratum_data %>%
    dplyr::semi_join(sample_data, by = c("YEAR", "REGION", "STRAT", "PROT")) %>%
    as.data.frame()
    
  return(res)
}

## A helper function to handle all of the filtering and calculation but taking different callbacks
## depending on the level and statistic
## x: a list of RVC data
## species: common/scientific/species_cd
## length_bins: numeric vector of length bins
## wrapper: a symbol, the name of the calling wrapper function
## fun: a function computing the statistic that the wrapper wraps
## ... : optional arguments to the various filters
#' @importFrom magrittr %>%

.wrapperProto = function(x, species, length_bins, merge_protected, wrapper, fun, ...){
  
  ## Try to get sample, stratum, and taxonomic data from x
  sample_data = x[['sample_data']]
  stratum_data = x[['stratum_data']]
  taxonomic_data = x[['taxonomic_data']]

  # Use short-circuiting || instead of vectorized | for safety and CRAN compliance
  if(is.null(sample_data) || is.null(stratum_data) || is.null(taxonomic_data)){
    stop('could not find all of the data.frames -- sample_data, stratum_data, and taxonomic_data -- in object x')
  }

  #######################################################################
  #Get species codes, if no species found raise error. If mismatch warn #
  #######################################################################
  species_cd = .getSpecies_cd(species, taxonomic_data)

  if(length(species_cd) < 1){
    sl = paste(species, collapse = ", ")
    stop(paste('no species found with name(s):', sl))
  } else if (length(species_cd) != length(species)) {
    warning(paste('length mismatch between species provided and species found.\n', 
                  length(species), ' species provided ', 
                  length(species_cd), ' species found.', sep=""))
  }

  ## If group in args convert first column to species codes
  dots = list(...)
  if("group" %in% names(dots)){
    if(!is.data.frame(dots$group)){ stop("group must be a data.frame") }
    
    sl2 = lapply(lapply(dots$group[,1], .getSpecies_cd, x$taxonomic_data), as.character)
    dots$group[,1] = unlist(lapply(sl2, function(sp){ ifelse(length(sp) == 0, NA, sp) }))
    
    missing = !(species_cd %in% dots$group[,1])
    
    ## If species are missing from lookup table raise error
    if(any(missing)){
      stop(paste("species argument contains species not found in group argument's lookup table:",
                 paste(species_cd[missing], collapse = ", ")))
    }
    
    ## If extra species in lookup table, warn
    extra = !(dots$group[,1] %in% species_cd)
    if(any(extra)){
      warning(paste("group argument lookup table contains extra species not found in species arguments:",
                    paste(dots$group[extra,1], collapse = ", ")))
    }
  }

  ##########################################################################
  #################### Calculate Statistics ################################
  ##########################################################################

  ## Base Case: merge_protected
  if(merge_protected){
    ## Base Case: No length bins
    # We use a string check for the wrapper functions to avoid evaluating missing promises
    if(is.null(length_bins) || 
       identical(as.character(substitute(wrapper)), "getStratumLengthFrequency") ||
       identical(as.character(substitute(wrapper)), "getDomainLengthFrequency")) {
      
      ## Apply filters to sample data
      sample_data = .apply_sample_filters(sample_data, species_cd, ...)
      ## Apply filters to stratum data
      stratum_data = .apply_stratum_filters(stratum_data, sample_data, ...)

      out = do.call(fun, c(list(sample_data, stratum_data), dots))
      
      if("group" %in% names(dots) && !("frequency" %in% names(out))){
        names(out)[names(out) == "SPECIES_CD"] = "GROUP"
      }
    } else {
      ## Recursive Case: Length bins present
      out = do.call(.funByLen, c(list(x, species_cd, length_bins, wrapper), dots))
    }
  } else {
    ## Recursive Case: merge_protected is FALSE
    out = do.call(.funByProt, c(list(x, species_cd, length_bins, wrapper), dots))
  }

  return(out)
}

## Applies filters to sample data
## x: unfiltered sample_data
## species: species codes by which to filter the data
#' @importFrom magrittr %>%

.apply_sample_filters = function(x, species, ...){
  
  filtered = x %>%
    species_filter(species_cd = species, ...) %>%
    count_filter(...) %>%
    length_filter(...) %>%
    region_filter(...) %>%
    year_filter(...) %>%
    protected_filter(...) %>%
    strata_filter(...)
    
  return(filtered)
}

## Applies filter to stratum data
## stratum_data: unfiltered stratum data
## sample_data: filtered sample data
#' @importFrom magrittr %>%

.apply_stratum_filters = function(stratum_data, sample_data, ...){
  
  filtered = stratum_data %>%
    region_filter(...) %>%
    year_filter(...) %>%
    protected_filter(...) %>%
    strata_filter(...) %>%
    .with_strata(sample_data)
    
  return(filtered)
}


## Get the species code of a species given its
## scientific/common name or species codes
## common names and species codes are not
## case sensitive
.getSpecies_cd = function(species, taxonomic_data) {
  ## get species codes from taxonomic_data
  res <- taxonomic_data %>%
    dplyr::filter(
      .data$SPECIES_CD %in% toupper(species) |
      .data$SCINAME %in% species |
      toupper(.data$COMNAME) %in% toupper(species)
    ) %>%
    dplyr::pull(.data$SPECIES_CD) %>%
    as.character()
    
  return(res)
}

## Calls the callback function multiple times on each length increment
## @inheritParams getStratumDensity
## @param cb
## A callback to one of the wrapper functions, e.g. getStratumDensity, getDomainDensity
## @param ...
## Optional parameters passed to the callback
.funByLen = function(x, species, length_bins, cb, ...) {
  
  ## Safely check if length_bins is a string like "LC" or "LM"
  if (is.character(length_bins) && length(length_bins) == 1) {
    bin_type <- toupper(length_bins)
    if (bin_type == "LC") {
      length_bins <- x$taxonomic_data[c("SPECIES_CD", "LC")]
    } else if (bin_type == "LM") {
      length_bins <- x$taxonomic_data[c("SPECIES_CD", "LM")]
    }
  }

  ## Recursive Case: If length_bins is a data.frame, run for each species
  if (is.data.frame(length_bins)) {
    ## Get species codes for species in length_bins column 1
    length_bins[,1] <- .getSpecies_cd(length_bins[,1], x$taxonomic_data)
    
    ## Check that all requested species have breakpoints
    missing <- !(species %in% length_bins[!is.na(length_bins[,2]), 1])
    if (any(missing)) {
      warning(paste('could not find breakpoints for species', paste(species[missing], collapse = ", ")))
      species <- species[!missing] # Scrub missing species
    }
    
    ## For each species, run .funByLen recursively with just that species and its lookup
    l <- lapply(species, function(sp) {
      sp_bins <- length_bins[length_bins[,1] == sp, 2]
      .funByLen(x, sp, sp_bins, cb, ...)
    })
    
    return(do.call(rbind, l))
  }
  
  ## Base Case: length_bins is a numeric vector
  n <- length(length_bins)
  l <- list()
  
  ## callback value below lowest bin value
  l[[1]] <- cb(x, species, len_lt = length_bins[1], ...)
  l[[1]]$length_class <- paste("<", length_bins[1])
  
  ## Calculate between bin values
  if (n > 1) {
    for (i in 1:(n - 1)) {
      l[[i + 1]] <- cb(x, species, len_geq = length_bins[i], len_lt = length_bins[i + 1], ...)
      l[[i + 1]]$length_class <- paste0('[', length_bins[i], ', ', length_bins[i + 1], ')')
    }
  }
  
  ## callback to above highest bin value
  l[[n + 1]] <- cb(x, species, len_geq = length_bins[n], ...)
  l[[n + 1]]$length_class <- paste(">=", length_bins[n])
  
  ## callback to all combined
  l[[n + 2]] <- cb(x, species, ...)
  l[[n + 2]]$length_class <- "all"
  
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
  
  ## Get unique protected statuses
  prot <- unique(x$stratum_data$PROT)
  
  ## Use lapply to calculate statistics for each protected status cleanly
  l <- lapply(prot, function(p) {
    # CRITICAL FIX: Added ... so user filters aren't silently dropped!
    res <- cb(x, species, length_bins, status = p, ...) 
    
    # R automatically recycles this single value to fill the column
    res$protected_status <- p 
    return(res)
  })

  ## Rbind and return
  return(do.call(rbind, l))
}

## Helper function that pull growth parameters from taxonomic data table
## raises error if not all are found
## @param x List returned by getRvcData
## @param species list of species scientific/common names or species codes
## @param growth_parameters A list or data.frame of growth parameters,
## if NULL function uses taxonomic data, else passes on parameters
## Helper function that pulls growth parameters from taxonomic data table
## raises error if not all are found
.getGrowthParameters = function(x, species, growth_parameters) {
  
  ## Base Case: No parameters provided, use taxonomic data
  if(is.null(growth_parameters)){
    message('no growth parameters given. Using growth parameters from taxonomic data (?getTaxonomicData)')
    growth_parameters <- x$taxonomic_data
  }
  
  ## Standardize column/element names
  names(growth_parameters)[names(growth_parameters) == 'a'] <- "WLEN_A"
  names(growth_parameters)[names(growth_parameters) == 'b'] <- "WLEN_B"
  names(growth_parameters) <- toupper(names(growth_parameters))
  
  ## Check that critical WLEN_A and WLEN_B variables are available
  if(!all(c("WLEN_A", "WLEN_B") %in% names(growth_parameters))){
    stop("could not find variables 'WLEN_A'|'a', 'WLEN_B'|'b' in growth_parameters")
  }
  
  ## If growth_parameters is a data.frame, validate and standardize species codes
  if(is.data.frame(growth_parameters)){
    
    if(!("SPECIES_CD" %in% names(growth_parameters))){
      stop("could not find column 'SPECIES_CD' in growth_parameters")
    }
    
    ## Convert any common/scientific names to standard species codes
    growth_parameters$SPECIES_CD <- .getSpecies_cd(
      growth_parameters$SPECIES_CD, 
      x$taxonomic_data
    )
  }
  
  return(growth_parameters)
}

## Checks species names in growth_parameters against species arguments
## Warns if doesn't match, and scrubs missing species from sample_data
## x: A list containing sample_data and taxonomic_data
## species: A character vector of species
## growth_parameters: A list or data.frame
## Checks species names in growth_parameters against species arguments
## Warns if doesn't match, and scrubs missing species from sample_data
.checkSpeciesMatch = function(x, species, growth_parameters) {
  
  ## Only execute the check if growth_parameters is a data.frame
  if(is.data.frame(growth_parameters)){
    
    spccd <- .getSpecies_cd(species, x$taxonomic_data)
    
    ## Filter to only the rows where both A and B parameters are present
    valid_gp <- growth_parameters[!is.na(growth_parameters$WLEN_A) & 
                                  !is.na(growth_parameters$WLEN_B), ]
    
    ## Identify which requested species are missing valid parameters
    missing <- !(spccd %in% valid_gp$SPECIES_CD)
    
    if(any(missing)){
      warning(paste('growth_parameters for', paste(spccd[missing], collapse = ', '), 'unavailable'))
      
      ## Scrub the missing species from the sample data safely
      valid_species <- spccd[!missing]
      x$sample_data <- x$sample_data[x$sample_data$SPECIES_CD %in% valid_species, , drop = FALSE]
    }
  }

  return(x)
}

