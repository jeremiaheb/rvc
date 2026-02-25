## Diversity functions

## Species richness at the SSU level
## @description Calculates the number of species observed at the
## secondary sampling unit level
## @inheritParams ssu_density
## @return A data.frame with species richness per SSU
#' @importFrom dplyr group_by summarize mutate rename select across all_of
#' @importFrom magrittr %>%
#' @importFrom rlang .data

ssu_richness = function(x){
  ## Columns by which to aggregate data
  by = .aggBy("ssu")
  by = by[by != "SPECIES_CD"] # Exclude species code for richness
  
  ## Calculate richness
  res <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(
      richness = length(unique(.data$SPECIES_CD[.data$NUM > 0])),
      .groups = "drop"
    ) %>%
    as.data.frame()
    
  return(res)
}

## Species richness at the PSU level
## @description Calculates the average number of species observed
## per SSU for each PSU
## @inheritParams ssu_density
## @return A data.frame with average species richness per SSU for
## each PSU
psu_richness = function(x){
  return(.wrapRichness(x, psu_density))
}

## Species richness at the stratum level
## @inheritParams strat_density
## @description
## Calculates the average number of species observed
## per SSU for each stratum
## @return A data.frame with average species richness per SSU
## for each stratum
strat_richness = function(x, ntot){
  return(.wrapRichness(x, strat_density, ntot))
}

## Species richness at the sampling domain level
## @description
## Calculates the average number of species observed
## per SSU for each sampling domain
## @return A data.frame with average species richness per SSU
## for each sampling domain
domain_richness = function(x, ntot) {
  return(.wrapRichness(x, domain_density, ntot))
}

## Helper function to wrap richness functions
## x: input data
## fun: function to wrap with
## ...: arguments to pass to fun
.wrapRichness = function(x, fun, ...){
  res <- x %>%
    dplyr::mutate(SPECIES_CD = 1) %>% # Create a dummy column
    dplyr::rename(density = "richness") %>% # Disguise richness as density
    fun(...) %>% # Run the density function
    dplyr::rename(richness = "density") %>% # Restore the name
    dplyr::select(-.data$SPECIES_CD) %>% # Drop the dummy column
    as.data.frame()
    
  return(res)
}
