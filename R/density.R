## SSU density
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the secondary sampling unit level
## @param x
## A data.frame including columns: REGION, YEAR, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
## STATION_NR, SPECIES_CD, NUM
## @return A data.frame with density per SSU for each SSU
ssu_density = function(x){
  ## variables to aggregate by
  by = .aggBy("ssu")
  ## Get summarize fn from plyr package
  summarize = get("summarize", asNamespace("plyr"));
  ## aggregate by SSU and sum count
  return(plyr::ddply(x, by, summarize, density = sum(NUM)))
}

## PSU density per SSU
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the primary sampling unit level
## @param x
## A data.frame of the output from ssu_density
## @return A data.frame with density per SSU aggregated by PSU and species,
## variance in density (var), and the number of ssus per psu (m)
psu_density = function(x) {
  ## variables to aggregate by
  by = .aggBy("psu")
  ## get summarize fn from plyr package
  summarize = get("summarize", asNamespace("plyr"))
  ## Aggregate by PSU, sum density and divide by number of SSUs
  return(plyr::ddply(x, by, summarize,
                     m = length(STATION_NR),
                     var = var(density),
                     density = sum(density)/length(STATION_NR)
  ))
}

## Stratum level density per SSU
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the stratum level
## @param x
## A data.frame which is the output from psu_density
## @param ntot
## A data.frame including columns: REGION, YEAR, STRAT, PROT, NTOT, GRID_SIZE, STAGE_LEVEL
## @return A data.frame with density per SSU aggregated by stratum and species, its variance (var),
## the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
## possible number of SSUs (NM), and the total possible number of PSUs (N)
strat_density <- function(x, ntot) {

  ## merge with ntot data
  merged = merge(x, ntot)
  ## Aggregate by variables
  by = .aggBy("strat")

  library(dplyr)
  strm =  merged %>%
    group_by(.dots=by) %>%
    summarise(
      v1 = var(density), # Between PSU variance
      STAGE_LEVEL = mean(STAGE_LEVEL),
      density = mean(density),
      n = length(PRIMARY_SAMPLE_UNIT),
      fn = n/mean(NTOT), #PSU variance weighting factor
      nm = ifelse(mean(STAGE_LEVEL) == 1,
                  NA,
                  sum(m)
      ),
      mtot = mean(GRID_SIZE)^2/(pi*7.5^2),
      var = ifelse(mean(STAGE_LEVEL) == 1,
                   (1-fn) * (v1/n),
                   (1-(n/mean(NTOT)))*vaxwr(density)/n + ((n/mean(NTOT))*(1-(mean(m)/mtot))*(sum(var, na.rm = TRUE)/sum(ifelse(m>1,1,0))))/nm
      ),
      density = mean(density),
      N = mean(NTOT),
      NM = mtot*N) %>%
    as.data.frame()

  keep = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "density", "var", "n", "nm", "N", "NM", "STAGE_LEVEL")

  returnValue = strm[keep]
  rownames(returnValue) <- seq(length=nrow(returnValue))

  return(returnValue)

}

## Domain level density per SSU
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the stratum level
## @param x
## A data.frame which is the output from the strat_density function
## @inheritParams strat_density
## @return A data.frame with density per SSU by species, its variance (var),
## the number of SSUs in the domain (nm), the number of PSUs in the domain (n), the total
## possible number of SSUs (NM), and the total possible number of PSUs (N)
domain_density = function(x, ntot){

  ## Use ntot data.frame to calculate weighting
  merged = .getWeight(x, ntot)
  ## Return weighted statistics
  by = .aggBy("domain")

  library(dplyr)
  strm =  merged %>%
    group_by(.dots=by) %>%
    summarise(
      STAGE_LEVEL = mean(STAGE_LEVEL),
      density = sum(wh*density),
      var = sum(wh^2*var, na.rm = TRUE),
      n = sum(n),
      nm = ifelse(mean(STAGE_LEVEL) == 1,
                  NA,
                  sum(nm)
      ),
      N = sum(N),
      NM = sum(NM)) %>%
    as.data.frame()

  keep = c("YEAR", "REGION", "SPECIES_CD", "density", "var", "n", "nm", "N", "NM", "STAGE_LEVEL")

  returnValue = strm[keep]
  rownames(returnValue) <- seq(length=nrow(returnValue))

  return(returnValue)
}
