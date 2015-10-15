#' Calculates SSU level density per ssu
#' @export
#' @description Sums the count per species at the SSU level
#' @param x
#' A data.frame including columns REGION, YEAR, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
#' STATION_NR, SPECIES_CD, NUM, and LEN (if by_length = TRUE)
#' @return A data.frame with counts aggregated by SSU, SPECIES_CD
ssu_density = function(x){
  ## variables to aggregate by
  by = .aggBy("ssu")
  ## Get summarize fn from plyr package
  summarize = get("summarize", asNamespace("plyr"));
  ## aggregate by SSU and sum count
  return(plyr::ddply(x, by, summarize, density = sum(NUM)))
}

#' Calculates PSU level density per SSU
#' @export
#' @description
#' Calculates the PSU level density per SSU, its variance (var), and the number
#' of SSUs per PSU (m)
#' @param x
#'  A data.frame including columns REGION, YEAR, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
#' SPECIES_CD, NUM
#' @return A data.frame with density per SSU aggregated by PSU, SPECIES_CD
#' variance in density (var), and the number of ssus per psu (m)
psu_density = function(x) {
  ## variables to aggregate by
  by = .aggBy("psu")
  ## get summarize fn from plyr package
  summarize = get("summarize", asNamespace("plyr"))
  ## SSU level density
  d = ssu_density(x)
  ## Aggregate by PSU, sum density and divide by number of SSUs
  return(plyr::ddply(d, by, summarize,
                     m = length(STATION_NR),
                     var = var(density),
                     density = sum(density)/length(STATION_NR)
                     ))
}

#' Calculates the Stratum level density per SSU
#' @export
#' @description
#' Calculates the Stratum level density per SSU, its variance (var), the number of SSUs per
#' stratum (nm), and the number of PSUs per stratum (n)
#' @param x
#' A data.frame including columns REGION, YEAR, STRAT, PROT, SPECIES_CD, NUM
#' @param ntot
#' A data.frame including columns REGION, YEAR, STRAT, PROT, NTOT, GRID_SIZE
#' @return A data.frame with density per SSU aggregated by stratum, its variance (var),
#' the number of SSUs per stratum (nm), the number of PSUs per stratum (n), the total
#' possible number of SSUs (NM), and the total possible number of PSUs (N)
strat_density <- function(x, ntot) {
  ## Calculate psu level density and variance
  d = psu_density(x)
  ## merge with ntot data
  merged = merge(d, ntot)
  ## Aggregate by variables
  by = .aggBy("strat")
  ## Get the summarize function from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Calculate a whole series of statistics while aggregating by stratum
  strm = plyr::ddply(merged, summarize,
                     v1 = var(density), # Between PSU variance
                     v2 = sum(var, na.rm = TRUE), #Between SSU variance
                     mtot = GRID_SIZE/(pi*7.5^2),
                     mbar = mean(m),
                     n = length(PRIMARY_SAMPLE_UNIT),
                     nm = sum(m),
                     fn = n/mtot,
                     fm = mbar/mtot,
                     var = (1-fn)*v1/n + (fn*(1-fm)*v2)/nm
  )
  ## TODO: Finish strat_density function and write test

}



## Helper function to produce by variables
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