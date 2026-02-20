## SSU density
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the secondary sampling unit level
## @param x
## A data.frame including columns: REGION, YEAR, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
## STATION_NR, SPECIES_CD, NUM
## @return A data.frame with density per SSU for each SSU
#' @importFrom magrittr %>%
#' @importFrom rlang .data
ssu_density = function(x){
  ## variables to aggregate by
  by = .aggBy("ssu")
  
  ## aggregate by SSU and sum count using dplyr
  res = x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(density = sum(.data$NUM), .groups = "drop") %>%
    as.data.frame()
    
  return(res)
}

## PSU density per SSU
## @export
## @description Number of individuals per secondary sampling unit (~177m^2) calculated
## for each species at the primary sampling unit level
## @param x
## A data.frame of the output from ssu_density
## @return A data.frame with density per SSU aggregated by PSU and species,
## variance in density (var), and the number of ssus per psu (m)
#' @importFrom dplyr group_by summarize n
#' @importFrom rlang .data
psu_density = function(x) {
  ## variables to aggregate by
  by = .aggBy("psu")
  
  res = x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(
      m = length(.data$STATION_NR),
      var = stats::var(.data$density),
      density = sum(.data$density) / length(.data$STATION_NR),
      .groups = "drop"
    ) %>%
    as.data.frame()
    
  return(res)
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
#' @importFrom dplyr group_by summarise n across all_of
#' @importFrom stats var
#' @importFrom rlang .data
strat_density <- function(x, ntot) {

  ## merge with ntot data (Base R merge is perfect here for backward compatibility)
  merged = merge(x, ntot)
  
  ## Aggregate by variables
  by = .aggBy("strat")

  strm = merged %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      STAGE_LEVEL = mean(.data$STAGE_LEVEL),
      n = dplyr::n(),
      nm = ifelse(mean(.data$STAGE_LEVEL) == 1, 
                  NA_real_, 
                  sum(.data$m)),
      
      # The math here is expanded directly to avoid sequential dependencies in summarise()
      var = ifelse(mean(.data$STAGE_LEVEL) == 1,
                   (1 - (dplyr::n() / mean(.data$NTOT))) * (stats::var(.data$density) / dplyr::n()),
                   (1 - (dplyr::n() / mean(.data$NTOT))) * (stats::var(.data$density) / dplyr::n()) + 
                     ((dplyr::n() / mean(.data$NTOT)) * (1 - (mean(.data$m) / (mean(.data$GRID_SIZE)^2 / (pi * 7.5^2)))) * (sum(.data$var, na.rm = TRUE) / sum(ifelse(.data$m > 1, 1, 0)))) / sum(.data$m)
      ),
      density = mean(.data$density),
      N = mean(.data$NTOT),
      NM = (mean(.data$GRID_SIZE)^2 / (pi * 7.5^2)) * mean(.data$NTOT),
      .groups = "drop"
    ) %>%
    as.data.frame()

  keep = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "density", "var", "n", "nm", "N", "NM", "STAGE_LEVEL")

  returnValue = strm[keep]
  rownames(returnValue) <- seq(length = nrow(returnValue))

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
#' @importFrom dplyr group_by summarise across all_of
#' @importFrom rlang .data
domain_density = function(x, ntot){

  ## Use ntot data.frame to calculate weighting
  merged = .getWeight(x, ntot)
  ## Return weighted statistics
  by = .aggBy("domain")

  strm =  merged %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      STAGE_LEVEL = mean(.data$STAGE_LEVEL),
      density = sum(.data$wh * .data$density),
      var = sum((.data$wh^2) * .data$var, na.rm = TRUE),
      n = sum(.data$n),
      nm = ifelse(mean(.data$STAGE_LEVEL) == 1,
                  NA_real_,
                  sum(.data$nm)
      ),
      N = sum(.data$N),
      NM = sum(.data$NM),
      .groups = "drop"
    ) %>%
    as.data.frame()

  keep = c("YEAR", "REGION", "SPECIES_CD", "density", "var", "n", "nm", "N", "NM", "STAGE_LEVEL")

  returnValue = strm[keep]
  rownames(returnValue) <- seq(length=nrow(returnValue))

  return(returnValue)
}
