## Length frequency functions

## Stratum level lenth frequencies
## @export
## @description
## Calculates length frequency at each stratum
## @param x
## A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
## STATION_NR, NUM, and LEN
## @inheritParams strat_density
## @return
## A data.frame with length frequencies for each stratum
## @details
## The LEN column should be a numeric column, but the lengths need not
## be the original measured lengths. If they are binned, the LEN column
## should be transfomed to represent the midpoint of each bin.
#' @importFrom dplyr group_by mutate summarize rename across all_of
#' @importFrom magrittr %>%
#' @importFrom rlang .data

strat_length_frequency = function(x, ntot){
  ## Filter data to include only where individuals are present
  x = count_filter(x, when_present = TRUE)
  
  merged1 = merge(x, ntot)
  
  by_strat = .aggBy('strat')
  by_strat_len = c(by_strat, "LEN")
  
  ## Calculate stratum + length level statistics
  out = merged1 %>%
    # First, group by stratum and calculate total sum (TOT)
    dplyr::group_by(dplyr::across(dplyr::all_of(by_strat))) %>%
    dplyr::mutate(TOT = sum(.data$NUM)) %>%
    # Then, regroup to include length class and calculate frequency
    dplyr::group_by(dplyr::across(dplyr::all_of(by_strat_len))) %>%
    dplyr::summarize(
      frequency = sum(.data$NUM) / mean(.data$TOT), 
      .groups = "drop"
    ) %>%
    dplyr::rename(length_class = "LEN") %>%
    as.data.frame()

  return(out)
}

## Domain level length frequency
## @export
## @description
## Calculates the length frequecy at each sampling domain
## @param x
## A data.frame which is the output of strat_length_frequency
## @inheritParams strat_density
## @return
## A data.frame with: length frequencies for each stratum, the number of PSUs
## per domain (n), the number of SSUs per domain (nm), the number of
## possible PSUs per domain (N), and the number of possible SSUs per domain (NM)
domain_length_frequency = function(x, ntot){
  ## Filter out strata in ntot that are not present in x
  ntot = .with_strata(ntot, x)
  
  ## Add weighting
  weighted = .getWeight(x, ntot)
  
  by_domain_len = c(.aggBy('domain'), 'length_class')
  
  ## Calculate frequency
  out = weighted %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_domain_len))) %>%
    dplyr::summarize(
      frequency = sum(.data$wh * .data$frequency),
      .groups = "drop"
    ) %>%
    as.data.frame()
    
  return(out)
}

