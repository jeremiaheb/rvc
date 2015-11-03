## Length frequency functions

#' Stratum level lenth frequencies
#' @export
#' @description
#' Calculates length frequency at each stratum
#' @param x
#' A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
#' STATION_NR, NUM, and LEN
#' @inheritParams strat_density
#' @return
#' A data.frame with length frequencies for each stratum
#' @details
#' The LEN column should be a numeric column, but the lengths need not
#' be the original measured lengths. If they are binned, the LEN column
#' should be transfomed to represent the midpoint of each bin.
strat_length_frequency = function(x, ntot){
  ## Filter data to include only where individuals are present
  x = count_filter(x, when_present = TRUE)
  ## Get the summarize function from plyr
  summarize = get('summarize', asNamespace('plyr'));
  ## Merge x and ntot
  merged1 = merge(x, ntot)
  ## Calculate the stratum level statistics
  total = plyr::ddply(merged1, .aggBy('strat'), summarize,
                      TOT = sum(NUM)
                      )
  ## Merge original data with total
  merged2 = merge(x, total)
  ## Calculate stratum + length level statistics
  out = plyr::ddply(merged2, c(.aggBy('strat'), 'LEN'), summarize,
                frequency = sum(NUM)/mean(TOT)
        )
  ## Rename LEN column to length_class
  names(out)[names(out) == "LEN"] = "length_class"

  return(out)
}

#' Domain level length frequency
#' @export
#' @description
#' Calculates the length frequecy at each sampling domain
#' @param x
#' A data.frame which is the output of strat_length_frequency
#' @inheritParams strat_density
#' @return
#' A data.frame with: length frequencies for each stratum, the number of PSUs
#' per domain (n), the number of SSUs per domain (nm), the number of
#' possible PSUs per domain (N), and the number of possible SSUs per domain (NM)
domain_length_frequency = function(x, ntot){
  ## Filter out strata in ntot that are not present in x
  ntot = .with_strata(ntot, x)
  ## Add weighting
  weighted = .getWeight(x, ntot)
  ## Get summarize from plyr package
  summarize = get('summarize', asNamespace('plyr'))
  ## Calculate frequency
  return(plyr::ddply(weighted, c(.aggBy('domain'), 'length_class'), summarize, frequency = sum(wh*frequency)))
}

