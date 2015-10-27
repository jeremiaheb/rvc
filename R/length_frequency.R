## Length frequency functions

#' Stratum level lenth frequencies
#' @export
#' @description
#' Calculates the frequency of counts at each length.
#' @param x
#' A data.frame containing columns: YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT,
#' STATION_NR, NUM, and LEN
#' @inheritParams strat_density
#' @return
#' A data.frame with: length frequencies for each stratum, the number of PSUs
#' per stratum (n), the number of SSUs per stratum (nm), the number of
#' possible PSUs per stratum (N), and the number of possible SSUs per stratum (NM)
#' @details
#' The LEN column should be a numeric column, but the lengths need not
#' be the original measured lengths. If they are binned, the LEN column
#' should be transfomed to represent the midpoint of each bin.
strat_length_frequency = function(x, ntot){
  ## Get the summarize function from plyr
  summarize = get('summarize', asNamespace('plyr'));
  ## Merge x and ntot
  merged1 = merge(x, ntot)
  ## Calculate the stratum level statistics
  total = plyr::ddply(merged1, .aggBy('strat'), summarize,
                      n = length(unique(PRIMARY_SAMPLE_UNIT)),
                      nm = nrow(unique(cbind(PRIMARY_SAMPLE_UNIT, STATION_NR))),
                      N = mean(NTOT),
                      NM = floor(N * mean(GRID_SIZE)^2/(7.5^2*pi)),
                      TOT = sum(NUM)
                      )
  ## Merge original data with total
  merged2 = merge(x, total)
  ## Calculate stratum + length level statistics
  return(
    plyr::ddply(merged2, c(.aggBy('strat'), 'LEN'), summarize,
                frequency = sum(NUM)/mean(TOT),
                n = n[1],
                nm = nm[1],
                N = N[1],
                NM = NM[1]
                )
  )
}


