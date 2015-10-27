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
  ## Drop strata where species not seen
  total = subset(total, TOT > 0)
  ## Merge original data with total
  merged2 = merge(x, total)
  ## Calculate stratum + length level statistics
  out = plyr::ddply(merged2, c(.aggBy('strat'), 'LEN'), summarize,
                frequency = sum(NUM)/mean(TOT),
                n = n[1],
                nm = nm[1],
                N = N[1],
                NM = NM[1]
        )
  ## Drop zero length artifacts and return
  return(subset(out, LEN > 0))
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
  ## Filter out strata where species not observed
  ## TODO: This is an ugly implemtation of subsetting by a combination
  ## of factors, find a better implementation
  keep = apply(unique(x[c("YEAR", "REGION", "STRAT", "PROT")]),1,paste, collapse="")
  available = apply(ntot[c("YEAR", "REGION", "STRAT", "PROT")], 1, paste, collapse="")
  ntot = ntot[available %in% keep,]
  ## Add weighting
  weighted = .getWeight(x, ntot)
  ## Calculate n, nm, N and NM
  summarize = get('summarize', asNamespace('plyr'))
  dat1 = plyr::ddply(unique(weighted[c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "n", "nm", "N", "NM")]),
                     .aggBy('domain'), summarize, n = sum(n), nm = sum(nm), N = sum(N), NM = sum(NM))
  ## Calculate frequency
  dat2 = plyr::ddply(weighted, c(.aggBy('domain'), 'LEN'), summarize, frequency = sum(wh*frequency))
  ## Return merged data
  return(merge(dat2, dat1))
}

