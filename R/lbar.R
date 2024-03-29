## SSU mean length
## @export
## @description
## Calculates mean length in a secondary sampling unit
## @return A data.frame with a column, num_sum at each num_len.
ssu_lbar = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("ssu")
  ## Get summarize function from plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Aggregate and return data
  return(plyr::ddply(x, by, summarize,
                     num_sum = sum(NUM),
                     num_len = sum(LEN*NUM)
  ))
}

## PSU mean length
## @export
## @description
## Mean length at the primary sampling unit level
## @param x
## A data.frame which is the output of ssu_lbar
## @return A data.frame with columns, num_sum_avg at each len_sum_avg for a primary sampling unit,
## sampled per primary sampling unit (m)
psu_lbar = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("psu")
  ## Get the summarize function from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Aggregate and return the data
  return(plyr::ddply(x, by, summarize,
                     m = length(STATION_NR),
                     num_sum_avg = mean(num_sum),
                     len_sum_avg = mean(num_len)

  ))
}

## Stratum mean length
## @export
## @description
## Mean length at the stratum level
## @param x
## A data.frame which is the output of psu_lbar
## @return 2 data.frames 1) strat data with columns: num_sum_avg (mean at each strata), len_sum_avg (mean at each strata),
##n, the number of sampled primary sampling units; 2) returns output from psu_lbar to pass to domain level
strat_lbar = function(x, ntot) {
  ## Wrap the domain_density function, renaming the occurrence column to/from density
  # return(.wrapFunction(x, "num_sum","density", strat_density, ntot))
  avg_sum_num <- .wrapFunction(x, "num_sum_avg","density", strat_density, ntot) %>%
                  select(YEAR, REGION, STRAT, PROT, SPECIES_CD, num_sum_avg)
  avg_sum_len <- .wrapFunction(x, "len_sum_avg","density", strat_density, ntot) %>%
                  select(YEAR, REGION, STRAT, PROT, SPECIES_CD, len_sum_avg, n, STAGE_LEVEL)

  a <- merge(avg_sum_num, avg_sum_len, by = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD"))
  return(list(strat_dat = a, psu_dat = x))

}

## Domain mean length
## @export
## @description
## Mean length (cm) at sampling domain level
## @param x
## A data.frame which is the output of strat_lbar
## @inheritParams strat_lbar
## @return A data.frame with columns: Lbar, mean length; vbar_L, the
## mean variance of Lbar; n, the number of sampled primary sampling units;
## nm, the number of sampled secondary sampling units; N, the number of possible
## primary sampling units
domain_lbar = function(x, ntot) {
  ## Use ntot data.frame to calculate weighting
  merged = .getWeight(x$strat_dat, ntot)
  ## Return weighted statistics
  by = .aggBy("domain")
  strm =  merged %>%
    group_by(.dots=by) %>%
    summarise(
      STAGE_LEVEL = mean(STAGE_LEVEL),
      Xbar = sum(wh*num_sum_avg),
      Ybar = sum(wh*len_sum_avg),
      n = sum(n),
      nm = ifelse(mean(STAGE_LEVEL) == 1,
                  NA,
                  sum(nm)
      )) %>%
    mutate(Lbar = Ybar / Xbar) %>%
    as.data.frame()
  keep = c("YEAR", "REGION", "SPECIES_CD", "Xbar", "Ybar", "Lbar", "n", "nm", "STAGE_LEVEL")

  returnValue = strm[keep]
  rownames(returnValue) <- seq(length=nrow(returnValue))

  var_Lh <- x$psu_dat %>%
    merge(., strm[c("YEAR", "REGION","SPECIES_CD", "Lbar")], by = c("YEAR", "REGION","SPECIES_CD")) %>%
    mutate(e2 = (len_sum_avg - Lbar*num_sum_avg)^2) %>%
    group_by(YEAR, REGION, STRAT, PROT, SPECIES_CD) %>%
    summarise(e2sum = sum(e2), nv = n_distinct(PRIMARY_SAMPLE_UNIT)) %>%
    mutate(svar = ifelse(!nv ==1, e2sum/(nv - 1), e2sum/nv)) %>%
    merge(., ntot) %>%
    mutate(f = nv / NTOT) %>%
    merge(., x$strat_dat[c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "num_sum_avg")]) %>%
    merge(., merged[c("YEAR", "REGION", "STRAT", "PROT", "wh")]) %>%
    mutate(vbar_Lh = ifelse(num_sum_avg > 0, (1/num_sum_avg^2)*(1-f)*(svar/nv),0)) %>%
    mutate(wvbar = wh^2 * vbar_Lh) %>%
    as.data.frame()

  vbar_L <- var_Lh %>%
    group_by(YEAR, REGION, SPECIES_CD) %>%
    summarise(vbar_L = sum(wvbar)) %>%
    as.data.frame()

  out <-  merge(strm, vbar_L, by = c("YEAR", "REGION", "SPECIES_CD")) %>%
    select(YEAR, REGION, SPECIES_CD, Lbar, vbar_L, n,nm,STAGE_LEVEL)

  return(out)
}
