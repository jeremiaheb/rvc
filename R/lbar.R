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
                     x = sum(NUM),
                     y = sum(LEN*NUM)
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
  a <- plyr::ddply(x, by, summarize,
                     m = length(STATION_NR),
                     var = var(x),
                     xi = mean(x),
                     yi = mean(y))

  return(list(ssu_dat = x, psu_dat = a))
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
  avg_sum_num <- .wrapFunction(x$psu_dat, "xi","density", strat_density, ntot) %>%
                  dplyr::select(YEAR, REGION, STRAT, PROT, SPECIES_CD, xi)
  avg_sum_len <- .wrapFunction(x$psu_dat, "yi","density", strat_density, ntot) %>%
                  dplyr::select(YEAR, REGION, STRAT, PROT, SPECIES_CD, yi, n,nm, STAGE_LEVEL)
  a <- merge(avg_sum_num, avg_sum_len, by = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD"))
  return(list(strat_dat = a, psu_dat = x$psu_dat, ssu_dat = x$ssu_dat))

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
      Xbar = sum(wh*xi),
      Ybar = sum(wh*yi),
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

  # var_Lh <- x$psu_dat %>%
  #   left_join(strm %>% select(YEAR, REGION, SPECIES_CD, STAGE_LEVEL, Lbar)) %>%
  #   mutate(e2 = (yi - Lbar*xi)^2) %>%
  #   group_by(YEAR, REGION, STRAT, PROT, SPECIES_CD) %>%
  #   summarise(e2sum = sum(e2), nv = n_distinct(PRIMARY_SAMPLE_UNIT)) %>%
  #   mutate(svar = ifelse(!nv ==1, e2sum/(nv - 1), e2sum/nv)) %>%
  #   merge(., ntot) %>%
  #   mutate(f = nv / NTOT) %>%
  #   left_join(x$strat_dat %>% select(YEAR, REGION, STRAT, PROT, SPECIES_CD, xi)) %>%
  #   left_join(merged %>% select(YEAR, REGION, STRAT, PROT, wh)) %>%
  #   mutate(vbar_Lh = ifelse(xi > 0, (1/xi^2)*(1-f)*(svar/nv),0)) %>%
  #   mutate(wvbar = wh^2 * vbar_Lh) %>%
  #   as.data.frame()
  # vbar_L <- var_Lh %>%
  #   group_by(YEAR, REGION, SPECIES_CD) %>%
  #   summarise(vbar_L = sum(wvbar)) %>%
  #   as.data.frame()


  psu_var <- x$ssu_dat %>%
    left_join(strm %>% select(YEAR, REGION, SPECIES_CD, Lbar), by = c("YEAR", "REGION", "SPECIES_CD")) %>%
    mutate(e = y - Lbar*x) %>%
    mutate(esq = e*e) %>%
    group_by(YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT, SPECIES_CD) %>%
    summarise(Lbar = mean(Lbar),
              xi = mean(x),
              yi = mean(y),
              mean1 = mean(esq),
              e2 = sum(esq),
              m = n_distinct(STATION_NR)) %>%
    mutate(np_freq = ifelse(m>1, 1, 0),
           vari = ifelse(m==1,0, e2/(m-1)),
           ei = yi - Lbar*xi) %>%
    mutate(eisq = ei^2) %>%
    ungroup()

  strat_var <- psu_var %>%
    group_by(YEAR, REGION, STRAT, PROT, SPECIES_CD) %>%
    summarise(av_x = mean(xi),
              mean1 = mean(eisq),
              mean2 = mean(vari),
              nm = sum(m),
              m = mean(m),
              n = n_distinct(PRIMARY_SAMPLE_UNIT),
              mean3 = mean(np_freq),
              sum1 = sum(xi),
              ei2 = sum(eisq),
              varm = sum(vari),
              np = sum(np_freq)) %>%
    ungroup() %>%
    mutate(n = if_else(n < 2, 2, n),
           s1 = ei2/(n-1),
           s2 = ifelse(n > 0, varm/n, 0))

  wt <- strat_var %>%
    left_join(.getWeight(ntot, ntot), by = c("YEAR", "REGION", "STRAT", "PROT")) %>%
    mutate(fn = n/NTOT,
           fm = m/ ((GRID_SIZE^2) / (pi*7.5^2)),
           vbar_lh = if_else(av_x == 0, 0,
                             if_else(STAGE_LEVEL == 2,  (1/(av_x**2))*(((1-fn)*s1/n)+((fn*(1-fm)*s2)/nm)),
                                     (1/(av_x**2))*(((1-fn)*s1/n)))
           )
    ) %>%
    mutate(wvbar = wh^2 * vbar_lh) %>%
    group_by(YEAR, REGION, SPECIES_CD) %>%
      summarise(vbar_L = sum(wvbar)) %>%
    as.data.frame()

  out <-  merge(strm, wt, by = c("YEAR", "REGION", "SPECIES_CD")) %>%
    select(YEAR, REGION, SPECIES_CD, Lbar, vbar_L, n,nm,STAGE_LEVEL)

  return(out)
}
