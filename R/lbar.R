#' @importFrom dplyr group_by summarize across all_of
#' @importFrom rlang .data

## SSU mean length
## @export
## @description
## Calculates mean length in a secondary sampling unit
## @return A data.frame with columns for the sum of counts (x) and the sum of lengths times counts (y) per SSU.
ssu_lbar = function(x) {
  by = .aggBy("ssu")
  
  res <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(
      x = sum(.data$NUM, na.rm = TRUE),
      y = sum(.data$LEN * .data$NUM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
    
  return(res)
}

#' PSU mean length
#' @export
#' @description
#' Mean length at the primary sampling unit level.
#' @param x
#' A data.frame which is the output of \code{ssu_lbar}.
#' @return A list containing two data.frames: 
#' \itemize{
#'   \item \code{ssu_dat}: The original SSU-level data passed to the function.
#'   \item \code{psu_dat}: A data.frame of PSU-level summaries including the number of SSUs sampled (\code{m}), variance of the sum of counts (\code{var}), mean sum of counts (\code{xi}), and mean sum of lengths (\code{yi}).
#' }
#' @importFrom dplyr group_by summarize n across all_of
#' @importFrom stats var
#' @importFrom rlang .data
psu_lbar = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("psu")
  
  ## Aggregate and return the data
  a <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarize(
      m = dplyr::n(), # Replaces length(STATION_NR)
      var = stats::var(.data$x),
      xi = mean(.data$x),
      yi = mean(.data$y),
      .groups = "drop"
    ) %>%
    as.data.frame()

  return(list(ssu_dat = x, psu_dat = a))
}

#' Stratum mean length
#' @export
#' @description
#' Mean length at the stratum level.
#' @param x
#' A list containing SSU and PSU data, which is the output of \code{psu_lbar}.
#' @param ntot
#' A data.frame of strata information.
#' @return A list containing three data.frames: 
#' \itemize{
#'   \item \code{strat_dat}: Stratum-level summaries including mean sum of counts (\code{xi}), mean sum of lengths (\code{yi}), and sampling unit counts (\code{n}, \code{nm}).
#'   \item \code{psu_dat}: The PSU-level data passed from the previous step.
#'   \item \code{ssu_dat}: The SSU-level data passed from the previous step.
#' }
#' @importFrom dplyr select inner_join
#' @importFrom magrittr %>%
strat_lbar = function(x, ntot) {
  
  # Calculate stratum level stats using the existing density wrapper
  # We use quotes around column names in select() for strict CRAN compliance
  avg_sum_num <- .wrapFunction(x$psu_dat, "xi", "density", strat_density, ntot) %>%
    dplyr::select("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "xi")
    
  avg_sum_len <- .wrapFunction(x$psu_dat, "yi", "density", strat_density, ntot) %>%
    dplyr::select("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD", "yi", "n", "nm", "STAGE_LEVEL")
  
  # Join them together using dplyr instead of base merge
  a <- dplyr::inner_join(
    avg_sum_num, 
    avg_sum_len, 
    by = c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD")
  ) %>%
    as.data.frame()
    
  # Pass SSU, PSU, and Stratum data forward to the domain level
  return(list(strat_dat = a, psu_dat = x$psu_dat, ssu_dat = x$ssu_dat))
}

#' Domain mean length
#' @export
#' @description
#' Mean length (cm) at sampling domain level.
#' @param x
#' A list containing SSU, PSU, and Stratum data, which is the output of \code{strat_lbar}.
#' @param ntot
#' A data.frame of strata information.
#' @return A data.frame with columns: Lbar (mean length), vbar_L (mean variance of Lbar), 
#' n (number of sampled primary sampling units), nm (number of sampled secondary sampling units), 
#' and STAGE_LEVEL.
#' @importFrom dplyr group_by summarise mutate select left_join inner_join n_distinct across all_of
#' @importFrom rlang .data
domain_lbar = function(x, ntot) {
  ## Use ntot data.frame to calculate weighting
  merged = .getWeight(x$strat_dat, ntot)
  by_domain = .aggBy("domain")

  # 1. Calculate the Domain-level Mean Length (Lbar)
  strm = merged %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_domain))) %>%
    dplyr::summarise(
      STAGE_LEVEL = mean(.data$STAGE_LEVEL),
      Xbar = sum(.data$wh * .data$xi),
      Ybar = sum(.data$wh * .data$yi),
      n = sum(.data$n),
      nm = ifelse(mean(.data$STAGE_LEVEL) == 1, NA_real_, sum(.data$nm)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Lbar = .data$Ybar / .data$Xbar) %>%
    as.data.frame()

  # 2. Calculate the PSU-level variance residuals
  psu_var <- x$ssu_dat %>%
    dplyr::left_join(
      strm %>% dplyr::select("YEAR", "REGION", "SPECIES_CD", "Lbar"), 
      by = c("YEAR", "REGION", "SPECIES_CD")
    ) %>%
    dplyr::mutate(
      e = .data$y - .data$Lbar * .data$x,
      esq = .data$e * .data$e
    ) %>%
    dplyr::group_by(dplyr::across(c("YEAR", "REGION", "STRAT", "PROT", "PRIMARY_SAMPLE_UNIT", "SPECIES_CD"))) %>%
    dplyr::summarise(
      Lbar = mean(.data$Lbar),
      xi = mean(.data$x),
      yi = mean(.data$y),
      mean1 = mean(.data$esq),
      e2 = sum(.data$esq),
      m = dplyr::n_distinct(.data$STATION_NR),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      np_freq = ifelse(.data$m > 1, 1, 0),
      vari = ifelse(.data$m == 1, 0, .data$e2 / (.data$m - 1)),
      ei = .data$yi - .data$Lbar * .data$xi,
      eisq = .data$ei^2
    ) 

  # 3. Calculate the Stratum-level variance residuals
  strat_var <- psu_var %>%
    dplyr::group_by(dplyr::across(c("YEAR", "REGION", "STRAT", "PROT", "SPECIES_CD"))) %>%
    dplyr::summarise(
      av_x = mean(.data$xi),
      mean1 = mean(.data$eisq),
      mean2 = mean(.data$vari),
      nm = sum(.data$m),
      m = mean(.data$m),
      n = dplyr::n_distinct(.data$PRIMARY_SAMPLE_UNIT),
      mean3 = mean(.data$np_freq),
      sum1 = sum(.data$xi),
      ei2 = sum(.data$eisq),
      varm = sum(.data$vari),
      np = sum(.data$np_freq),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n = ifelse(.data$n < 2, 2, .data$n),
      s1 = .data$ei2 / (.data$n - 1),
      s2 = ifelse(.data$n > 0, .data$varm / .data$n, 0)
    )

  # 4. Apply weighting to get the final Domain Variance (vbar_L)
  wt <- strat_var %>%
    dplyr::left_join(.getWeight(ntot, ntot), by = c("YEAR", "REGION", "STRAT", "PROT")) %>%
    dplyr::mutate(
      fn = .data$n / .data$NTOT,
      fm = .data$m / ((.data$GRID_SIZE^2) / (pi * 7.5^2)),
      vbar_lh = ifelse(.data$av_x == 0, 0,
                  ifelse(.data$STAGE_LEVEL == 2,  
                         (1 / (.data$av_x^2)) * (((1 - .data$fn) * .data$s1 / .data$n) + ((.data$fn * (1 - .data$fm) * .data$s2) / .data$nm)),
                         (1 / (.data$av_x^2)) * (((1 - .data$fn) * .data$s1 / .data$n))
                  )
      ),
      wvbar = (.data$wh^2) * .data$vbar_lh
    ) %>%
    dplyr::group_by(dplyr::across(c("YEAR", "REGION", "SPECIES_CD"))) %>%
    dplyr::summarise(vbar_L = sum(.data$wvbar), .groups = "drop") %>%
    as.data.frame()

  # 5. Bring it all together and format output
  out <- dplyr::inner_join(strm, wt, by = c("YEAR", "REGION", "SPECIES_CD")) %>%
    dplyr::select("YEAR", "REGION", "SPECIES_CD", "Lbar", "vbar_L", "n", "nm", "STAGE_LEVEL")

  return(out)
}
