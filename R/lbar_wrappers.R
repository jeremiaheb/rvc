






getDomainLbar = function(x, species, length_bins = NULL, merge_protected = TRUE, ...) {
  ## Summary statistics function
  f = function(sample_data, stratum_data, ...){
    domain_lbar(strat_lbar(psu_lbar(species_group(ssu_lbar(sample_data), ...)), stratum_data), stratum_data)
  }

  ## Wrap the function
  out = .wrapperProto(x, species, length_bins, merge_protected, getDomainLbar, f, ...)

  return(out)
}
