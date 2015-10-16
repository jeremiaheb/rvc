#' SSU Occurrence
#' @export
#' @description
#' Whether or not a particular species is present in a secondary sampling unit
#' @inheritParams ssu_density
#' @return A data.frame with a column, occurrence, indicating whether or not
#' a species was present in a particular secondary sampling unit
ssu_occurrence = function(x) {
  ## Get the variables by which to aggregate the data
  by = .aggBy("ssu")
  ## Get summarize function from plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  ## Aggregate and return data
  return(plyr::ddply(x, by, summarize,
                     occurrence = ifelse(sum(NUM)>0,1,0)
                     ))
}

