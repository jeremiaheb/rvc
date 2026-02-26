# 1. Load the original single-year data
load('../test_data.Rdata')

rvc_data = list(
  sample_data = fk2012,
  stratum_data = ntot2012,
  taxonomic_data = taxonomic_data
)

# 2. Load the multi-year, mixed-design data
load('../test_data_multi.Rdata')

# --- SINGLE-YEAR FILTERING TESTS (rvc_data) ---

test_that("getDomainOccurrence handles Base Case exactly as baseline", {
  res = getDomainOccurrence(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainOccurrence handles Kitchen Sink Filtering exactly as baseline", {
  res = getDomainOccurrence(
    rvc_data, 
    species = "EPI MORI", 
    years = 2012, 
    is_protected = FALSE, 
    length_bins = 20, 
    when_present = TRUE
  )
  expect_snapshot(res)
})

test_that("getDomainOccurrence handles Split Protected Status exactly as baseline", {
  res = getDomainOccurrence(rvc_data, species = "EPI MORI", merge_protected = FALSE)
  expect_snapshot(res)
})

test_that("getStratumOccurrence handles custom species groups exactly as baseline", {
  group_lookup = data.frame(
    species = c("EPI MORI", "MYC BONA"),
    group = rep("Groupers", 2)
  )
  res = getStratumOccurrence(rvc_data, species = c("EPI MORI", "MYC BONA"), group = group_lookup)
  expect_snapshot(res)
})

# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Occurrence wrappers correctly calculate mixed designs across multiple years", {
  
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # 1. Domain Occurrence
  res_domain = getDomainOccurrence(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_domain)
  
  # 2. Stratum Occurrence
  res_strat = getStratumOccurrence(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_strat)
})