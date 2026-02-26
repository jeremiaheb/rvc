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

test_that("getDomainLengthFrequency handles Base Case exactly as baseline", {
  res = getDomainLengthFrequency(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainLengthFrequency handles Sequence Length Bins exactly as baseline", {
  res = getDomainLengthFrequency(
    rvc_data, 
    species = "EPI MORI", 
    length_bins = seq(0, 100, 10) # Testing the cut() functionality
  )
  expect_snapshot(res)
})

test_that("getDomainLengthFrequency handles Split Protected Status exactly as baseline", {
  res = getDomainLengthFrequency(rvc_data, species = "EPI MORI", merge_protected = FALSE)
  expect_snapshot(res)
})

test_that("getDomainLengthFrequency throws error if length_bins is not numeric", {
  expect_error(getDomainLengthFrequency(rvc_data, species = "EPI MORI", length_bins = "LC"))
})


# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Length Frequency wrappers correctly calculate mixed designs across multiple years", {
  
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # 1. Domain Length Frequency
  res_domain = getDomainLengthFrequency(multi_year_data, species = "EPI MORI", length_bins = c(0, 20, 40, 60))
  expect_snapshot(res_domain)

  # 2. Domain Length Frequency
  res_domain_2 = getDomainLengthFrequency(multi_year_data, species = "OCY CHRY", years = 2022)
  expect_snapshot(res_domain_2)
  
  # 3. Stratum Length Frequency
  res_strat = getStratumLengthFrequency(multi_year_data, species = "EPI MORI", length_bins = c(0, 20, 40, 60))
  expect_snapshot(res_strat)
})