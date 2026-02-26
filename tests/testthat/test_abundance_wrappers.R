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

test_that("getDomainAbundance handles Base Case exactly as baseline", {
  res = getDomainAbundance(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainAbundance handles Kitchen Sink Filtering exactly as baseline", {
  res = getDomainAbundance(
    rvc_data, 
    species = "HAE PLUM", 
    years = 2012, 
    merge_protected = FALSE, 
    length_bins = 10
  )
  expect_snapshot(res)
})

test_that("getDomainAbundance handles Split Protected Status exactly as baseline", {
  res = getDomainAbundance(rvc_data, species = "EPI MORI", merge_protected = FALSE)
  expect_snapshot(res)
})

test_that("getStratumAbundance handles custom species groups exactly as baseline", {
  group_lookup = data.frame(
    species = c("LUT GRIS", "OCY CHRY"),
    group = rep("Snappers", 2)
  )
  res = getStratumAbundance(rvc_data, species = c("LUT GRIS", "OCY CHRY"), group = group_lookup)
  expect_snapshot(res)
})

# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Abundance wrappers correctly calculate mixed designs across multiple years", {
  
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # 1. Domain Abundance
  res_domain = getDomainAbundance(multi_year_data, species = "THA BIFA")
  expect_snapshot(res_domain)
  
  # 2. Stratum Abundance
  res_strat = getStratumAbundance(multi_year_data, species = "THA BIFA")
  expect_snapshot(res_strat)
  
  # 3. PSU Abundance
  res_psu = getPSUAbundance(multi_year_data, species = "THA BIFA")
  expect_snapshot(res_psu)
})