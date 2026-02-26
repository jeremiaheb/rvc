# 1. Load the original single-year data
load('../test_data.Rdata')

# Construct the master list expected by the wrappers for the 2012 data
rvc_data = list(
  sample_data = fk2012,
  stratum_data = ntot2012,
  taxonomic_data = taxonomic_data
)

# 2. Load the new multi-year, mixed-design data
load('../test_data_multi.Rdata')


# --- SINGLE-YEAR FILTERING TESTS (rvc_data) ---

test_that("getDomainDensity handles Base Case exactly as baseline", {
  res = getDomainDensity(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainDensity handles Kitchen Sink Filtering exactly as baseline", {
  # Filtering by year, protected status, length, and count all at once
  res = getDomainDensity(
    rvc_data, 
    species = "EPI MORI", 
    years = 2012, 
    is_protected = FALSE, 
    length_bins = 20,
    when_present = TRUE
  )
  expect_snapshot(res)
})

test_that("getDomainDensity handles Split Protected Status exactly as baseline", {
  # merge_protected = FALSE
  res = getDomainDensity(rvc_data, species = "EPI MORI", merge_protected = FALSE)
  expect_snapshot(res)
})

test_that("getDomainDensity handles Length Bins exactly as baseline", {
  # numeric length bins
  res = getDomainDensity(rvc_data, species = "EPI MORI", length_bins = c(20, 30))
  expect_snapshot(res)
})

test_that("getStratumDensity handles custom species groups exactly as baseline", {
  # Testing the group lookup table and stratum-level output
  group_lookup = data.frame(
    species = c("EPI MORI", "MYC BONA"),
    group = rep("Groupers", 2)
  )
  res = getStratumDensity(rvc_data, species = c("EPI MORI", "MYC BONA"), group = group_lookup)
  expect_snapshot(res)
})

test_that("getPSUDensity output exactly matches baseline", {
  res = getPSUDensity(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})


# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Wrappers correctly calculate mixed 1-stage/2-stage designs across multiple years", {
  
  # Safety check: Ensure the multi-year data loaded correctly
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # 1. Domain Density: Tests grouping across years and domains
  res_domain = getDomainDensity(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_domain)
  
  # 2. Stratum Density: Tests stratum variance math on mixed designs
  res_strat = getStratumDensity(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_strat)
  
  # 3. PSU Density: Tests base-level density calculations on 1-stage vs 2-stage SSUs
  res_psu = getPSUDensity(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_psu)
})