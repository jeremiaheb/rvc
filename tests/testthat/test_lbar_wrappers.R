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

test_that("getDomainLbar handles Base Case exactly as baseline", {
  res = getDomainLbar(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainLbar handles Length Bins exactly as baseline", {
  res = getDomainLbar(rvc_data, species = "EPI MORI", length_bins = 60)
  expect_snapshot(res)
})

test_that("getDomainLbar handles Split Protected Status exactly as baseline", {
  res = getDomainLbar(rvc_data, species = "EPI MORI", merge_protected = FALSE)
  expect_snapshot(res)
})

test_that("getDomainLbar handles Kitchen Sink Filtering exactly as baseline", {
  res = getDomainLbar(
    rvc_data, 
    species = "OCY CHRY", 
    years = 2012, 
    merge_protected = FALSE, 
    length_bins = 23
  )
  expect_snapshot(res)
})

# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Lbar wrappers correctly calculate mixed designs across multiple years", {
  
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # Domain Lbar
  res_domain = getDomainLbar(multi_year_data, species = "EPI MORI", 
    merge_protected = FALSE)
  expect_snapshot(res_domain)
})