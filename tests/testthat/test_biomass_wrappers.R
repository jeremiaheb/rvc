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

test_that("getDomainTotalBiomass handles Base Case (NULL growth parameters) exactly as baseline", {
  # Should automatically pull parameters from taxonomic_data
  res = getDomainTotalBiomass(rvc_data, species = "EPI MORI")
  expect_snapshot(res)
})

test_that("getDomainTotalBiomass handles custom list parameters exactly as baseline", {
  res = getDomainTotalBiomass(
    rvc_data, 
    species = "EPI MORI", 
    growth_parameters = list(a = 1.13e-5, b = 3.035)
  )
  expect_snapshot(res)
})

test_that("getDomainTotalBiomass handles custom dataframe parameters and grouping exactly as baseline", {
  gp_df = data.frame(
    SPECIES_CD = c("EPI MORI", "MYC BONA"),
    a = c(1.13e-5, 1.2e-5),
    b = c(3.035, 3.1)
  )
  group_lookup = data.frame(
    species = c("EPI MORI", "MYC BONA"),
    group = rep("Groupers", 2)
  )
  
  res = getDomainTotalBiomass(
    rvc_data, 
    species = c("EPI MORI", "MYC BONA"), 
    growth_parameters = gp_df,
    group = group_lookup
  )
  expect_snapshot(res)
})

test_that("getDomainTotalBiomass handles Kitchen Sink Filtering exactly as baseline", {
  res = getDomainTotalBiomass(
    rvc_data, 
    species = "EPI MORI", 
    years = 2012, 
    is_protected = FALSE, 
    length_bins = 20
  )
  expect_snapshot(res)
})


# --- MULTI-YEAR & MIXED DESIGN TESTS (multi_year_data) ---

test_that("Biomass wrappers correctly calculate mixed designs across multiple years", {
  
  skip_if_not(exists("multi_year_data"), message = "multi_year_data not found in test environment.")
  
  # 1. Domain Total Biomass
  res_domain_tot = getDomainTotalBiomass(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_domain_tot)
  
  # 2. Stratum Biomass per SSU
  res_strat_biomass = getStratumBiomass(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_strat_biomass)
  
  # 3. PSU Biomass
  res_psu = getPSUBiomass(multi_year_data, species = "EPI MORI")
  expect_snapshot(res_psu)
})