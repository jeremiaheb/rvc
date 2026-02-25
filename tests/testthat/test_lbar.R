load('../test_data.Rdata')

test_that("ssu_lbar output exactly matches baseline", {
  # Use a single stratum to keep the SSU data frame small
  input = subset(fk2012, STRAT == "FSLR" & PROT == 0 & SPECIES_CD == "EPI MORI")
  res = ssu_lbar(input)
  expect_snapshot(res)
})

test_that("psu_lbar output exactly matches baseline", {
  input = subset(fk2012, STRAT == "FSLR" & PROT == 0 & SPECIES_CD == "EPI MORI")
  res = psu_lbar(ssu_lbar(input))
  expect_snapshot(res) # This will snapshot a list of 2 dataframes
})

test_that("strat_lbar output exactly matches baseline", {
  input = subset(fk2012, STRAT == "FSLR" & PROT == 0 & SPECIES_CD == "EPI MORI")
  res = strat_lbar(psu_lbar(ssu_lbar(input)), ntot2012)
  expect_snapshot(res) # This will snapshot a list of 3 dataframes
})

test_that("domain_lbar output exactly matches baseline", {
  # We can use a wider dataset here because domain_lbar returns a single summarized dataframe
  input = subset(fk2012, SPECIES_CD == "MYC BONA")
  res = domain_lbar(strat_lbar(psu_lbar(ssu_lbar(input)), ntot2012), ntot2012)
  expect_snapshot(res)
})