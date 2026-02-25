load("../test_data.Rdata")

test_that("strat_total_biomass output exactly matches baseline", {
  # 1. Isolate the data used in the original test
  strat = subset(fk2012, STRAT == "FDLR" & PROT == 0 & SPECIES_CD == "MYC BONA")
  
  # 2. Run the prerequisite pipeline
  ssu_res = ssu_biomass(strat, list(WLEN_A = 2e-4, WLEN_B = 3))
  psu_res = psu_biomass(ssu_res)
  
  # 3. Get the result and snapshot
  res = strat_total_biomass(psu_res, ntot2012)
  expect_snapshot(res)
})

test_that("domain_total_biomass output exactly matches baseline", {
  # 1. Isolate the data
  domain = subset(fk2012, SPECIES_CD == "CEP FULV")
  
  # 2. Run the prerequisite pipeline
  ssu_res = ssu_biomass(domain, list(WLEN_A = 2e-4, WLEN_B = 2.8))
  psu_res = psu_biomass(ssu_res)
  strat_res = strat_biomass(psu_res, ntot2012)
  
  # 3. Get the result and snapshot
  res = domain_total_biomass(strat_res, ntot2012)
  expect_snapshot(res)
})


# context("STRAT total_biomass function")

# load("./../test_data.Rdata")

# strat = subset(fk2012, STRAT == "FDLR" & PROT == 0 & SPECIES_CD == "MYC BONA")
# sbiom = strat_total_biomass(psu_biomass(ssu_biomass(strat, list(WLEN_A = 2e-4, WLEN_B =3))), ntot2012)

# test_that("returns correct total biomass",
#           expect_equal(signif(sbiom$total_biomass, 4), 2.274e6)
#           )
# test_that("returns correct variance",
#           expect_equal(signif(sbiom$var, 4), 4.380e11)
#           )

# context("DOMAIN total_biomass function")

# domain = subset(fk2012, SPECIES_CD == "CEP FULV")
# dbiom = domain_total_biomass(strat_biomass(psu_biomass(ssu_biomass(domain, list(WLEN_A = 2e-4, WLEN_B =2.8))),
#                                            ntot2012), ntot2012)

# test_that("returns correct total biomass",
#           expect_equal(signif(dbiom$total_biomass, 4), 1.124e5)
#           )
# test_that("returns correct variance",
#           expect_equal(signif(dbiom$var, 4), 1.094e9)
#           )
