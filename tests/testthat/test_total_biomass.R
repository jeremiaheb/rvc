context("STRAT total_biomass function")

load("./../test_data.Rdata")

strat = subset(fk2012, STRAT == "FDLR" & PROT == 0 & SPECIES_CD == "MYC BONA")
sbiom = strat_total_biomass(psu_biomass(ssu_biomass(strat, list(a = 2e-4, b = 3))), ntot2012)

test_that("returns correct total biomass",
          expect_equal(round(sbiom$total_biomass), 2274492)
          )
test_that("returns correct variance",
          expect_equal(round(sbiom$var), 438026523625)
          )

context("DOMAIN total_biomass function")

domain = subset(fk2012, SPECIES_CD == "CEP FULV")
dbiom = domain_total_biomass(strat_biomass(psu_biomass(ssu_biomass(domain, list(a = 2e-4, b = 2.8))),
                                           ntot2012), ntot2012)

test_that("returns correct total biomass",
          expect_equal(round(dbiom$total_biomass), 112428)
          )
test_that("returns correct variance",
          expect_equal(round(dbiom$var), 1093592375)
          )
