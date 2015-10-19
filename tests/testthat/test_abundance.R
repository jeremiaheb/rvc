# Test abundance functions
context("STRAT abundance function")

load("./../test_data.Rdata")

strat = subset(fk2012, SPECIES_CD == "MYC BONA" &
                       STRAT == "FDLR" &
                       PROT == 0)

pdens = psu_density(ssu_density(strat))
sabun = strat_abundance(pdens, ntot2012)

test_that("returns correct abundance",
          expect_equal(round(sabun$abundance), 446)
          )
test_that("returns correct variance",
          expect_equal(round(sabun$var), 17614)
          )

