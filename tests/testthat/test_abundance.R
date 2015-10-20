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

context("DOMAIN abundance function")

domain = subset(fk2012, SPECIES_CD == "ANI VIRG")

sdens = strat_density(psu_density(ssu_density(domain)), ntot2012)
dabun = domain_abundance(sdens, ntot2012)

test_that("returns correct abundance",
          expect_equal(round(dabun$abundance), 11990)
          )
test_that("returns correct variance",
          expect_equal(round(dabun$var), 1682082)
          )