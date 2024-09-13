# Test abundance functions
context("STRAT abundance function")

load("../test_data.Rdata")

strat = subset(fk2012, SPECIES_CD == "MYC BONA" &
                       STRAT == "FDLR" &
                       PROT == 0)

pdens = psu_density(ssu_density(strat))
# sabun = strat_abundance(pdens, ntot2012)
#
# test_that("returns correct abundance",
#           expect_equal(signif(sabun$abundance, 4), 1.009e5)
#           )
# test_that("returns correct variance",
#           expect_equal(signif(sabun$var, 4), 9.025e8)
#           )

context("DOMAIN abundance function")

domain = subset(fk2012, SPECIES_CD == "ANI VIRG")

sdens = strat_density(psu_density(ssu_density(domain)), ntot2012)
dabun = domain_abundance(sdens, ntot2012)

test_that("returns correct abundance",
          expect_equal(signif(dabun$abundance, 4), 2.714e6)
          )
test_that("returns correct variance",
          expect_equal(signif(dabun$var, 4), 8.618e10)
          )
