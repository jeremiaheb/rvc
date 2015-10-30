# Test biomass functions

context("SSU biomass function")

load("./../test_data.Rdata")
ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == '005U' & STATION_NR == 2 &
               SPECIES_CD == 'SCA ISER')
ssbiom = ssu_biomass(ssu, growth_parameters = list(a = 2e-4, b = 3.0))

test_that('returns correct biomass',
          expect_equal(signif(ssbiom$biomass, 4), 3.317)
          )

context("PSU biomass function")

psu = subset(fk2012, PRIMARY_SAMPLE_UNIT == '330U' & SPECIES_CD == 'CAR RUBE')
pbiom = psu_biomass(ssu_biomass(psu, list(a = 2.8e-4, b = 2.8)))

test_that('returns correct biomass',
          expect_equal(signif(pbiom$biomass, 4), 1.542)
          )
test_that('returns correct variance',
          expect_equal(signif(pbiom$var, 4), 4.753)
          )

context("STRAT biomass function")

strat = subset(fk2012, STRAT == "FSLR" & PROT == 0 & SPECIES_CD == "STE LEUC")
sbiom = strat_biomass(psu_biomass(ssu_biomass(strat, list(a = 2e-4, b = 3))), ntot2012)

test_that('returns correct biomass',
          expect_equal(signif(sbiom$biomass, 4), 9.971e-3)
          )
test_that('returns correct variance',
          expect_equal(signif(sbiom$var, 4), 4.318e-6)
          )

context("DOMAIN biomass function")

domain = subset(fk2012, SPECIES_CD == "MYC BONA")
dbiom = domain_biomass(strat_biomass(psu_biomass(ssu_biomass(domain, list(a = 6.84e-6, b = 3.205))),
                                     ntot2012), ntot2012)

test_that('returns correct biomass',
          expect_equal(signif(dbiom$biomass, 4), 0.2706)
          )
test_that('returns correct variance',
          expect_equal(signif(dbiom$var, 4), 1.579e-3)
          )
