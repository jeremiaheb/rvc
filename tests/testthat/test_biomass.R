# Test biomass functions

context("SSU biomass function")

load("./../test_data.Rdata")
ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == '005U' & STATION_NR == 2 &
               SPECIES_CD == 'SCA ISER')
ssu2 = subset(fk2012,  PRIMARY_SAMPLE_UNIT == '005U' & STATION_NR == 2 &
                SPECIES_CD %in% c("LUT GRIS", "OCY CHRY"))
ssbiom = ssu_biomass(ssu, growth_parameters = list(WLEN_A = 2e-4, WLEN_B = 3.0))
ssbiom2 = ssu_biomass(ssu2, taxonomic_data)

test_that('returns correct biomass',
          expect_equal(signif(ssbiom$biomass, 4), 3.317)
          )
test_that('handles data.frame input',
          {
            lg = subset(ssbiom2, SPECIES_CD == 'LUT GRIS')
            oc = subset(ssbiom2, SPECIES_CD == 'OCY CHRY')
            expect_equal(signif(lg$biomass, 4), 0.4028)
            expect_equal(signif(oc$biomass, 4), 3.173)
          })
test_that('raises error if growth_parameters not found',
          {
            gpl = list(WLEN_A = 2.25e-5)
            gpd = data.frame(SPECIES_CD = "LAC MAXI", WLEN_A = 2.25e-5)
            expect_error(ssu_biomass(ssu2, gpl))
            expect_error(ssu_biomass(ssu2, gpd))})

context("PSU biomass function")

psu = subset(fk2012, PRIMARY_SAMPLE_UNIT == '330U' & SPECIES_CD == 'CAR RUBE')
pbiom = psu_biomass(ssu_biomass(psu, list(WLEN_A = 2.8e-4, WLEN_B = 2.8)))

test_that('returns correct biomass',
          expect_equal(signif(pbiom$biomass, 4), 1.542)
          )
test_that('returns correct variance',
          expect_equal(signif(pbiom$var, 4), 4.753)
          )

context("STRAT biomass function")

strat = subset(fk2012, STRAT == "FSLR" & PROT == 0 & SPECIES_CD == "STE LEUC")
sbiom = strat_biomass(psu_biomass(ssu_biomass(strat, list(WLEN_A = 2e-4, WLEN_B = 3))), ntot2012)

test_that('returns correct biomass',
          expect_equal(signif(sbiom$biomass, 4), 9.971e-3)
          )
test_that('returns correct variance',
          expect_equal(signif(sbiom$var, 4), 4.318e-6)
          )

context("DOMAIN biomass function")

domain = subset(fk2012, SPECIES_CD == "MYC BONA")
dbiom = domain_biomass(strat_biomass(psu_biomass(ssu_biomass(domain, list(WLEN_A = 6.84e-6, WLEN_B = 3.205))),
                                     ntot2012), ntot2012)

test_that('returns correct biomass',
          expect_equal(signif(dbiom$biomass, 4), 0.2706)
          )
test_that('returns correct variance',
          expect_equal(signif(dbiom$var, 4), 1.579e-3)
          )
