## Tests for the occurrence functions
context("SSU occurrence function")

load('../test_data.Rdata')

ssu1 = subset(fk2012, SPECIES_CD == "OCY CHRY" & STRAT == "MCPR" &
               PRIMARY_SAMPLE_UNIT == "005U" & STATION_NR == 1)
ssu2 = subset(fk2012, SPECIES_CD == "MYC BONA" & STRAT == "MCPR" &
                PRIMARY_SAMPLE_UNIT == "005U" & STATION_NR == 1)
ssocc1 = ssu_occurrence(ssu1)
ssocc2 = ssu_occurrence(ssu2)

test_that("returns correct occurrence for single species in an SSU",
          {
            expect_equal(ssocc1$occurrence, 1)
            expect_equal(ssocc2$occurrence, 0)
          })
