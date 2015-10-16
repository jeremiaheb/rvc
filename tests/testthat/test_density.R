## Test density functions
context("SSU density function")

## Load data and select an SSU
load('../test_data.Rdata')
ssu = subset(fk2012, SPECIES_CD == "ANI VIRG" &
                PRIMARY_SAMPLE_UNIT == "005U" &
                STATION_NR == 2)

test_that("returns correctly aggregated count for one species",
          {
            expect_equal(ssu_density(ssu)$density, 18)
          })

context("PSU density function")

psu = subset(fk2012, SPECIES_CD == "LUT GRIS" &
               PRIMARY_SAMPLE_UNIT == "005U")

test_that("returns correctly aggregated density for one species",
          {
            expect_equal(psu_density(psu)$density, 0.5)
          })
test_that("returns correctly calculated variance for one species",
          {
            expect_equal(psu_density(psu)$var, 0.5)
          })
test_that("returns correctly calculated m for one psu",
          {
            expect_equal(psu_density(psu)$m, 2)
          })

context("STRAT density function")

strat = subset(fk2012, SPECIES_CD == "MYC BONA" &
                 STRAT == "FDLR" &
                 PROT == 0)
sdens = strat_density(strat, ntot2012)

test_that("returns correct density for one stratum",
          {
            expect_equal(signif(sdens$density, 4), 2.938e-1)
          })
test_that("returns correct variance for one stratum",
          {
            expect_equal(signif(sdens$var, 4), 7.654e-3)
          })
test_that("returns correct n for one stratum",
          {
            expect_equal(sdens$n, 40)
          })
test_that("returns correct nm for one stratum",
          {
            expect_equal(sdens$nm, 77)
          })
test_that("returns correct N for one stratum",
          {
            expect_equal(sdens$N, 1517)
          })
test_that("returns correct NM for one stratum",
          {
            expect_equal(sdens$NM, 343378)
          })


