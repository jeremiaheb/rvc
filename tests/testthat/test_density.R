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


