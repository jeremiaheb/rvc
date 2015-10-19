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

context("PSU occurrence function")

psu = subset(fk2012, SPECIES_CD == "OCY CHRY" & STRAT == "MCPR" &
               PRIMARY_SAMPLE_UNIT == "005U")
pocc = psu_occurrence(ssu_occurrence(psu))

test_that("returns correct occurrence for a single species and psu",
          expect_equal(pocc$occurrence, 1))
test_that("returns correct variance for a single species and psu",
          expect_equal(pocc$var, 0))
test_that("returns correnct m for a psu",
          expect_equal(pocc$m, 2))

context("STRAT level occurrence")

strat = subset(fk2012, SPECIES_CD == "EPI MORI" & STRAT == "FSLR" &
                 PROT == 0)
socc = strat_occurrence(psu_occurrence(ssu_occurrence(strat)), ntot2012)

test_that("returns correct occurrence for a single species and stratum",
          expect_equal(signif(socc$occurrence,4), 0.1034)
)
test_that("returns correct variance for a single species and stratum",
          expect_equal(signif(socc$var, 4), 2.652e-3)
          )
test_that("returns correct n for a single species and stratum",
          expect_equal(socc$n, 29)
          )
test_that("returns correct nm for a single stratum",
          expect_equal(socc$nm, 58)
          )
test_that("returns corecct N for a single stratum",
          expect_equal(socc$N, 1365)
          )
test_that("returns correct NM for a single stratum",
          expect_equal(socc$NM, 308972)
          )

context("DOMAIN level occurrence")

domain = subset(fk2012, SPECIES_CD == "STE VARI")
docc = domain_occurrence(strat_occurrence(psu_occurrence(ssu_occurrence(domain)), ntot2012), ntot2012)

test_that("returns correct occurrece for a single species and domain",
          expect_equal(signif(docc$occurrence, 4), 0.6456)
          )
test_that("returns correct variance for a single species and domain",
          expect_equal(signif(docc$var, 4), 4.358e-4)
          )
test_that("returns correct n for a single domain",
          expect_equal(docc$n, 416)
          )
test_that("returns correct nm for a single domain",
          expect_equal(docc$nm, 803)
          )
test_that("returns correct N for a single domain",
          expect_equal(docc$N, 14095)
          )
test_that("returns correct NM for a single domain",
          expect_equal(docc$NM, 3190450)
          )
