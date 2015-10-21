## Tests for filters

context("Species filter")

species_data = data.frame(SPECIES_CD = c("EPI MORI", "MYC BONA"))

test_that('returns subset data for single species',
          expect_equal(as.character(species_filter(species_data, "EPI MORI")$SPECIES_CD), "EPI MORI")
          )
test_that('returns subset data for multiple species',
          expect_equal(as.character(species_filter(species_data, c("EPI MORI", "MYC BONA"))$SPECIES_CD),
          c("EPI MORI", "MYC BONA"))
)

context("Strata filter")

stratum_data = data.frame(STRAT = c("FDLR", "FSLR"))

test_that('returns subset data for a single stratum',
          expect_equal(as.character(strata_filter(stratum_data, "FSLR")$STRAT), "FSLR")
          )
test_that('returns subset data for multiple species',
          expect_equal(as.character(strata_filter(stratum_data, c("FSLR", "FDLR"))$STRAT), c("FDLR", "FSLR"))
          )

context("Protected status filter")

prot_data = data.frame(PROT = c(1, 0, 2))

test_that('returns original data if optional args left NULL',
          expect_equal(protected_filter(prot_data)$PROT, c(1,0,2))
          )
test_that('returns subset data for one protected status',
          expect_equal(protected_filter(prot_data, 0)$PROT, 0)
          )
test_that('returns subset data for multiple protected statuses',
          expect_equal(protected_filter(prot_data, c(0,1))$PROT, c(1,0))
          )
test_that('returns subset data for is_protected = TRUE',
          expect_equal(protected_filter(prot_data, is_protected = TRUE)$PROT, c(1,2))
          )
test_that('returns subset data for is_protected = FALSE',
          expect_equal(protected_filter(prot_data, is_protected = FALSE)$PROT, 0)
          )
