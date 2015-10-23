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
test_that('returns original data is species_cd left NULL',
          expect_equal(species_filter(species_data), species_data)
          )

context("Strata filter")

stratum_data = data.frame(STRAT = c("FDLR", "FSLR"))

test_that('returns subset data for a single stratum',
          expect_equal(as.character(strata_filter(stratum_data, "FSLR")$STRAT), "FSLR")
          )
test_that('returns subset data for multiple species',
          expect_equal(as.character(strata_filter(stratum_data, c("FSLR", "FDLR"))$STRAT), c("FDLR", "FSLR"))
          )
test_that('returns original data if strata left NULL',
          expect_equal(strata_filter(stratum_data), stratum_data)
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

context("Year filter")

year_data = data.frame(YEAR = c(2000, 2001))

test_that('returns original data if years is NULL',
          expect_equal(year_filter(year_data), year_data)
          )
test_that('returns correctly subset data',
          expect_equal(year_filter(year_data, years = 2001)$YEAR, 2001)
          )

context("Region filter")

region_data = data.frame(REGION = c("FLA KEYS", "DRTO"))

test_that('returns original data if regions is NULL',
          expect_equal(region_filter(region_data), region_data))
test_that('returns correctly subset data',
          expect_equal(as.character(region_filter(region_data,
          region = "FLA KEYS")$REGION), "FLA KEYS"))

context("Length filter")

length_data = data.frame(LEN = c(0,1,2,3), NUM = c(1,2,3,4))

test_that('returns original data if args NULL',
          expect_equal(length_filter(length_data), length_data)
          )
test_that('returns lengths greater than input args',
          expect_equal(length_filter(length_data, len_geq = 2)$NUM, c(0,0,3,4))
          )
test_that('returns length less than input args',
          expect_equal(length_filter(length_data, len_lt = 3)$NUM, c(1,2,3,0))
          )
test_that('returns length greater and less than input args',
          expect_equal(length_filter(length_data, len_lt = 3, len_geq = 2)$NUM, c(0,0,3,0))
          )

context("Count filter")

count_data = data.frame(NUM = c(0, 0.25, 1, 3))

test_that('returns original data if args NULL',
          expect_equal(count_filter(count_data), count_data)
          )
test_that('returns data greater than or equal to input args',
          expect_equal(count_filter(count_data, cnt_geq = 2e-6)$NUM, c(0.25, 1, 3))
          )
test_that('returns data less than input args',
          expect_equal(count_filter(count_data, cnt_lt = 1)$NUM, c(0, 0.25))
          )
test_that('returns data between input args',
          expect_equal(count_filter(count_data, cnt_geq = 2e-6, cnt_lt = 3)$NUM, c(0.25,1))
          )
test_that('returns data where NUM > 0 if when_present is TRUE',
          expect_equal(count_filter(count_data, when_present = TRUE)$NUM, c(0.25, 1, 3))
          )
