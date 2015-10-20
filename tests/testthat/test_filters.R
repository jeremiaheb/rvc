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
