load('../test_data.Rdata')

lookup = data.frame(
  species = c("LUT GRIS", "LAC MAXI", "LUT ANAL", "OCY CHRY"),
  family = c("Lutjanidae", "Labridae", "Lutjanidae", "Lutjanidae")
)

ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & 
               STATION_NR == 1 &
               SPECIES_CD %in% c("LUT GRIS", "LAC MAXI", "LUT ANAL", "OCY CHRY"))

test_that("species_group output exactly matches baseline for density", {
  res = species_group(ssu_density(ssu), group = lookup)
  expect_snapshot(res)
})

test_that("species_group output exactly matches baseline for biomass", {
  res = species_group(ssu_biomass(ssu, taxonomic_data), group = lookup)
  expect_snapshot(res)
})

test_that("species_group output exactly matches baseline for occurrence", {
  res = species_group(ssu_occurrence(ssu), group = lookup)
  expect_snapshot(res)
})