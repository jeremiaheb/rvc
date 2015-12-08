load('../test_data.Rdata')
context('test species group')
lookup = data.frame(species = c("Lutjanus griseus", "LAC MAXI", "LUT ANAL", "Yellowtail Snapper"),
                    family = c("Lutjanidae", "Labridae", "Lutjanidae", "Lutjanidae"))
ssu = subset(fk2012, PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & STATION_NR == 1 &
               SPECIES_CD %in% c("LUT GRIS", "LAC MAXI", "LUT ANAL", "OCY CHRY"))
test_that('returns unchanged data if group option not passed',
          expect_equal(species_group(ssu_density(ssu), taxonomic_data), ssu_density(ssu)))
test_that('raises error if group is not a data.frame',
          expect_error(species_group(ssu_density(ssu), taxonomic_data, group = 1))
          )
test_that('returns correct density',
          {
            dens = species_group(ssu_density(ssu), taxonomic_data, group = lookup)
            expect_equal(dens$density[dens$SPECIES_CD == "Lutjanidae"], 6)
            expect_equal(dens$density[dens$SPECIES_CD == "Labridae"], 2.5)
          }
          )
test_that('returns correct biomass',
          {
            biom = species_group(ssu_biomass(ssu, taxonomic_data), taxonomic_data, group = lookup)
            expect_equal(signif(biom$biomass[biom$SPECIES_CD == "Lutjanidae"]), 1.18874)
          }
          )
