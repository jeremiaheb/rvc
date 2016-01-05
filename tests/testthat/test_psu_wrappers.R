context("getPSUbiomass function");
load("../RVC.Rdata")
psu = subset(RVC$sample_data, REGION == "FLA KEYS" & PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & SPECIES_CD == "LAC MAXI")
newData = RVC;
newData$sample_data = psu;
gp = list(WLEN_A = 2.09e-5, WLEN_B = 2.988)
test_that('returns a data.frame',
          expect_true(is.data.frame(getPSUBiomass(RVC, species = "LAC MAXI"))))

test_that('returns correct biomass',
          expect_equal(getPSUBiomass(newData, species = "Lachnolaimus maximus",
                                     growth_parameters = list(a = 2.09e-5, b = 2.988)),
                      psu_biomass(ssu_biomass(psu, growth_parameters = gp))))

context("getPSUabundance function");
load("../RVC.Rdata")
psu = subset(RVC$sample_data, REGION == "FLA KEYS" & PRIMARY_SAMPLE_UNIT == PRIMARY_SAMPLE_UNIT[1] & SPECIES_CD == "LAC MAXI")
newData = RVC;
newData$sample_data = psu;
test_that('returns a data.frame',
          expect_true(is.data.frame(getPSUBiomass(RVC, species = "LAC MAXI"))))

test_that('returns correct biomass',
          expect_equal(getPSUAbundance(newData, species = "Lachnolaimus maximus"),
                      psu_abundance(psu_density(ssu_density(psu)))))