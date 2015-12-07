## Test wrapper functions against known data

load('../RVC.Rdata')

context('getDomainDensity function')

ddens = getDomainDensity(RVC, c("Hogfish"), length_bins = 30,
                 merge_protected = FALSE)

test_that('returns correct combined density',{
          dt2012 = subset(ddens, REGION == "DRTO" & length_class == "all" &
                          protected_status == "all")
          expect_equal(signif(dt2012$density, 6), 0.643498)
          }
          )
test_that('returns correct exploited density',
          {
            fk2011 = subset(ddens, YEAR == 2011 & REGION == 'FLA KEYS' & length_class == '>= 30' &
                               protected_status == "all")
            expect_equal(signif(fk2011$density,6), 0.279773)
          })
test_that('returns correct variance',
          {
            dt2012_2 = subset(ddens, REGION == "DRTO" & length_class == "< 30" &
                              protected_status == "all")
            expect_equal(signif(sqrt(dt2012_2$var), 4), 0.02171)
          }
          )

context('getStratumDensity function')

sdens = getStratumDensity(RVC, "LAC MAXI", years = 2011)

test_that('returns a data.frame with 13 rows and 11 columns',
          expect_equal(dim(sdens), c(13,11))
          )


context('getDomainBiomass function')

dbiom = getDomainBiomass(RVC, "LAC MAXI", list(a=3.438e-5, b = 2.9095), len_geq = 30, regions = "DRTO")

test_that('returns expected exploited biomass',
          expect_equal(signif(dbiom$biomass, 6), 0.609281)
          )
test_that('returns expected variance in exploited biomass',
          expect_equal(signif(sqrt(dbiom$var), 3), 0.0648)
          )

context('getStratumBiomass function')

sbiom = getStratumBiomass(RVC, "LAC MAXI", list(a=3.438e-5, b = 2.9095), regions = "DRTO")

test_that('returns a data.frame with the correct dimensions',
          expect_equal(dim(sbiom), c(17,11))
          )

context('getDomainTotalBiomass function')

dbiom_total = getDomainTotalBiomass(RVC, "LUT ANAL", list(a = 1.570e-5, b = 3.0112), length_bins = 40)

test_that('returns a data.frame with the correct dimensions',
          expect_equal(dim(dbiom_total), c(9, 10)))

context('getStratumTotalBiomass function')

sbiom_total = getStratumTotalBiomass(RVC, "LUT ANAL", list(a = 1.57e-5, b = 3.0112))

test_that('returns a data.frame with the correct number of dimensions',
          expect_equal(dim(sbiom_total), c(43, 11)))

context('getDomainAbundance function')

dabun = getDomainAbundance(RVC, "LUT ANAL", length_bins = 40, merge_protected = FALSE)

test_that('returns a data.frame with the correct number of dimensions',
          expect_equal(dim(dabun), c(30, 11))
          )

context('getStratumAbundance function')

sabun = getStratumAbundance(RVC, "LUT ANAL")

test_that('returns a data.frame with the correct number of dimensions',
          expect_equal(dim(sabun), c(43,11))
          )

context('getStratumLengthFrequency function')

slf = getStratumLengthFrequency(RVC, 'LAC MAXI', length_bins = seq(0,100), years = 2012, regions = "FLA KEYS")

test_that('returns a data.frame with the correct number of columns',
          expect_equal(ncol(slf), 7)
          )

context('getDomainLengthFrequency function')

dlf = getDomainLengthFrequency(RVC, 'LAC MAXI', length_bins = seq(0,100), years = 2012, regions = "FLA KEYS")

test_that('returns a data.frame with the correct number of columns',
          expect_equal(ncol(dlf), 5))

context('getDomainOccurrence function')

docc = getDomainOccurrence(RVC, "LAC MAXI")

test_that('returns a data.frame',
          expect_equal(class(docc), "data.frame"))

context('getStratumOccurrence function')

socc = getStratumOccurrence(RVC, 'LUT ANAL')

test_that('returns a data.frame',
           expect_equal(class(socc), "data.frame"))
