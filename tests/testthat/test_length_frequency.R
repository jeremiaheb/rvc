## Test length frequency functions

context('STRAT length frequency');

load('../test_data.Rdata')

strat = subset(fk2012, SPECIES_CD == 'EPI MORI' & STRAT == 'FSLR' & PROT == 0)
slf = strat_length_frequency(strat, ntot2012)

test_that('returns correct length frequencies',
          {
            expect_equal(nrow(slf), 6)
            expect_equal(slf$frequency[slf$length_class == 29], 0.125)
          })

context('DOMAIN length frequency')

domain = subset(fk2012, SPECIES_CD == 'LUT GRIS')
dlf = domain_length_frequency(strat_length_frequency(domain, ntot2012), ntot2012)

test_that('sum frequency is 1', expect_equal(sum(dlf$frequency), 1))
test_that('returns correct frequency',
          expect_equal(signif(dlf$frequency[dlf$length_class == 6.0], 4), 3.525e-5)
)
