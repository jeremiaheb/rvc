## Test length frequency functions

context('STRAT length frequency');

load('../test_data.Rdata')

strat = subset(fk2012, SPECIES_CD == 'EPI MORI' & STRAT == 'FSLR' & PROT == 0)
slf = strat_length_frequency(strat, ntot2012)

test_that('returns correct length frequencies',
          {
            expect_equal(nrow(slf), 7)
            expect_equal(slf$frequency[slf$LEN == 29], 0.125)
          })
test_that('returns correct n',
          expect_equal(mean(slf$n), 29)
          )
test_that('returns correct nm', expect_equal(slf$nm[1], 58))
test_that('returns correct N', expect_equal(slf$N[1], 1365))
test_that('returns correct NM', expect_equal(slf$NM[1], 308972))

context('DOMAIN length frequency')

domain = subset(fk2012, SPECIES_CD == 'LUT GRIS')
dlf = domain_length_frequency(strat_length_frequency(domain, ntot2012), ntot2012)

test_that('sum frequency is 1', expect_equal(sum(dlf$frequency), 1))
test_that('returns correct frequency',
          expect_equal(signif(dlf$frequency[dlf$LEN == 6.0], 4), 3.525e-5)
)
test_that('returns correct N', expect_equal(dlf$N[1], 14065))
test_that('returns correct NM', expect_equal(dlf$NM[1], 3183660))
test_that('returns correct n', expect_equal(dlf$n[1], 413))
test_that('returns corret nm', expect_equal(dlf$nm[1], 797))
