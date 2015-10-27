## Test length frequency functions

context('STRAT length frequency');

load('../test_data.Rdata')

strat = subset(fk2012, SPECIES_CD == 'EPI MORI' & STRAT == 'FSLR' & PROT == 0)
slf = strat_length_frequency(strat, ntot2012)

test_that('returns correct length frequencies',
          {
            expect_equal(nrow(slf), 8)
            expect_equal(slf$frequency[slf$LEN == 29], 0.125)
          })
test_that('returns correct n',
          expect_equal(mean(slf$n), 29)
          )
test_that('returns correct nm', expect_equal(slf$nm[1], 58))
test_that('returns correct N', expect_equal(slf$N[1], 1365))
test_that('returns correct NM', expect_equal(slf$NM[1], 308972))
