# getDomainOccurrence handles Base Case exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD occurrence          var   n  nm     N      NM
      1 2012 FLA KEYS   EPI MORI  0.1736231 0.0002609437 416 803 14095 3190455
        STAGE_LEVEL
      1           2

# getDomainOccurrence handles Kitchen Sink Filtering exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD occurrence          var  n nm     N      NM
      1 2012 FLA KEYS   EPI MORI 0.03741015 7.010299e-05 77 92 13078 2960254
      2 2012 FLA KEYS   EPI MORI 0.96258985 7.010299e-05 77 92 13078 2960254
      3 2012 FLA KEYS   EPI MORI 1.00000000 0.000000e+00 77 92 13078 2960254
        STAGE_LEVEL length_class
      1           2         < 20
      2           2        >= 20
      3           2          all

# getDomainOccurrence handles Split Protected Status exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD occurrence          var   n  nm     N        NM
      1 2012 FLA KEYS   EPI MORI  0.1748787 0.0002914798 302 587 13285 3007108.9
      2 2012 FLA KEYS   EPI MORI  0.1530291 0.0006063203 114 216   810  183346.5
        STAGE_LEVEL protected_status
      1           2                0
      2           2                1

# getStratumOccurrence handles custom species groups exactly as baseline

    Code
      res
    Output
         YEAR   REGION STRAT PROT    GROUP occurrence         var   n  nm    N
      1  2012 FLA KEYS  FDLR    0 Groupers 0.32500000 0.003962255  40  77 1517
      2  2012 FLA KEYS  FMLR    0 Groupers 0.26744186 0.000990575 129 245 4979
      3  2012 FLA KEYS  FMLR    1 Groupers 0.37209302 0.003212234  43  84  331
      4  2012 FLA KEYS  FSLR    0 Groupers 0.15517241 0.003107052  29  58 1365
      5  2012 FLA KEYS  FSLR    1 Groupers 0.22727273 0.005008391  22  39  128
      6  2012 FLA KEYS  HRRF    0 Groupers 0.27777778 0.014498619   9  17  207
      7  2012 FLA KEYS  HRRF    1 Groupers 0.47058824 0.011233286  17  30  175
      8  2012 FLA KEYS  INPR    0 Groupers 0.08333333 0.006943491   6  12  386
      9  2012 FLA KEYS  INPR    1 Groupers 0.33333333 0.100000000   3   6   30
      10 2012 FLA KEYS  MCPR    0 Groupers 0.33333333 0.002063389  60 120 3080
      11 2012 FLA KEYS  MCPR    1 Groupers 0.45833333 0.009877427  12  24   58
      12 2012 FLA KEYS  OFPR    0 Groupers 0.41379310 0.003753232  29  58 1751
      13 2012 FLA KEYS  OFPR    1 Groupers 0.50000000 0.007564803  17  33   88
                  NM STAGE_LEVEL
      1   343378.558           2
      2  1127015.057           2
      3    74923.074           2
      4   308972.796           2
      5    28973.273           2
      6    46855.215           2
      7    39611.897           2
      8    87372.527           2
      9     6790.611           2
      10  697169.386           2
      11   13128.514           2
      12  396345.323           2
      13   19919.125           2

# Occurrence wrappers correctly calculate mixed designs across multiple years

    Code
      res_domain
    Output
        YEAR   REGION SPECIES_CD occurrence          var   n  nm      N      NM
      1 2012 FLA KEYS   EPI MORI  0.1727927 0.0002635648 416 803  16977 3842807
      2 2022 FLA KEYS   EPI MORI  0.1377435 0.0005595039 648  NA 270545 3827429
        STAGE_LEVEL
      1           2
      2           1

---

    Code
      res_strat
    Output
         YEAR   REGION STRAT PROT SPECIES_CD occurrence          var   n  nm      N
      1  2012 FLA KEYS  FDLR    0   EPI MORI 0.07500000 0.0014383276  40  77   2220
      2  2012 FLA KEYS  FMLR    0   EPI MORI 0.11240310 0.0005176958 129 245   6674
      3  2012 FLA KEYS  FMLR    1   EPI MORI 0.17441860 0.0020423748  43  84    272
      4  2012 FLA KEYS  FSLR    0   EPI MORI 0.10344828 0.0026243822  29  58    846
      5  2012 FLA KEYS  FSLR    1   EPI MORI 0.20454545 0.0048037715  22  39    120
      6  2012 FLA KEYS  HRRF    0   EPI MORI 0.00000000 0.0000000000   9  17    291
      7  2012 FLA KEYS  HRRF    1   EPI MORI 0.00000000 0.0000000000  17  30    149
      8  2012 FLA KEYS  INPR    0   EPI MORI 0.08333333 0.0069434907   6  12    386
      9  2012 FLA KEYS  INPR    1   EPI MORI 0.00000000 0.0000000000   3   6     35
      10 2012 FLA KEYS  MCPR    0   EPI MORI 0.28333333 0.0018819057  60 120   3711
      11 2012 FLA KEYS  MCPR    1   EPI MORI 0.33333333 0.0090433830  12  24     87
      12 2012 FLA KEYS  OFPR    0   EPI MORI 0.34482759 0.0037633371  29  58   2103
      13 2012 FLA KEYS  OFPR    1   EPI MORI 0.23529412 0.0078652031  17  33     83
      14 2022 FLA KEYS   S01    0   EPI MORI 0.09090909 0.0082497597  11  NA   6183
      15 2022 FLA KEYS   S01    1   EPI MORI 0.00000000 0.0000000000   6  NA    556
      16 2022 FLA KEYS   S02    0   EPI MORI 0.11764706 0.0064856861  17  NA  50062
      17 2022 FLA KEYS   S02    1   EPI MORI 0.25000000 0.0622700092   4  NA   1087
      18 2022 FLA KEYS   S03    0   EPI MORI 0.22784810 0.0022368128  79  NA   9507
      19 2022 FLA KEYS   S03    1   EPI MORI 0.08333333 0.0066201902  12  NA    257
      20 2022 FLA KEYS   S05    0   EPI MORI 0.19753086 0.0019798350  81  NA 102208
      21 2022 FLA KEYS   S05    1   EPI MORI 0.12500000 0.0035059217  32  NA   5062
      22 2022 FLA KEYS   S06    0   EPI MORI 0.10112360 0.0010283366  89  NA  20025
      23 2022 FLA KEYS   S06    1   EPI MORI 0.04716981 0.0004091563 106  NA   2402
      24 2022 FLA KEYS   S08    0   EPI MORI 0.08823529 0.0024350352  34  NA  29203
      25 2022 FLA KEYS   S08    1   EPI MORI 0.00000000           NA   1  NA   1014
      26 2022 FLA KEYS   S09    0   EPI MORI 0.06862745 0.0006293695 102  NA  18553
      27 2022 FLA KEYS   S09    1   EPI MORI 0.21739130 0.0074713352  23  NA    679
      28 2022 FLA KEYS   S11    0   EPI MORI 0.05882353 0.0011048884  51  NA  23747
                  NM STAGE_LEVEL
      1   502505.207           2
      2  1510684.573           2
      3    61568.206           2
      4   191495.228           2
      5    27162.444           2
      6    65868.926           2
      7    33726.701           2
      8    87372.527           2
      9     7922.379           2
      10  839998.569           2
      11   19692.772           2
      12  476021.824           2
      13   18787.357           2
      14   87471.557           1
      15    7865.791           1
      16  708232.423           1
      17   15377.904           1
      18  134496.537           1
      19    3635.806           1
      20 1445947.415           1
      21   71612.651           1
      22  283295.799           1
      23   33981.349           1
      24  413137.938           1
      25   14345.166           1
      26  262471.259           1
      27    9605.885           1
      28  335951.327           1

