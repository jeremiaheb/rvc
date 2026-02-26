# getDomainAbundance handles Base Case exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD abundance        var   n  nm     N      NM
      1 2012 FLA KEYS   EPI MORI  401090.7 2040825326 416 803 14095 3190455
        STAGE_LEVEL
      1           2

# getDomainAbundance handles Kitchen Sink Filtering exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD  abundance          var   n  nm     N        NM
      1 2012 FLA KEYS   HAE PLUM  5967720.5 1.899676e+12 302 587 13285 3007108.9
      2 2012 FLA KEYS   HAE PLUM 25975789.2 5.841972e+12 302 587 13285 3007108.9
      3 2012 FLA KEYS   HAE PLUM 31943509.6 9.149574e+12 302 587 13285 3007108.9
      4 2012 FLA KEYS   HAE PLUM   149309.4 4.141079e+09 114 216   810  183346.5
      5 2012 FLA KEYS   HAE PLUM  1056819.5 6.486314e+10 114 216   810  183346.5
      6 2012 FLA KEYS   HAE PLUM  1206129.0 8.047498e+10 114 216   810  183346.5
        STAGE_LEVEL length_class protected_status
      1           2         < 10                0
      2           2        >= 10                0
      3           2          all                0
      4           2         < 10                1
      5           2        >= 10                1
      6           2          all                1

# getDomainAbundance handles Split Protected Status exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD abundance        var   n  nm     N        NM
      1 2012 FLA KEYS   EPI MORI 382252.72 2029922785 302 587 13285 3007108.9
      2 2012 FLA KEYS   EPI MORI  18837.98   10902541 114 216   810  183346.5
        STAGE_LEVEL protected_status
      1           2                0
      2           2                1

# getStratumAbundance handles custom species groups exactly as baseline

    Code
      res
    Output
         YEAR   REGION STRAT PROT    GROUP  abundance          var   n  nm    N
      1  2012 FLA KEYS  FDLR    0 Snappers 1418582.67 1.634819e+11  40  77 1517
      2  2012 FLA KEYS  FMLR    0 Snappers 5521500.12 8.188340e+11 129 245 4979
      3  2012 FLA KEYS  FMLR    1 Snappers  384634.15 1.251405e+10  43  84  331
      4  2012 FLA KEYS  FSLR    0 Snappers  961544.65 1.000693e+11  29  58 1365
      5  2012 FLA KEYS  FSLR    1 Snappers  196557.32 2.690877e+09  22  39  128
      6  2012 FLA KEYS  HRRF    0 Snappers  753588.05 1.760986e+11   9  17  207
      7  2012 FLA KEYS  HRRF    1 Snappers  696703.36 1.450788e+10  17  30  175
      8  2012 FLA KEYS  INPR    0 Snappers  560640.38 1.203703e+11   6  12  386
      9  2012 FLA KEYS  INPR    1 Snappers   84882.64 7.779252e+08   3   6   30
      10 2012 FLA KEYS  MCPR    0 Snappers 4964427.00 4.230176e+11  60 120 3080
      11 2012 FLA KEYS  MCPR    1 Snappers  132652.70 9.943535e+08  12  24   58
      12 2012 FLA KEYS  OFPR    0 Snappers 4534053.83 1.662666e+12  29  58 1751
      13 2012 FLA KEYS  OFPR    1 Snappers  149686.37 9.232308e+08  17  33   88
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

# Abundance wrappers correctly calculate mixed designs across multiple years

    Code
      res_domain
    Output
        YEAR   REGION SPECIES_CD abundance          var   n  nm      N      NM
      1 2012 FLA KEYS   THA BIFA  65344040 6.015528e+12 416 803  16977 3842807
      2 2022 FLA KEYS   THA BIFA  72706312 1.500182e+13 648  NA 270545 3827429
        STAGE_LEVEL
      1           2
      2           1

---

    Code
      res_strat
    Output
         YEAR   REGION STRAT PROT SPECIES_CD    abundance          var   n  nm      N
      1  2012 FLA KEYS  FDLR    0   THA BIFA  7119870.652 3.532333e+11  40  77   2220
      2  2012 FLA KEYS  FMLR    0   THA BIFA 37178649.979 3.864313e+12 129 245   6674
      3  2012 FLA KEYS  FMLR    1   THA BIFA  1252841.392 1.468014e+10  43  84    272
      4  2012 FLA KEYS  FSLR    0   THA BIFA  4780777.404 5.471722e+11  29  58    846
      5  2012 FLA KEYS  FSLR    1   THA BIFA   634922.120 4.172787e+09  22  39    120
      6  2012 FLA KEYS  HRRF    0   THA BIFA  2307242.095 4.295935e+11   9  17    291
      7  2012 FLA KEYS  HRRF    1   THA BIFA  1043543.802 1.065737e+10  17  30    149
      8  2012 FLA KEYS  INPR    0   THA BIFA   145620.878 5.802600e+09   6  12    386
      9  2012 FLA KEYS  INPR    1   THA BIFA     5281.586 2.787403e+07   3   6     35
      10 2012 FLA KEYS  MCPR    0   THA BIFA  5260491.038 3.920069e+11  60 120   3711
      11 2012 FLA KEYS  MCPR    1   THA BIFA    85745.610 5.781943e+08  12  24     87
      12 2012 FLA KEYS  OFPR    0   THA BIFA  5269069.160 3.925892e+11  29  58   2103
      13 2012 FLA KEYS  OFPR    1   THA BIFA   259983.864 7.013985e+08  17  33     83
      14 2022 FLA KEYS   S01    0   THA BIFA   540733.260 1.156853e+11  11  NA   6183
      15 2022 FLA KEYS   S01    1   THA BIFA    34740.577 6.920189e+08   6  NA    556
      16 2022 FLA KEYS   S02    0   THA BIFA  3728635.405 1.532029e+12  17  NA  50062
      17 2022 FLA KEYS   S02    1   THA BIFA    51900.427 5.338033e+08   4  NA   1087
      18 2022 FLA KEYS   S03    0   THA BIFA  1147476.786 1.604866e+10  79  NA   9507
      19 2022 FLA KEYS   S03    1   THA BIFA    21814.838 1.895051e+07  12  NA    257
      20 2022 FLA KEYS   S05    0   THA BIFA 31418116.681 1.011944e+13  81  NA 102208
      21 2022 FLA KEYS   S05    1   THA BIFA  2335243.786 2.255594e+11  32  NA   5062
      22 2022 FLA KEYS   S06    0   THA BIFA  8623014.817 4.714406e+11  89  NA  20025
      23 2022 FLA KEYS   S06    1   THA BIFA  1585422.266 2.393470e+10 106  NA   2402
      24 2022 FLA KEYS   S08    0   THA BIFA 10036821.672 1.959286e+12  34  NA  29203
      25 2022 FLA KEYS   S08    1   THA BIFA   380146.887           NA   1  NA   1014
      26 2022 FLA KEYS   S09    0   THA BIFA  7311883.150 2.377464e+11 102  NA  18553
      27 2022 FLA KEYS   S09    1   THA BIFA   207361.822 4.180461e+08  23  NA    679
      28 2022 FLA KEYS   S11    0   THA BIFA  5282999.306 2.989823e+11  51  NA  23747
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

---

    Code
      res_psu
    Output
           YEAR   REGION STRAT PROT PRIMARY_SAMPLE_UNIT SPECIES_CD m     var
      1    2012 FLA KEYS  FDLR    0                065U   THA BIFA 2    12.5
      2    2012 FLA KEYS  FDLR    0                066U   THA BIFA 2    98.0
      3    2012 FLA KEYS  FDLR    0                074U   THA BIFA 2   800.0
      4    2012 FLA KEYS  FDLR    0                077U   THA BIFA 1      NA
      5    2012 FLA KEYS  FDLR    0                087U   THA BIFA 2    72.0
      6    2012 FLA KEYS  FDLR    0                090U   THA BIFA 2  1250.0
      7    2012 FLA KEYS  FDLR    0                109U   THA BIFA 2   200.0
      8    2012 FLA KEYS  FDLR    0                156U   THA BIFA 2   180.5
      9    2012 FLA KEYS  FDLR    0                157U   THA BIFA 2   264.5
      10   2012 FLA KEYS  FDLR    0                160U   THA BIFA 2    12.5
      11   2012 FLA KEYS  FDLR    0                161U   THA BIFA 2   242.0
      12   2012 FLA KEYS  FDLR    0                213U   THA BIFA 2   364.5
      13   2012 FLA KEYS  FDLR    0                214U   THA BIFA 2  1624.5
      14   2012 FLA KEYS  FDLR    0                226U   THA BIFA 2  1012.5
      15   2012 FLA KEYS  FDLR    0                230U   THA BIFA 2   480.5
      16   2012 FLA KEYS  FDLR    0                234U   THA BIFA 2   760.5
      17   2012 FLA KEYS  FDLR    0                235U   THA BIFA 2    84.5
      18   2012 FLA KEYS  FDLR    0                237U   THA BIFA 2     2.0
      19   2012 FLA KEYS  FDLR    0                238U   THA BIFA 2    50.0
      20   2012 FLA KEYS  FDLR    0                239U   THA BIFA 2  1352.0
      21   2012 FLA KEYS  FDLR    0                240U   THA BIFA 2   242.0
      22   2012 FLA KEYS  FDLR    0                241U   THA BIFA 2    40.5
      23   2012 FLA KEYS  FDLR    0                242U   THA BIFA 2   242.0
      24   2012 FLA KEYS  FDLR    0                243U   THA BIFA 2   364.5
      25   2012 FLA KEYS  FDLR    0                244U   THA BIFA 2   392.0
      26   2012 FLA KEYS  FDLR    0                246U   THA BIFA 2   162.0
      27   2012 FLA KEYS  FDLR    0                247U   THA BIFA 2   144.5
      28   2012 FLA KEYS  FDLR    0                249U   THA BIFA 2   800.0
      29   2012 FLA KEYS  FDLR    0                291U   THA BIFA 2    40.5
      30   2012 FLA KEYS  FDLR    0                313U   THA BIFA 2     8.0
      31   2012 FLA KEYS  FDLR    0                320U   THA BIFA 2     2.0
      32   2012 FLA KEYS  FDLR    0                323H   THA BIFA 1      NA
      33   2012 FLA KEYS  FDLR    0                323U   THA BIFA 1      NA
      34   2012 FLA KEYS  FDLR    0                384U   THA BIFA 2   392.0
      35   2012 FLA KEYS  FDLR    0                436U   THA BIFA 2  1740.5
      36   2012 FLA KEYS  FDLR    0                906U   THA BIFA 2   128.0
      37   2012 FLA KEYS  FDLR    0                944U   THA BIFA 2    32.0
      38   2012 FLA KEYS  FDLR    0                955U   THA BIFA 2    98.0
      39   2012 FLA KEYS  FDLR    0                959U   THA BIFA 2  1568.0
      40   2012 FLA KEYS  FDLR    0                960U   THA BIFA 2    98.0
      41   2012 FLA KEYS  FMLR    0                022H   THA BIFA 1      NA
      42   2012 FLA KEYS  FMLR    0                022U   THA BIFA 1      NA
      43   2012 FLA KEYS  FMLR    0                036U   THA BIFA 2    50.0
      44   2012 FLA KEYS  FMLR    0                040U   THA BIFA 2  1512.5
      45   2012 FLA KEYS  FMLR    0                041U   THA BIFA 2  1250.0
      46   2012 FLA KEYS  FMLR    0                046H   THA BIFA 1      NA
      47   2012 FLA KEYS  FMLR    0                046U   THA BIFA 1      NA
      48   2012 FLA KEYS  FMLR    0                047U   THA BIFA 2    12.5
      49   2012 FLA KEYS  FMLR    0                051H   THA BIFA 1      NA
      50   2012 FLA KEYS  FMLR    0                053U   THA BIFA 2    72.0
      51   2012 FLA KEYS  FMLR    0                055H   THA BIFA 1      NA
      52   2012 FLA KEYS  FMLR    0                056U   THA BIFA 2  1740.5
      53   2012 FLA KEYS  FMLR    0                057U   THA BIFA 2     8.0
      54   2012 FLA KEYS  FMLR    0                058U   THA BIFA 2   450.0
      55   2012 FLA KEYS  FMLR    0                059U   THA BIFA 2  1800.0
      56   2012 FLA KEYS  FMLR    0                061H   THA BIFA 1      NA
      57   2012 FLA KEYS  FMLR    0                061U   THA BIFA 1      NA
      58   2012 FLA KEYS  FMLR    0                062U   THA BIFA 2   144.5
      59   2012 FLA KEYS  FMLR    0                063U   THA BIFA 2    32.0
      60   2012 FLA KEYS  FMLR    0                064U   THA BIFA 2   968.0
      61   2012 FLA KEYS  FMLR    0                068U   THA BIFA 2     0.0
      62   2012 FLA KEYS  FMLR    0                069U   THA BIFA 2  2812.5
      63   2012 FLA KEYS  FMLR    0                070U   THA BIFA 2    32.0
      64   2012 FLA KEYS  FMLR    0                071U   THA BIFA 2  2812.5
      65   2012 FLA KEYS  FMLR    0                072U   THA BIFA 2   338.0
      66   2012 FLA KEYS  FMLR    0                073U   THA BIFA 2   338.0
      67   2012 FLA KEYS  FMLR    0                075U   THA BIFA 2     0.5
      68   2012 FLA KEYS  FMLR    0                076U   THA BIFA 2    18.0
      69   2012 FLA KEYS  FMLR    0                078U   THA BIFA 2    12.5
      70   2012 FLA KEYS  FMLR    0                079U   THA BIFA 2   364.5
      71   2012 FLA KEYS  FMLR    0                080U   THA BIFA 2   612.5
      72   2012 FLA KEYS  FMLR    0                082U   THA BIFA 2   128.0
      73   2012 FLA KEYS  FMLR    0                083U   THA BIFA 2    84.5
      74   2012 FLA KEYS  FMLR    0                084U   THA BIFA 2  1404.5
      75   2012 FLA KEYS  FMLR    0                085U   THA BIFA 2   420.5
      76   2012 FLA KEYS  FMLR    0                086U   THA BIFA 2   578.0
      77   2012 FLA KEYS  FMLR    0                088U   THA BIFA 2    18.0
      78   2012 FLA KEYS  FMLR    0                089U   THA BIFA 2  1104.5
      79   2012 FLA KEYS  FMLR    0                107U   THA BIFA 2 11552.0
      80   2012 FLA KEYS  FMLR    0                116U   THA BIFA 2    40.5
      81   2012 FLA KEYS  FMLR    0                118U   THA BIFA 2    12.5
      82   2012 FLA KEYS  FMLR    0                121U   THA BIFA 2     4.5
      83   2012 FLA KEYS  FMLR    0                122U   THA BIFA 2   242.0
      84   2012 FLA KEYS  FMLR    0                123U   THA BIFA 2    18.0
      85   2012 FLA KEYS  FMLR    0                124U   THA BIFA 2   544.5
      86   2012 FLA KEYS  FMLR    0                125U   THA BIFA 2    98.0
      87   2012 FLA KEYS  FMLR    0                126U   THA BIFA 2    50.0
      88   2012 FLA KEYS  FMLR    0                127U   THA BIFA 2   968.0
      89   2012 FLA KEYS  FMLR    0                130U   THA BIFA 2    50.0
      90   2012 FLA KEYS  FMLR    0                132U   THA BIFA 2   312.5
      91   2012 FLA KEYS  FMLR    0                133U   THA BIFA 2   544.5
      92   2012 FLA KEYS  FMLR    0                134U   THA BIFA 2    98.0
      93   2012 FLA KEYS  FMLR    0                135U   THA BIFA 2    32.0
      94   2012 FLA KEYS  FMLR    0                136U   THA BIFA 2  1352.0
      95   2012 FLA KEYS  FMLR    0                137U   THA BIFA 2   128.0
      96   2012 FLA KEYS  FMLR    0                138U   THA BIFA 2    12.5
      97   2012 FLA KEYS  FMLR    0                139U   THA BIFA 2    32.0
      98   2012 FLA KEYS  FMLR    0                141U   THA BIFA 2  1200.5
      99   2012 FLA KEYS  FMLR    0                142U   THA BIFA 2    18.0
      100  2012 FLA KEYS  FMLR    0                143U   THA BIFA 2   722.0
      101  2012 FLA KEYS  FMLR    0                144U   THA BIFA 2   338.0
      102  2012 FLA KEYS  FMLR    0                145U   THA BIFA 2     8.0
      103  2012 FLA KEYS  FMLR    0                146U   THA BIFA 2   144.5
      104  2012 FLA KEYS  FMLR    0                147U   THA BIFA 2     4.5
      105  2012 FLA KEYS  FMLR    0                148U   THA BIFA 2   722.0
      106  2012 FLA KEYS  FMLR    0                149U   THA BIFA 2  1250.0
      107  2012 FLA KEYS  FMLR    0                150U   THA BIFA 2   242.0
      108  2012 FLA KEYS  FMLR    0                151U   THA BIFA 2   264.5
      109  2012 FLA KEYS  FMLR    0                152U   THA BIFA 2   648.0
      110  2012 FLA KEYS  FMLR    0                154U   THA BIFA 2   968.0
      111  2012 FLA KEYS  FMLR    0                155U   THA BIFA 2    98.0
      112  2012 FLA KEYS  FMLR    0                158U   THA BIFA 2   722.0
      113  2012 FLA KEYS  FMLR    0                159U   THA BIFA 2     8.0
      114  2012 FLA KEYS  FMLR    0                210U   THA BIFA 2   162.0
      115  2012 FLA KEYS  FMLR    0                211U   THA BIFA 2    32.0
      116  2012 FLA KEYS  FMLR    0                212U   THA BIFA 2    84.5
      117  2012 FLA KEYS  FMLR    0                216U   THA BIFA 2     8.0
      118  2012 FLA KEYS  FMLR    0                217U   THA BIFA 2   544.5
      119  2012 FLA KEYS  FMLR    0                218U   THA BIFA 2   512.0
      120  2012 FLA KEYS  FMLR    0                219U   THA BIFA 2   264.5
      121  2012 FLA KEYS  FMLR    0                220U   THA BIFA 2   128.0
      122  2012 FLA KEYS  FMLR    0                222U   THA BIFA 2  1300.5
      123  2012 FLA KEYS  FMLR    0                223U   THA BIFA 2     0.0
      124  2012 FLA KEYS  FMLR    0                224U   THA BIFA 2    18.0
      125  2012 FLA KEYS  FMLR    0                225U   THA BIFA 2     8.0
      126  2012 FLA KEYS  FMLR    0                227U   THA BIFA 1      NA
      127  2012 FLA KEYS  FMLR    0                228U   THA BIFA 2   544.5
      128  2012 FLA KEYS  FMLR    0                229U   THA BIFA 2   760.5
      129  2012 FLA KEYS  FMLR    0                231U   THA BIFA 2    60.5
      130  2012 FLA KEYS  FMLR    0                232U   THA BIFA 2  3784.5
      131  2012 FLA KEYS  FMLR    0                233U   THA BIFA 2   648.0
      132  2012 FLA KEYS  FMLR    0                236U   THA BIFA 2   264.5
      133  2012 FLA KEYS  FMLR    0                245U   THA BIFA 2  1682.0
      134  2012 FLA KEYS  FMLR    0                289U   THA BIFA 2     8.0
      135  2012 FLA KEYS  FMLR    0                293U   THA BIFA 2  2664.5
      136  2012 FLA KEYS  FMLR    0                295U   THA BIFA 2 18050.0
      137  2012 FLA KEYS  FMLR    0                297U   THA BIFA 2    40.5
      138  2012 FLA KEYS  FMLR    0                300U   THA BIFA 2   128.0
      139  2012 FLA KEYS  FMLR    0                301U   THA BIFA 2  1860.5
      140  2012 FLA KEYS  FMLR    0                303U   THA BIFA 2   144.5
      141  2012 FLA KEYS  FMLR    0                305U   THA BIFA 2   578.0
      142  2012 FLA KEYS  FMLR    0                306U   THA BIFA 2  1740.5
      143  2012 FLA KEYS  FMLR    0                307U   THA BIFA 2    50.0
      144  2012 FLA KEYS  FMLR    0                310U   THA BIFA 2    24.5
      145  2012 FLA KEYS  FMLR    0                311U   THA BIFA 2   924.5
      146  2012 FLA KEYS  FMLR    0                312U   THA BIFA 2     2.0
      147  2012 FLA KEYS  FMLR    0                314U   THA BIFA 2   144.5
      148  2012 FLA KEYS  FMLR    0                315U   THA BIFA 2    18.0
      149  2012 FLA KEYS  FMLR    0                316U   THA BIFA 2    32.0
      150  2012 FLA KEYS  FMLR    0                317U   THA BIFA 2   840.5
      151  2012 FLA KEYS  FMLR    0                319U   THA BIFA 2   162.0
      152  2012 FLA KEYS  FMLR    0                321U   THA BIFA 2  1104.5
      153  2012 FLA KEYS  FMLR    0                322U   THA BIFA 2   144.5
      154  2012 FLA KEYS  FMLR    0                324U   THA BIFA 2    32.0
      155  2012 FLA KEYS  FMLR    0                325U   THA BIFA 2   722.0
      156  2012 FLA KEYS  FMLR    0                326U   THA BIFA 2     2.0
      157  2012 FLA KEYS  FMLR    0                855U   THA BIFA 2    24.5
      158  2012 FLA KEYS  FMLR    0                856U   THA BIFA 2   800.0
      159  2012 FLA KEYS  FMLR    0                889U   THA BIFA 2   684.5
      160  2012 FLA KEYS  FMLR    0                898U   THA BIFA 2   882.0
      161  2012 FLA KEYS  FMLR    0                899U   THA BIFA 2     2.0
      162  2012 FLA KEYS  FMLR    0                901H   THA BIFA 1      NA
      163  2012 FLA KEYS  FMLR    0                901U   THA BIFA 1      NA
      164  2012 FLA KEYS  FMLR    0                910U   THA BIFA 2    60.5
      165  2012 FLA KEYS  FMLR    0                938U   THA BIFA 2   312.5
      166  2012 FLA KEYS  FMLR    0                941U   THA BIFA 2   288.0
      167  2012 FLA KEYS  FMLR    0                988H   THA BIFA 1      NA
      168  2012 FLA KEYS  FMLR    0                988U   THA BIFA 1      NA
      169  2012 FLA KEYS  FMLR    0                A03U   THA BIFA 2    18.0
      170  2012 FLA KEYS  FMLR    1                248U   THA BIFA 2   760.5
      171  2012 FLA KEYS  FMLR    1                342U   THA BIFA 2   144.5
      172  2012 FLA KEYS  FMLR    1                343U   THA BIFA 2   200.0
      173  2012 FLA KEYS  FMLR    1                347U   THA BIFA 2    12.5
      174  2012 FLA KEYS  FMLR    1                349U   THA BIFA 2   722.0
      175  2012 FLA KEYS  FMLR    1                359U   THA BIFA 2   162.0
      176  2012 FLA KEYS  FMLR    1                360U   THA BIFA 2  7442.0
      177  2012 FLA KEYS  FMLR    1                365U   THA BIFA 2     0.5
      178  2012 FLA KEYS  FMLR    1                366U   THA BIFA 2   220.5
      179  2012 FLA KEYS  FMLR    1                367U   THA BIFA 2    98.0
      180  2012 FLA KEYS  FMLR    1                368U   THA BIFA 2    72.0
      181  2012 FLA KEYS  FMLR    1                370U   THA BIFA 2   220.5
      182  2012 FLA KEYS  FMLR    1                371U   THA BIFA 2     4.5
      183  2012 FLA KEYS  FMLR    1                380U   THA BIFA 2  1568.0
      184  2012 FLA KEYS  FMLR    1                381U   THA BIFA 2    60.5
      185  2012 FLA KEYS  FMLR    1                382U   THA BIFA 2   480.5
      186  2012 FLA KEYS  FMLR    1                383U   THA BIFA 2    60.5
      187  2012 FLA KEYS  FMLR    1                385U   THA BIFA 2  5202.0
      188  2012 FLA KEYS  FMLR    1                386U   THA BIFA 2    50.0
      189  2012 FLA KEYS  FMLR    1                387U   THA BIFA 2   144.5
      190  2012 FLA KEYS  FMLR    1                389U   THA BIFA 2   288.0
      191  2012 FLA KEYS  FMLR    1                390U   THA BIFA 2    40.5
      192  2012 FLA KEYS  FMLR    1                391U   THA BIFA 2   882.0
      193  2012 FLA KEYS  FMLR    1                395U   THA BIFA 2   800.0
      194  2012 FLA KEYS  FMLR    1                396U   THA BIFA 2     8.0
      195  2012 FLA KEYS  FMLR    1                398U   THA BIFA 2    24.5
      196  2012 FLA KEYS  FMLR    1                400U   THA BIFA 2     0.5
      197  2012 FLA KEYS  FMLR    1                401U   THA BIFA 2   180.5
      198  2012 FLA KEYS  FMLR    1                406U   THA BIFA 2     8.0
      199  2012 FLA KEYS  FMLR    1                407U   THA BIFA 2   264.5
      200  2012 FLA KEYS  FMLR    1                421U   THA BIFA 2    24.5
      201  2012 FLA KEYS  FMLR    1                422U   THA BIFA 2    40.5
      202  2012 FLA KEYS  FMLR    1                425H   THA BIFA 1      NA
      203  2012 FLA KEYS  FMLR    1                428U   THA BIFA 1      NA
      204  2012 FLA KEYS  FMLR    1                431U   THA BIFA 2  1860.5
      205  2012 FLA KEYS  FMLR    1                432U   THA BIFA 2  5512.5
      206  2012 FLA KEYS  FMLR    1                433U   THA BIFA 2    24.5
      207  2012 FLA KEYS  FMLR    1                434U   THA BIFA 2     4.5
      208  2012 FLA KEYS  FMLR    1                435U   THA BIFA 2   180.5
      209  2012 FLA KEYS  FMLR    1                437U   THA BIFA 2    60.5
      210  2012 FLA KEYS  FMLR    1                438U   THA BIFA 2   800.0
      211  2012 FLA KEYS  FMLR    1                439U   THA BIFA 2  1152.0
      212  2012 FLA KEYS  FMLR    1                440U   THA BIFA 2    98.0
      213  2012 FLA KEYS  FSLR    0                037U   THA BIFA 2     4.5
      214  2012 FLA KEYS  FSLR    0                038U   THA BIFA 2   612.5
      215  2012 FLA KEYS  FSLR    0                039U   THA BIFA 2   312.5
      216  2012 FLA KEYS  FSLR    0                043U   THA BIFA 2    98.0
      217  2012 FLA KEYS  FSLR    0                044U   THA BIFA 2   242.0
      218  2012 FLA KEYS  FSLR    0                045U   THA BIFA 2  8450.0
      219  2012 FLA KEYS  FSLR    0                048U   THA BIFA 2  2112.5
      220  2012 FLA KEYS  FSLR    0                049U   THA BIFA 2   800.0
      221  2012 FLA KEYS  FSLR    0                052U   THA BIFA 2   264.5
      222  2012 FLA KEYS  FSLR    0                110U   THA BIFA 2   420.5
      223  2012 FLA KEYS  FSLR    0                113U   THA BIFA 2     0.0
      224  2012 FLA KEYS  FSLR    0                183U   THA BIFA 2   264.5
      225  2012 FLA KEYS  FSLR    0                202U   THA BIFA 2   162.0
      226  2012 FLA KEYS  FSLR    0                203U   THA BIFA 2    18.0
      227  2012 FLA KEYS  FSLR    0                204U   THA BIFA 2   364.5
      228  2012 FLA KEYS  FSLR    0                205U   THA BIFA 2   312.5
      229  2012 FLA KEYS  FSLR    0                206U   THA BIFA 2   338.0
      230  2012 FLA KEYS  FSLR    0                207U   THA BIFA 2    60.5
      231  2012 FLA KEYS  FSLR    0                208U   THA BIFA 2    72.0
      232  2012 FLA KEYS  FSLR    0                287U   THA BIFA 2   578.0
      233  2012 FLA KEYS  FSLR    0                288U   THA BIFA 2    72.0
      234  2012 FLA KEYS  FSLR    0                290U   THA BIFA 2   312.5
      235  2012 FLA KEYS  FSLR    0                292U   THA BIFA 2  1152.0
      236  2012 FLA KEYS  FSLR    0                302U   THA BIFA 2    98.0
      237  2012 FLA KEYS  FSLR    0                304U   THA BIFA 2  2244.5
      238  2012 FLA KEYS  FSLR    0                308U   THA BIFA 2  3120.5
      239  2012 FLA KEYS  FSLR    0                309U   THA BIFA 2  1800.0
      240  2012 FLA KEYS  FSLR    0                393U   THA BIFA 2    98.0
      241  2012 FLA KEYS  FSLR    0                883U   THA BIFA 2   312.5
      242  2012 FLA KEYS  FSLR    1                341H   THA BIFA 1      NA
      243  2012 FLA KEYS  FSLR    1                341U   THA BIFA 1      NA
      244  2012 FLA KEYS  FSLR    1                344U   THA BIFA 2   392.0
      245  2012 FLA KEYS  FSLR    1                348H   THA BIFA 1      NA
      246  2012 FLA KEYS  FSLR    1                351U   THA BIFA 2  4608.0
      247  2012 FLA KEYS  FSLR    1                352U   THA BIFA 2    12.5
      248  2012 FLA KEYS  FSLR    1                355U   THA BIFA 2    32.0
      249  2012 FLA KEYS  FSLR    1                358U   THA BIFA 2    24.5
      250  2012 FLA KEYS  FSLR    1                361U   THA BIFA 2    60.5
      251  2012 FLA KEYS  FSLR    1                369U   THA BIFA 2   544.5
      252  2012 FLA KEYS  FSLR    1                388U   THA BIFA 2   200.0
      253  2012 FLA KEYS  FSLR    1                392H   THA BIFA 1      NA
      254  2012 FLA KEYS  FSLR    1                392U   THA BIFA 1      NA
      255  2012 FLA KEYS  FSLR    1                399U   THA BIFA 2   112.5
      256  2012 FLA KEYS  FSLR    1                402U   THA BIFA 2   162.0
      257  2012 FLA KEYS  FSLR    1                403U   THA BIFA 2     2.0
      258  2012 FLA KEYS  FSLR    1                405U   THA BIFA 2    24.5
      259  2012 FLA KEYS  FSLR    1                419U   THA BIFA 2    72.0
      260  2012 FLA KEYS  FSLR    1                420U   THA BIFA 2   760.5
      261  2012 FLA KEYS  FSLR    1                429U   THA BIFA 2   512.0
      262  2012 FLA KEYS  FSLR    1                A89U   THA BIFA 2  1984.5
      263  2012 FLA KEYS  FSLR    1                A90U   THA BIFA 2  1012.5
      264  2012 FLA KEYS  HRRF    0                042U   THA BIFA 2     8.0
      265  2012 FLA KEYS  HRRF    0                050U   THA BIFA 2 54450.0
      266  2012 FLA KEYS  HRRF    0                051U   THA BIFA 1      NA
      267  2012 FLA KEYS  HRRF    0                067U   THA BIFA 2  3042.0
      268  2012 FLA KEYS  HRRF    0                196U   THA BIFA 2    84.5
      269  2012 FLA KEYS  HRRF    0                198U   THA BIFA 2    32.0
      270  2012 FLA KEYS  HRRF    0                215U   THA BIFA 2     4.5
      271  2012 FLA KEYS  HRRF    0                294U   THA BIFA 2  8320.5
      272  2012 FLA KEYS  HRRF    0                957U   THA BIFA 2    32.0
      273  2012 FLA KEYS  HRRF    1                346U   THA BIFA 2   800.0
      274  2012 FLA KEYS  HRRF    1                348U   THA BIFA 1      NA
      275  2012 FLA KEYS  HRRF    1                350U   THA BIFA 2   480.5
      276  2012 FLA KEYS  HRRF    1                353U   THA BIFA 2   882.0
      277  2012 FLA KEYS  HRRF    1                354U   THA BIFA 2  1458.0
      278  2012 FLA KEYS  HRRF    1                356U   THA BIFA 2    12.5
      279  2012 FLA KEYS  HRRF    1                357U   THA BIFA 2  1152.0
      280  2012 FLA KEYS  HRRF    1                394U   THA BIFA 2   144.5
      281  2012 FLA KEYS  HRRF    1                404U   THA BIFA 2   264.5
      282  2012 FLA KEYS  HRRF    1                418H   THA BIFA 1      NA
      283  2012 FLA KEYS  HRRF    1                423U   THA BIFA 2   288.0
      284  2012 FLA KEYS  HRRF    1                424U   THA BIFA 2   684.5
      285  2012 FLA KEYS  HRRF    1                425U   THA BIFA 1      NA
      286  2012 FLA KEYS  HRRF    1                426U   THA BIFA 2  6384.5
      287  2012 FLA KEYS  HRRF    1                427U   THA BIFA 2    84.5
      288  2012 FLA KEYS  HRRF    1                428H   THA BIFA 1      NA
      289  2012 FLA KEYS  HRRF    1                430U   THA BIFA 2   220.5
      290  2012 FLA KEYS  INPR    0                091U   THA BIFA 2     0.0
      291  2012 FLA KEYS  INPR    0                092U   THA BIFA 2     4.5
      292  2012 FLA KEYS  INPR    0                093U   THA BIFA 2     2.0
      293  2012 FLA KEYS  INPR    0                250U   THA BIFA 2     0.0
      294  2012 FLA KEYS  INPR    0                253U   THA BIFA 2   264.5
      295  2012 FLA KEYS  INPR    0                257U   THA BIFA 2     8.0
      296  2012 FLA KEYS  INPR    1                327U   THA BIFA 2     0.0
      297  2012 FLA KEYS  INPR    1                329U   THA BIFA 2    32.0
      298  2012 FLA KEYS  INPR    1                330U   THA BIFA 2     0.0
      299  2012 FLA KEYS  MCPR    0                005U   THA BIFA 2    32.0
      300  2012 FLA KEYS  MCPR    0                006U   THA BIFA 2     0.0
      301  2012 FLA KEYS  MCPR    0                008U   THA BIFA 2     2.0
      302  2012 FLA KEYS  MCPR    0                009U   THA BIFA 2    18.0
      303  2012 FLA KEYS  MCPR    0                010U   THA BIFA 2   264.5
      304  2012 FLA KEYS  MCPR    0                011U   THA BIFA 2   392.0
      305  2012 FLA KEYS  MCPR    0                012U   THA BIFA 2    18.0
      306  2012 FLA KEYS  MCPR    0                013U   THA BIFA 2     0.0
      307  2012 FLA KEYS  MCPR    0                023U   THA BIFA 2   112.5
      308  2012 FLA KEYS  MCPR    0                024U   THA BIFA 2   512.0
      309  2012 FLA KEYS  MCPR    0                025U   THA BIFA 2     8.0
      310  2012 FLA KEYS  MCPR    0                095U   THA BIFA 2   420.5
      311  2012 FLA KEYS  MCPR    0                096U   THA BIFA 2    84.5
      312  2012 FLA KEYS  MCPR    0                097U   THA BIFA 2     8.0
      313  2012 FLA KEYS  MCPR    0                098U   THA BIFA 2   200.0
      314  2012 FLA KEYS  MCPR    0                099U   THA BIFA 2     0.0
      315  2012 FLA KEYS  MCPR    0                101U   THA BIFA 2     4.5
      316  2012 FLA KEYS  MCPR    0                164U   THA BIFA 2   544.5
      317  2012 FLA KEYS  MCPR    0                166U   THA BIFA 2    72.0
      318  2012 FLA KEYS  MCPR    0                167U   THA BIFA 2    24.5
      319  2012 FLA KEYS  MCPR    0                170U   THA BIFA 2     0.5
      320  2012 FLA KEYS  MCPR    0                171U   THA BIFA 2     0.5
      321  2012 FLA KEYS  MCPR    0                172U   THA BIFA 2     0.5
      322  2012 FLA KEYS  MCPR    0                173U   THA BIFA 2     0.0
      323  2012 FLA KEYS  MCPR    0                174U   THA BIFA 2     0.5
      324  2012 FLA KEYS  MCPR    0                175U   THA BIFA 2     0.0
      325  2012 FLA KEYS  MCPR    0                177U   THA BIFA 2    32.0
      326  2012 FLA KEYS  MCPR    0                178U   THA BIFA 2    18.0
      327  2012 FLA KEYS  MCPR    0                179U   THA BIFA 2    60.5
      328  2012 FLA KEYS  MCPR    0                180U   THA BIFA 2     8.0
      329  2012 FLA KEYS  MCPR    0                258U   THA BIFA 2     0.0
      330  2012 FLA KEYS  MCPR    0                259U   THA BIFA 2    18.0
      331  2012 FLA KEYS  MCPR    0                260U   THA BIFA 2    18.0
      332  2012 FLA KEYS  MCPR    0                261U   THA BIFA 2     4.5
      333  2012 FLA KEYS  MCPR    0                262U   THA BIFA 2    60.5
      334  2012 FLA KEYS  MCPR    0                263U   THA BIFA 2   512.0
      335  2012 FLA KEYS  MCPR    0                264U   THA BIFA 2   392.0
      336  2012 FLA KEYS  MCPR    0                265U   THA BIFA 2    32.0
      337  2012 FLA KEYS  MCPR    0                266U   THA BIFA 2     4.5
      338  2012 FLA KEYS  MCPR    0                267U   THA BIFA 2   144.5
      339  2012 FLA KEYS  MCPR    0                268U   THA BIFA 2     2.0
      340  2012 FLA KEYS  MCPR    0                269U   THA BIFA 2  1352.0
      341  2012 FLA KEYS  MCPR    0                270U   THA BIFA 2    24.5
      342  2012 FLA KEYS  MCPR    0                271U   THA BIFA 2    18.0
      343  2012 FLA KEYS  MCPR    0                272U   THA BIFA 2     2.0
      344  2012 FLA KEYS  MCPR    0                273U   THA BIFA 2     0.5
      345  2012 FLA KEYS  MCPR    0                274U   THA BIFA 2     8.0
      346  2012 FLA KEYS  MCPR    0                275U   THA BIFA 2    98.0
      347  2012 FLA KEYS  MCPR    0                276U   THA BIFA 2   312.5
      348  2012 FLA KEYS  MCPR    0                277U   THA BIFA 2    40.5
      349  2012 FLA KEYS  MCPR    0                279U   THA BIFA 2    72.0
      350  2012 FLA KEYS  MCPR    0                280U   THA BIFA 2    12.5
      351  2012 FLA KEYS  MCPR    0                281U   THA BIFA 2   288.0
      352  2012 FLA KEYS  MCPR    0                282U   THA BIFA 2    12.5
      353  2012 FLA KEYS  MCPR    0                414U   THA BIFA 2   882.0
      354  2012 FLA KEYS  MCPR    0                834U   THA BIFA 2    32.0
      355  2012 FLA KEYS  MCPR    0                876U   THA BIFA 2   112.5
      356  2012 FLA KEYS  MCPR    0                917U   THA BIFA 2     0.0
      357  2012 FLA KEYS  MCPR    0                974U   THA BIFA 2   760.5
      358  2012 FLA KEYS  MCPR    0                980U   THA BIFA 2   112.5
      359  2012 FLA KEYS  MCPR    1                331U   THA BIFA 2     0.0
      360  2012 FLA KEYS  MCPR    1                332U   THA BIFA 2   512.0
      361  2012 FLA KEYS  MCPR    1                333U   THA BIFA 2   180.5
      362  2012 FLA KEYS  MCPR    1                334U   THA BIFA 2    40.5
      363  2012 FLA KEYS  MCPR    1                372U   THA BIFA 2   288.0
      364  2012 FLA KEYS  MCPR    1                373U   THA BIFA 2   128.0
      365  2012 FLA KEYS  MCPR    1                374U   THA BIFA 2     8.0
      366  2012 FLA KEYS  MCPR    1                375U   THA BIFA 2     0.0
      367  2012 FLA KEYS  MCPR    1                376U   THA BIFA 2     0.5
      368  2012 FLA KEYS  MCPR    1                377U   THA BIFA 2     8.0
      369  2012 FLA KEYS  MCPR    1                378U   THA BIFA 2     2.0
      370  2012 FLA KEYS  MCPR    1                379U   THA BIFA 2     8.0
      371  2012 FLA KEYS  OFPR    0                027U   THA BIFA 2     0.5
      372  2012 FLA KEYS  OFPR    0                029U   THA BIFA 2     0.0
      373  2012 FLA KEYS  OFPR    0                030U   THA BIFA 2   420.5
      374  2012 FLA KEYS  OFPR    0                031U   THA BIFA 2    24.5
      375  2012 FLA KEYS  OFPR    0                033U   THA BIFA 2     8.0
      376  2012 FLA KEYS  OFPR    0                034U   THA BIFA 2    18.0
      377  2012 FLA KEYS  OFPR    0                181U   THA BIFA 2   220.5
      378  2012 FLA KEYS  OFPR    0                182U   THA BIFA 2     0.0
      379  2012 FLA KEYS  OFPR    0                185U   THA BIFA 2   220.5
      380  2012 FLA KEYS  OFPR    0                186U   THA BIFA 2   112.5
      381  2012 FLA KEYS  OFPR    0                187U   THA BIFA 2     4.5
      382  2012 FLA KEYS  OFPR    0                188U   THA BIFA 2   180.5
      383  2012 FLA KEYS  OFPR    0                189U   THA BIFA 2    72.0
      384  2012 FLA KEYS  OFPR    0                190U   THA BIFA 2    32.0
      385  2012 FLA KEYS  OFPR    0                191U   THA BIFA 2    72.0
      386  2012 FLA KEYS  OFPR    0                192U   THA BIFA 2   480.5
      387  2012 FLA KEYS  OFPR    0                193U   THA BIFA 2     0.0
      388  2012 FLA KEYS  OFPR    0                194U   THA BIFA 2     4.5
      389  2012 FLA KEYS  OFPR    0                195U   THA BIFA 2   512.0
      390  2012 FLA KEYS  OFPR    0                197U   THA BIFA 2     0.5
      391  2012 FLA KEYS  OFPR    0                199U   THA BIFA 2     0.5
      392  2012 FLA KEYS  OFPR    0                200U   THA BIFA 2     2.0
      393  2012 FLA KEYS  OFPR    0                201U   THA BIFA 2    18.0
      394  2012 FLA KEYS  OFPR    0                283U   THA BIFA 2    98.0
      395  2012 FLA KEYS  OFPR    0                284U   THA BIFA 2    98.0
      396  2012 FLA KEYS  OFPR    0                285U   THA BIFA 2  1250.0
      397  2012 FLA KEYS  OFPR    0                286U   THA BIFA 2  3960.5
      398  2012 FLA KEYS  OFPR    0                879U   THA BIFA 2   392.0
      399  2012 FLA KEYS  OFPR    0                925U   THA BIFA 2    24.5
      400  2012 FLA KEYS  OFPR    1                336U   THA BIFA 2   392.0
      401  2012 FLA KEYS  OFPR    1                337U   THA BIFA 2   264.5
      402  2012 FLA KEYS  OFPR    1                338U   THA BIFA 2    32.0
      403  2012 FLA KEYS  OFPR    1                339U   THA BIFA 2   264.5
      404  2012 FLA KEYS  OFPR    1                340U   THA BIFA 2   800.0
      405  2012 FLA KEYS  OFPR    1                408U   THA BIFA 2   144.5
      406  2012 FLA KEYS  OFPR    1                409U   THA BIFA 2   220.5
      407  2012 FLA KEYS  OFPR    1                410U   THA BIFA 2    98.0
      408  2012 FLA KEYS  OFPR    1                411U   THA BIFA 2   312.5
      409  2012 FLA KEYS  OFPR    1                412U   THA BIFA 2    98.0
      410  2012 FLA KEYS  OFPR    1                413U   THA BIFA 2   338.0
      411  2012 FLA KEYS  OFPR    1                415U   THA BIFA 2   112.5
      412  2012 FLA KEYS  OFPR    1                416U   THA BIFA 2    50.0
      413  2012 FLA KEYS  OFPR    1                417U   THA BIFA 2   180.5
      414  2012 FLA KEYS  OFPR    1                418U   THA BIFA 1      NA
      415  2012 FLA KEYS  OFPR    1                835U   THA BIFA 2    18.0
      416  2012 FLA KEYS  OFPR    1                A24U   THA BIFA 2    72.0
      417  2022 FLA KEYS   S01    0                1004   THA BIFA 1      NA
      418  2022 FLA KEYS   S01    0                1005   THA BIFA 1      NA
      419  2022 FLA KEYS   S01    0                1006   THA BIFA 1      NA
      420  2022 FLA KEYS   S01    0                1010   THA BIFA 1      NA
      421  2022 FLA KEYS   S01    0                1011   THA BIFA 1      NA
      422  2022 FLA KEYS   S01    0                1365   THA BIFA 1      NA
      423  2022 FLA KEYS   S01    0                1490   THA BIFA 1      NA
      424  2022 FLA KEYS   S01    0                1491   THA BIFA 1      NA
      425  2022 FLA KEYS   S01    0                1963   THA BIFA 1      NA
      426  2022 FLA KEYS   S01    0                8069   THA BIFA 1      NA
      427  2022 FLA KEYS   S01    0                8071   THA BIFA 1      NA
      428  2022 FLA KEYS   S01    1                1002   THA BIFA 1      NA
      429  2022 FLA KEYS   S01    1                1367   THA BIFA 1      NA
      430  2022 FLA KEYS   S01    1                1368   THA BIFA 1      NA
      431  2022 FLA KEYS   S01    1                1369   THA BIFA 1      NA
      432  2022 FLA KEYS   S01    1                1849   THA BIFA 1      NA
      433  2022 FLA KEYS   S01    1                9003   THA BIFA 1      NA
      434  2022 FLA KEYS   S02    0                1015   THA BIFA 1      NA
      435  2022 FLA KEYS   S02    0                1018   THA BIFA 1      NA
      436  2022 FLA KEYS   S02    0                1023   THA BIFA 1      NA
      437  2022 FLA KEYS   S02    0                1025   THA BIFA 1      NA
      438  2022 FLA KEYS   S02    0                1039   THA BIFA 1      NA
      439  2022 FLA KEYS   S02    0                1072   THA BIFA 1      NA
      440  2022 FLA KEYS   S02    0                1077   THA BIFA 1      NA
      441  2022 FLA KEYS   S02    0                1497   THA BIFA 1      NA
      442  2022 FLA KEYS   S02    0                1514   THA BIFA 1      NA
      443  2022 FLA KEYS   S02    0                1556   THA BIFA 1      NA
      444  2022 FLA KEYS   S02    0                1962   THA BIFA 1      NA
      445  2022 FLA KEYS   S02    0                1966   THA BIFA 1      NA
      446  2022 FLA KEYS   S02    0                1968   THA BIFA 1      NA
      447  2022 FLA KEYS   S02    0                1970   THA BIFA 1      NA
      448  2022 FLA KEYS   S02    0                1972   THA BIFA 1      NA
      449  2022 FLA KEYS   S02    0                1974   THA BIFA 1      NA
      450  2022 FLA KEYS   S02    0                1978   THA BIFA 1      NA
      451  2022 FLA KEYS   S02    1                1372   THA BIFA 1      NA
      452  2022 FLA KEYS   S02    1                1373   THA BIFA 1      NA
      453  2022 FLA KEYS   S02    1                1374   THA BIFA 1      NA
      454  2022 FLA KEYS   S02    1                1857   THA BIFA 1      NA
      455  2022 FLA KEYS   S03    0                1016   THA BIFA 1      NA
      456  2022 FLA KEYS   S03    0                1020   THA BIFA 1      NA
      457  2022 FLA KEYS   S03    0                1022   THA BIFA 1      NA
      458  2022 FLA KEYS   S03    0                1026   THA BIFA 1      NA
      459  2022 FLA KEYS   S03    0                1027   THA BIFA 1      NA
      460  2022 FLA KEYS   S03    0                1028   THA BIFA 1      NA
      461  2022 FLA KEYS   S03    0                1030   THA BIFA 1      NA
      462  2022 FLA KEYS   S03    0                1031   THA BIFA 1      NA
      463  2022 FLA KEYS   S03    0                1032   THA BIFA 1      NA
      464  2022 FLA KEYS   S03    0                1033   THA BIFA 1      NA
      465  2022 FLA KEYS   S03    0                1035   THA BIFA 1      NA
      466  2022 FLA KEYS   S03    0                1037   THA BIFA 1      NA
      467  2022 FLA KEYS   S03    0                1038   THA BIFA 1      NA
      468  2022 FLA KEYS   S03    0                1042   THA BIFA 1      NA
      469  2022 FLA KEYS   S03    0                1043   THA BIFA 1      NA
      470  2022 FLA KEYS   S03    0                1044   THA BIFA 1      NA
      471  2022 FLA KEYS   S03    0                1045   THA BIFA 1      NA
      472  2022 FLA KEYS   S03    0                1046   THA BIFA 1      NA
      473  2022 FLA KEYS   S03    0                1048   THA BIFA 1      NA
      474  2022 FLA KEYS   S03    0                1050   THA BIFA 1      NA
      475  2022 FLA KEYS   S03    0                1052   THA BIFA 1      NA
      476  2022 FLA KEYS   S03    0                1053   THA BIFA 1      NA
      477  2022 FLA KEYS   S03    0                1055   THA BIFA 1      NA
      478  2022 FLA KEYS   S03    0                1056   THA BIFA 1      NA
      479  2022 FLA KEYS   S03    0                1058   THA BIFA 1      NA
      480  2022 FLA KEYS   S03    0                1060   THA BIFA 1      NA
      481  2022 FLA KEYS   S03    0                1063   THA BIFA 1      NA
      482  2022 FLA KEYS   S03    0                1066   THA BIFA 1      NA
      483  2022 FLA KEYS   S03    0                1068   THA BIFA 1      NA
      484  2022 FLA KEYS   S03    0                1069   THA BIFA 1      NA
      485  2022 FLA KEYS   S03    0                1073   THA BIFA 1      NA
      486  2022 FLA KEYS   S03    0                1075   THA BIFA 1      NA
      487  2022 FLA KEYS   S03    0                1078   THA BIFA 1      NA
      488  2022 FLA KEYS   S03    0                1081   THA BIFA 1      NA
      489  2022 FLA KEYS   S03    0                1083   THA BIFA 1      NA
      490  2022 FLA KEYS   S03    0                1504   THA BIFA 1      NA
      491  2022 FLA KEYS   S03    0                1505   THA BIFA 1      NA
      492  2022 FLA KEYS   S03    0                1507   THA BIFA 1      NA
      493  2022 FLA KEYS   S03    0                1508   THA BIFA 1      NA
      494  2022 FLA KEYS   S03    0                1509   THA BIFA 1      NA
      495  2022 FLA KEYS   S03    0                1511   THA BIFA 1      NA
      496  2022 FLA KEYS   S03    0                1512   THA BIFA 1      NA
      497  2022 FLA KEYS   S03    0                1513   THA BIFA 1      NA
      498  2022 FLA KEYS   S03    0                1515   THA BIFA 1      NA
      499  2022 FLA KEYS   S03    0                1520   THA BIFA 1      NA
      500  2022 FLA KEYS   S03    0                1521   THA BIFA 1      NA
      501  2022 FLA KEYS   S03    0                1523   THA BIFA 1      NA
      502  2022 FLA KEYS   S03    0                1524   THA BIFA 1      NA
      503  2022 FLA KEYS   S03    0                1525   THA BIFA 1      NA
      504  2022 FLA KEYS   S03    0                1538   THA BIFA 1      NA
      505  2022 FLA KEYS   S03    0                1541   THA BIFA 1      NA
      506  2022 FLA KEYS   S03    0                1544   THA BIFA 1      NA
      507  2022 FLA KEYS   S03    0                1546   THA BIFA 1      NA
      508  2022 FLA KEYS   S03    0                1558   THA BIFA 1      NA
      509  2022 FLA KEYS   S03    0                1561   THA BIFA 1      NA
      510  2022 FLA KEYS   S03    0                1563   THA BIFA 1      NA
      511  2022 FLA KEYS   S03    0                1964   THA BIFA 1      NA
      512  2022 FLA KEYS   S03    0                1965   THA BIFA 1      NA
      513  2022 FLA KEYS   S03    0                1969   THA BIFA 1      NA
      514  2022 FLA KEYS   S03    0                1971   THA BIFA 1      NA
      515  2022 FLA KEYS   S03    0                1973   THA BIFA 1      NA
      516  2022 FLA KEYS   S03    0                1975   THA BIFA 1      NA
      517  2022 FLA KEYS   S03    0                1976   THA BIFA 1      NA
      518  2022 FLA KEYS   S03    0                1977   THA BIFA 1      NA
      519  2022 FLA KEYS   S03    0                1979   THA BIFA 1      NA
      520  2022 FLA KEYS   S03    0                1980   THA BIFA 1      NA
      521  2022 FLA KEYS   S03    0                1981   THA BIFA 1      NA
      522  2022 FLA KEYS   S03    0                1982   THA BIFA 1      NA
      523  2022 FLA KEYS   S03    0                1983   THA BIFA 1      NA
      524  2022 FLA KEYS   S03    0                1984   THA BIFA 1      NA
      525  2022 FLA KEYS   S03    0                1985   THA BIFA 1      NA
      526  2022 FLA KEYS   S03    0                1986   THA BIFA 1      NA
      527  2022 FLA KEYS   S03    0                1987   THA BIFA 1      NA
      528  2022 FLA KEYS   S03    0                1988   THA BIFA 1      NA
      529  2022 FLA KEYS   S03    0                9018   THA BIFA 1      NA
      530  2022 FLA KEYS   S03    0                9020   THA BIFA 1      NA
      531  2022 FLA KEYS   S03    0                9210   THA BIFA 1      NA
      532  2022 FLA KEYS   S03    0                9211   THA BIFA 1      NA
      533  2022 FLA KEYS   S03    0                9215   THA BIFA 1      NA
      534  2022 FLA KEYS   S03    1                1371   THA BIFA 1      NA
      535  2022 FLA KEYS   S03    1                1375   THA BIFA 1      NA
      536  2022 FLA KEYS   S03    1                1376   THA BIFA 1      NA
      537  2022 FLA KEYS   S03    1                1379   THA BIFA 1      NA
      538  2022 FLA KEYS   S03    1                1380   THA BIFA 1      NA
      539  2022 FLA KEYS   S03    1                1381   THA BIFA 1      NA
      540  2022 FLA KEYS   S03    1                1382   THA BIFA 1      NA
      541  2022 FLA KEYS   S03    1                1861   THA BIFA 1      NA
      542  2022 FLA KEYS   S03    1                9013   THA BIFA 1      NA
      543  2022 FLA KEYS   S03    1                9112   THA BIFA 1      NA
      544  2022 FLA KEYS   S03    1                9119   THA BIFA 1      NA
      545  2022 FLA KEYS   S03    1                9122   THA BIFA 1      NA
      546  2022 FLA KEYS   S05    0                1024   THA BIFA 1      NA
      547  2022 FLA KEYS   S05    0                1047   THA BIFA 1      NA
      548  2022 FLA KEYS   S05    0                1084   THA BIFA 1      NA
      549  2022 FLA KEYS   S05    0                1086   THA BIFA 1      NA
      550  2022 FLA KEYS   S05    0                1087   THA BIFA 1      NA
      551  2022 FLA KEYS   S05    0                1089   THA BIFA 1      NA
      552  2022 FLA KEYS   S05    0                1095   THA BIFA 1      NA
      553  2022 FLA KEYS   S05    0                1101   THA BIFA 1      NA
      554  2022 FLA KEYS   S05    0                1105   THA BIFA 1      NA
      555  2022 FLA KEYS   S05    0                1113   THA BIFA 1      NA
      556  2022 FLA KEYS   S05    0                1114   THA BIFA 1      NA
      557  2022 FLA KEYS   S05    0                1115   THA BIFA 1      NA
      558  2022 FLA KEYS   S05    0                1118   THA BIFA 1      NA
      559  2022 FLA KEYS   S05    0                1119   THA BIFA 1      NA
      560  2022 FLA KEYS   S05    0                1122   THA BIFA 1      NA
      561  2022 FLA KEYS   S05    0                1123   THA BIFA 1      NA
      562  2022 FLA KEYS   S05    0                1124   THA BIFA 1      NA
      563  2022 FLA KEYS   S05    0                1125   THA BIFA 1      NA
      564  2022 FLA KEYS   S05    0                1126   THA BIFA 1      NA
      565  2022 FLA KEYS   S05    0                1127   THA BIFA 1      NA
      566  2022 FLA KEYS   S05    0                1133   THA BIFA 1      NA
      567  2022 FLA KEYS   S05    0                1134   THA BIFA 1      NA
      568  2022 FLA KEYS   S05    0                1135   THA BIFA 1      NA
      569  2022 FLA KEYS   S05    0                1137   THA BIFA 1      NA
      570  2022 FLA KEYS   S05    0                1140   THA BIFA 1      NA
      571  2022 FLA KEYS   S05    0                1141   THA BIFA 1      NA
      572  2022 FLA KEYS   S05    0                1142   THA BIFA 1      NA
      573  2022 FLA KEYS   S05    0                1146   THA BIFA 1      NA
      574  2022 FLA KEYS   S05    0                1147   THA BIFA 1      NA
      575  2022 FLA KEYS   S05    0                1148   THA BIFA 1      NA
      576  2022 FLA KEYS   S05    0                1151   THA BIFA 1      NA
      577  2022 FLA KEYS   S05    0                1155   THA BIFA 1      NA
      578  2022 FLA KEYS   S05    0                1158   THA BIFA 1      NA
      579  2022 FLA KEYS   S05    0                1162   THA BIFA 1      NA
      580  2022 FLA KEYS   S05    0                1163   THA BIFA 1      NA
      581  2022 FLA KEYS   S05    0                1165   THA BIFA 1      NA
      582  2022 FLA KEYS   S05    0                1166   THA BIFA 1      NA
      583  2022 FLA KEYS   S05    0                1168   THA BIFA 1      NA
      584  2022 FLA KEYS   S05    0                1170   THA BIFA 1      NA
      585  2022 FLA KEYS   S05    0                1178   THA BIFA 1      NA
      586  2022 FLA KEYS   S05    0                1179   THA BIFA 1      NA
      587  2022 FLA KEYS   S05    0                1180   THA BIFA 1      NA
      588  2022 FLA KEYS   S05    0                1181   THA BIFA 1      NA
      589  2022 FLA KEYS   S05    0                1182   THA BIFA 1      NA
      590  2022 FLA KEYS   S05    0                1192   THA BIFA 1      NA
      591  2022 FLA KEYS   S05    0                1197   THA BIFA 1      NA
      592  2022 FLA KEYS   S05    0                1198   THA BIFA 1      NA
      593  2022 FLA KEYS   S05    0                1204   THA BIFA 1      NA
      594  2022 FLA KEYS   S05    0                1253   THA BIFA 1      NA
      595  2022 FLA KEYS   S05    0                1266   THA BIFA 1      NA
      596  2022 FLA KEYS   S05    0                1281   THA BIFA 1      NA
      597  2022 FLA KEYS   S05    0                1406   THA BIFA 1      NA
      598  2022 FLA KEYS   S05    0                1564   THA BIFA 1      NA
      599  2022 FLA KEYS   S05    0                1575   THA BIFA 1      NA
      600  2022 FLA KEYS   S05    0                1582   THA BIFA 1      NA
      601  2022 FLA KEYS   S05    0                1592   THA BIFA 1      NA
      602  2022 FLA KEYS   S05    0                1598   THA BIFA 1      NA
      603  2022 FLA KEYS   S05    0                1611   THA BIFA 1      NA
      604  2022 FLA KEYS   S05    0                1622   THA BIFA 1      NA
      605  2022 FLA KEYS   S05    0                1623   THA BIFA 1      NA
      606  2022 FLA KEYS   S05    0                1635   THA BIFA 1      NA
      607  2022 FLA KEYS   S05    0                1636   THA BIFA 1      NA
      608  2022 FLA KEYS   S05    0                1637   THA BIFA 1      NA
      609  2022 FLA KEYS   S05    0                1641   THA BIFA 1      NA
      610  2022 FLA KEYS   S05    0                1642   THA BIFA 1      NA
      611  2022 FLA KEYS   S05    0                1665   THA BIFA 1      NA
      612  2022 FLA KEYS   S05    0                1992   THA BIFA 1      NA
      613  2022 FLA KEYS   S05    0                1993   THA BIFA 1      NA
      614  2022 FLA KEYS   S05    0                1994   THA BIFA 1      NA
      615  2022 FLA KEYS   S05    0                1995   THA BIFA 1      NA
      616  2022 FLA KEYS   S05    0                1997   THA BIFA 1      NA
      617  2022 FLA KEYS   S05    0                1998   THA BIFA 1      NA
      618  2022 FLA KEYS   S05    0                1999   THA BIFA 1      NA
      619  2022 FLA KEYS   S05    0                8001   THA BIFA 1      NA
      620  2022 FLA KEYS   S05    0                8002   THA BIFA 1      NA
      621  2022 FLA KEYS   S05    0                8003   THA BIFA 1      NA
      622  2022 FLA KEYS   S05    0                8004   THA BIFA 1      NA
      623  2022 FLA KEYS   S05    0                8005   THA BIFA 1      NA
      624  2022 FLA KEYS   S05    0                8011   THA BIFA 1      NA
      625  2022 FLA KEYS   S05    0                8017   THA BIFA 1      NA
      626  2022 FLA KEYS   S05    0                8019   THA BIFA 1      NA
      627  2022 FLA KEYS   S05    1                1384   THA BIFA 1      NA
      628  2022 FLA KEYS   S05    1                1385   THA BIFA 1      NA
      629  2022 FLA KEYS   S05    1                1386   THA BIFA 1      NA
      630  2022 FLA KEYS   S05    1                1391   THA BIFA 1      NA
      631  2022 FLA KEYS   S05    1                1393   THA BIFA 1      NA
      632  2022 FLA KEYS   S05    1                1394   THA BIFA 1      NA
      633  2022 FLA KEYS   S05    1                1396   THA BIFA 1      NA
      634  2022 FLA KEYS   S05    1                1398   THA BIFA 1      NA
      635  2022 FLA KEYS   S05    1                1399   THA BIFA 1      NA
      636  2022 FLA KEYS   S05    1                1400   THA BIFA 1      NA
      637  2022 FLA KEYS   S05    1                1401   THA BIFA 1      NA
      638  2022 FLA KEYS   S05    1                1404   THA BIFA 1      NA
      639  2022 FLA KEYS   S05    1                1407   THA BIFA 1      NA
      640  2022 FLA KEYS   S05    1                1408   THA BIFA 1      NA
      641  2022 FLA KEYS   S05    1                1411   THA BIFA 1      NA
      642  2022 FLA KEYS   S05    1                1412   THA BIFA 1      NA
      643  2022 FLA KEYS   S05    1                1428   THA BIFA 1      NA
      644  2022 FLA KEYS   S05    1                1429   THA BIFA 1      NA
      645  2022 FLA KEYS   S05    1                1431   THA BIFA 1      NA
      646  2022 FLA KEYS   S05    1                1432   THA BIFA 1      NA
      647  2022 FLA KEYS   S05    1                1435   THA BIFA 1      NA
      648  2022 FLA KEYS   S05    1                1438   THA BIFA 1      NA
      649  2022 FLA KEYS   S05    1                1449   THA BIFA 1      NA
      650  2022 FLA KEYS   S05    1                1450   THA BIFA 1      NA
      651  2022 FLA KEYS   S05    1                1454   THA BIFA 1      NA
      652  2022 FLA KEYS   S05    1                1464   THA BIFA 1      NA
      653  2022 FLA KEYS   S05    1                1913   THA BIFA 1      NA
      654  2022 FLA KEYS   S05    1                9063   THA BIFA 1      NA
      655  2022 FLA KEYS   S05    1                9068   THA BIFA 1      NA
      656  2022 FLA KEYS   S05    1                9069   THA BIFA 1      NA
      657  2022 FLA KEYS   S05    1                9194   THA BIFA 1      NA
      658  2022 FLA KEYS   S05    1                9255   THA BIFA 1      NA
      659  2022 FLA KEYS   S06    0                1036   THA BIFA 1      NA
      660  2022 FLA KEYS   S06    0                1090   THA BIFA 1      NA
      661  2022 FLA KEYS   S06    0                1097   THA BIFA 1      NA
      662  2022 FLA KEYS   S06    0                1102   THA BIFA 1      NA
      663  2022 FLA KEYS   S06    0                1107   THA BIFA 1      NA
      664  2022 FLA KEYS   S06    0                1110   THA BIFA 1      NA
      665  2022 FLA KEYS   S06    0                1117   THA BIFA 1      NA
      666  2022 FLA KEYS   S06    0                1120   THA BIFA 1      NA
      667  2022 FLA KEYS   S06    0                1121   THA BIFA 1      NA
      668  2022 FLA KEYS   S06    0                1128   THA BIFA 1      NA
      669  2022 FLA KEYS   S06    0                1129   THA BIFA 1      NA
      670  2022 FLA KEYS   S06    0                1130   THA BIFA 1      NA
      671  2022 FLA KEYS   S06    0                1131   THA BIFA 1      NA
      672  2022 FLA KEYS   S06    0                1132   THA BIFA 1      NA
      673  2022 FLA KEYS   S06    0                1136   THA BIFA 1      NA
      674  2022 FLA KEYS   S06    0                1138   THA BIFA 1      NA
      675  2022 FLA KEYS   S06    0                1144   THA BIFA 1      NA
      676  2022 FLA KEYS   S06    0                1149   THA BIFA 1      NA
      677  2022 FLA KEYS   S06    0                1150   THA BIFA 1      NA
      678  2022 FLA KEYS   S06    0                1152   THA BIFA 1      NA
      679  2022 FLA KEYS   S06    0                1153   THA BIFA 1      NA
      680  2022 FLA KEYS   S06    0                1154   THA BIFA 1      NA
      681  2022 FLA KEYS   S06    0                1159   THA BIFA 1      NA
      682  2022 FLA KEYS   S06    0                1161   THA BIFA 1      NA
      683  2022 FLA KEYS   S06    0                1169   THA BIFA 1      NA
      684  2022 FLA KEYS   S06    0                1172   THA BIFA 1      NA
      685  2022 FLA KEYS   S06    0                1173   THA BIFA 1      NA
      686  2022 FLA KEYS   S06    0                1174   THA BIFA 1      NA
      687  2022 FLA KEYS   S06    0                1177   THA BIFA 1      NA
      688  2022 FLA KEYS   S06    0                1183   THA BIFA 1      NA
      689  2022 FLA KEYS   S06    0                1184   THA BIFA 1      NA
      690  2022 FLA KEYS   S06    0                1185   THA BIFA 1      NA
      691  2022 FLA KEYS   S06    0                1186   THA BIFA 1      NA
      692  2022 FLA KEYS   S06    0                1187   THA BIFA 1      NA
      693  2022 FLA KEYS   S06    0                1188   THA BIFA 1      NA
      694  2022 FLA KEYS   S06    0                1189   THA BIFA 1      NA
      695  2022 FLA KEYS   S06    0                1190   THA BIFA 1      NA
      696  2022 FLA KEYS   S06    0                1191   THA BIFA 1      NA
      697  2022 FLA KEYS   S06    0                1193   THA BIFA 1      NA
      698  2022 FLA KEYS   S06    0                1195   THA BIFA 1      NA
      699  2022 FLA KEYS   S06    0                1196   THA BIFA 1      NA
      700  2022 FLA KEYS   S06    0                1203   THA BIFA 1      NA
      701  2022 FLA KEYS   S06    0                1205   THA BIFA 1      NA
      702  2022 FLA KEYS   S06    0                1206   THA BIFA 1      NA
      703  2022 FLA KEYS   S06    0                1213   THA BIFA 1      NA
      704  2022 FLA KEYS   S06    0                1231   THA BIFA 1      NA
      705  2022 FLA KEYS   S06    0                1240   THA BIFA 1      NA
      706  2022 FLA KEYS   S06    0                1247   THA BIFA 1      NA
      707  2022 FLA KEYS   S06    0                1272   THA BIFA 1      NA
      708  2022 FLA KEYS   S06    0                1280   THA BIFA 1      NA
      709  2022 FLA KEYS   S06    0                1492   THA BIFA 1      NA
      710  2022 FLA KEYS   S06    0                1577   THA BIFA 1      NA
      711  2022 FLA KEYS   S06    0                1639   THA BIFA 1      NA
      712  2022 FLA KEYS   S06    0                1640   THA BIFA 1      NA
      713  2022 FLA KEYS   S06    0                1643   THA BIFA 1      NA
      714  2022 FLA KEYS   S06    0                1644   THA BIFA 1      NA
      715  2022 FLA KEYS   S06    0                1657   THA BIFA 1      NA
      716  2022 FLA KEYS   S06    0                1664   THA BIFA 1      NA
      717  2022 FLA KEYS   S06    0                1666   THA BIFA 1      NA
      718  2022 FLA KEYS   S06    0                1667   THA BIFA 1      NA
      719  2022 FLA KEYS   S06    0                1668   THA BIFA 1      NA
      720  2022 FLA KEYS   S06    0                1669   THA BIFA 1      NA
      721  2022 FLA KEYS   S06    0                1670   THA BIFA 1      NA
      722  2022 FLA KEYS   S06    0                1673   THA BIFA 1      NA
      723  2022 FLA KEYS   S06    0                1680   THA BIFA 1      NA
      724  2022 FLA KEYS   S06    0                1682   THA BIFA 1      NA
      725  2022 FLA KEYS   S06    0                1731   THA BIFA 1      NA
      726  2022 FLA KEYS   S06    0                1959   THA BIFA 1      NA
      727  2022 FLA KEYS   S06    0                1967   THA BIFA 1      NA
      728  2022 FLA KEYS   S06    0                1991   THA BIFA 1      NA
      729  2022 FLA KEYS   S06    0                1996   THA BIFA 1      NA
      730  2022 FLA KEYS   S06    0                8006   THA BIFA 1      NA
      731  2022 FLA KEYS   S06    0                8007   THA BIFA 1      NA
      732  2022 FLA KEYS   S06    0                8008   THA BIFA 1      NA
      733  2022 FLA KEYS   S06    0                8009   THA BIFA 1      NA
      734  2022 FLA KEYS   S06    0                8010   THA BIFA 1      NA
      735  2022 FLA KEYS   S06    0                8012   THA BIFA 1      NA
      736  2022 FLA KEYS   S06    0                8013   THA BIFA 1      NA
      737  2022 FLA KEYS   S06    0                8014   THA BIFA 1      NA
      738  2022 FLA KEYS   S06    0                8015   THA BIFA 1      NA
      739  2022 FLA KEYS   S06    0                8016   THA BIFA 1      NA
      740  2022 FLA KEYS   S06    0                8018   THA BIFA 1      NA
      741  2022 FLA KEYS   S06    0                8032   THA BIFA 1      NA
      742  2022 FLA KEYS   S06    0                8065   THA BIFA 1      NA
      743  2022 FLA KEYS   S06    0                8067   THA BIFA 1      NA
      744  2022 FLA KEYS   S06    0                9029   THA BIFA 1      NA
      745  2022 FLA KEYS   S06    0                9030   THA BIFA 1      NA
      746  2022 FLA KEYS   S06    0                9097   THA BIFA 1      NA
      747  2022 FLA KEYS   S06    0                9298   THA BIFA 1      NA
      748  2022 FLA KEYS   S06    1                1092   THA BIFA 1      NA
      749  2022 FLA KEYS   S06    1                1383   THA BIFA 1      NA
      750  2022 FLA KEYS   S06    1                1387   THA BIFA 1      NA
      751  2022 FLA KEYS   S06    1                1388   THA BIFA 1      NA
      752  2022 FLA KEYS   S06    1                1389   THA BIFA 1      NA
      753  2022 FLA KEYS   S06    1                1390   THA BIFA 1      NA
      754  2022 FLA KEYS   S06    1                1392   THA BIFA 1      NA
      755  2022 FLA KEYS   S06    1                1395   THA BIFA 1      NA
      756  2022 FLA KEYS   S06    1                1397   THA BIFA 1      NA
      757  2022 FLA KEYS   S06    1                1402   THA BIFA 1      NA
      758  2022 FLA KEYS   S06    1                1403   THA BIFA 1      NA
      759  2022 FLA KEYS   S06    1                1405   THA BIFA 1      NA
      760  2022 FLA KEYS   S06    1                1409   THA BIFA 1      NA
      761  2022 FLA KEYS   S06    1                1410   THA BIFA 1      NA
      762  2022 FLA KEYS   S06    1                1413   THA BIFA 1      NA
      763  2022 FLA KEYS   S06    1                1415   THA BIFA 1      NA
      764  2022 FLA KEYS   S06    1                1416   THA BIFA 1      NA
      765  2022 FLA KEYS   S06    1                1417   THA BIFA 1      NA
      766  2022 FLA KEYS   S06    1                1418   THA BIFA 1      NA
      767  2022 FLA KEYS   S06    1                1419   THA BIFA 1      NA
      768  2022 FLA KEYS   S06    1                1420   THA BIFA 1      NA
      769  2022 FLA KEYS   S06    1                1421   THA BIFA 1      NA
      770  2022 FLA KEYS   S06    1                1422   THA BIFA 1      NA
      771  2022 FLA KEYS   S06    1                1423   THA BIFA 1      NA
      772  2022 FLA KEYS   S06    1                1424   THA BIFA 1      NA
      773  2022 FLA KEYS   S06    1                1425   THA BIFA 1      NA
      774  2022 FLA KEYS   S06    1                1426   THA BIFA 1      NA
      775  2022 FLA KEYS   S06    1                1427   THA BIFA 1      NA
      776  2022 FLA KEYS   S06    1                1430   THA BIFA 1      NA
      777  2022 FLA KEYS   S06    1                1434   THA BIFA 1      NA
      778  2022 FLA KEYS   S06    1                1436   THA BIFA 1      NA
      779  2022 FLA KEYS   S06    1                1437   THA BIFA 1      NA
      780  2022 FLA KEYS   S06    1                1439   THA BIFA 1      NA
      781  2022 FLA KEYS   S06    1                1440   THA BIFA 1      NA
      782  2022 FLA KEYS   S06    1                1442   THA BIFA 1      NA
      783  2022 FLA KEYS   S06    1                1443   THA BIFA 1      NA
      784  2022 FLA KEYS   S06    1                1444   THA BIFA 1      NA
      785  2022 FLA KEYS   S06    1                1445   THA BIFA 1      NA
      786  2022 FLA KEYS   S06    1                1447   THA BIFA 1      NA
      787  2022 FLA KEYS   S06    1                1448   THA BIFA 1      NA
      788  2022 FLA KEYS   S06    1                1452   THA BIFA 1      NA
      789  2022 FLA KEYS   S06    1                1459   THA BIFA 1      NA
      790  2022 FLA KEYS   S06    1                1460   THA BIFA 1      NA
      791  2022 FLA KEYS   S06    1                1461   THA BIFA 1      NA
      792  2022 FLA KEYS   S06    1                1463   THA BIFA 1      NA
      793  2022 FLA KEYS   S06    1                1921   THA BIFA 1      NA
      794  2022 FLA KEYS   S06    1                9026   THA BIFA 1      NA
      795  2022 FLA KEYS   S06    1                9027   THA BIFA 1      NA
      796  2022 FLA KEYS   S06    1                9028   THA BIFA 1      NA
      797  2022 FLA KEYS   S06    1                9036   THA BIFA 1      NA
      798  2022 FLA KEYS   S06    1                9037   THA BIFA 1      NA
      799  2022 FLA KEYS   S06    1                9038   THA BIFA 1      NA
      800  2022 FLA KEYS   S06    1                9039   THA BIFA 1      NA
      801  2022 FLA KEYS   S06    1                9040   THA BIFA 1      NA
      802  2022 FLA KEYS   S06    1                9041   THA BIFA 1      NA
      803  2022 FLA KEYS   S06    1                9042   THA BIFA 1      NA
      804  2022 FLA KEYS   S06    1                9043   THA BIFA 1      NA
      805  2022 FLA KEYS   S06    1                9045   THA BIFA 1      NA
      806  2022 FLA KEYS   S06    1                9046   THA BIFA 1      NA
      807  2022 FLA KEYS   S06    1                9047   THA BIFA 1      NA
      808  2022 FLA KEYS   S06    1                9048   THA BIFA 1      NA
      809  2022 FLA KEYS   S06    1                9049   THA BIFA 1      NA
      810  2022 FLA KEYS   S06    1                9050   THA BIFA 1      NA
      811  2022 FLA KEYS   S06    1                9052   THA BIFA 1      NA
      812  2022 FLA KEYS   S06    1                9053   THA BIFA 1      NA
      813  2022 FLA KEYS   S06    1                9054   THA BIFA 1      NA
      814  2022 FLA KEYS   S06    1                9070   THA BIFA 1      NA
      815  2022 FLA KEYS   S06    1                9071   THA BIFA 1      NA
      816  2022 FLA KEYS   S06    1                9072   THA BIFA 1      NA
      817  2022 FLA KEYS   S06    1                9073   THA BIFA 1      NA
      818  2022 FLA KEYS   S06    1                9074   THA BIFA 1      NA
      819  2022 FLA KEYS   S06    1                9076   THA BIFA 1      NA
      820  2022 FLA KEYS   S06    1                9079   THA BIFA 1      NA
      821  2022 FLA KEYS   S06    1                9080   THA BIFA 1      NA
      822  2022 FLA KEYS   S06    1                9081   THA BIFA 1      NA
      823  2022 FLA KEYS   S06    1                9082   THA BIFA 1      NA
      824  2022 FLA KEYS   S06    1                9083   THA BIFA 1      NA
      825  2022 FLA KEYS   S06    1                9084   THA BIFA 1      NA
      826  2022 FLA KEYS   S06    1                9085   THA BIFA 1      NA
      827  2022 FLA KEYS   S06    1                9086   THA BIFA 1      NA
      828  2022 FLA KEYS   S06    1                9087   THA BIFA 1      NA
      829  2022 FLA KEYS   S06    1                9088   THA BIFA 1      NA
      830  2022 FLA KEYS   S06    1                9089   THA BIFA 1      NA
      831  2022 FLA KEYS   S06    1                9090   THA BIFA 1      NA
      832  2022 FLA KEYS   S06    1                9091   THA BIFA 1      NA
      833  2022 FLA KEYS   S06    1                9092   THA BIFA 1      NA
      834  2022 FLA KEYS   S06    1                9093   THA BIFA 1      NA
      835  2022 FLA KEYS   S06    1                9100   THA BIFA 1      NA
      836  2022 FLA KEYS   S06    1                9128   THA BIFA 1      NA
      837  2022 FLA KEYS   S06    1                9138   THA BIFA 1      NA
      838  2022 FLA KEYS   S06    1                9139   THA BIFA 1      NA
      839  2022 FLA KEYS   S06    1                9142   THA BIFA 1      NA
      840  2022 FLA KEYS   S06    1                9144   THA BIFA 1      NA
      841  2022 FLA KEYS   S06    1                9151   THA BIFA 1      NA
      842  2022 FLA KEYS   S06    1                9161   THA BIFA 1      NA
      843  2022 FLA KEYS   S06    1                9166   THA BIFA 1      NA
      844  2022 FLA KEYS   S06    1                9167   THA BIFA 1      NA
      845  2022 FLA KEYS   S06    1                9176   THA BIFA 1      NA
      846  2022 FLA KEYS   S06    1                9180   THA BIFA 1      NA
      847  2022 FLA KEYS   S06    1                9195   THA BIFA 1      NA
      848  2022 FLA KEYS   S06    1                9196   THA BIFA 1      NA
      849  2022 FLA KEYS   S06    1                9199   THA BIFA 1      NA
      850  2022 FLA KEYS   S06    1                9228   THA BIFA 1      NA
      851  2022 FLA KEYS   S06    1                9235   THA BIFA 1      NA
      852  2022 FLA KEYS   S06    1                9256   THA BIFA 1      NA
      853  2022 FLA KEYS   S06    1                9265   THA BIFA 1      NA
      854  2022 FLA KEYS   S08    0                1209   THA BIFA 1      NA
      855  2022 FLA KEYS   S08    0                1211   THA BIFA 1      NA
      856  2022 FLA KEYS   S08    0                1214   THA BIFA 1      NA
      857  2022 FLA KEYS   S08    0                1216   THA BIFA 1      NA
      858  2022 FLA KEYS   S08    0                1217   THA BIFA 1      NA
      859  2022 FLA KEYS   S08    0                1221   THA BIFA 1      NA
      860  2022 FLA KEYS   S08    0                1224   THA BIFA 1      NA
      861  2022 FLA KEYS   S08    0                1227   THA BIFA 1      NA
      862  2022 FLA KEYS   S08    0                1228   THA BIFA 1      NA
      863  2022 FLA KEYS   S08    0                1233   THA BIFA 1      NA
      864  2022 FLA KEYS   S08    0                1252   THA BIFA 1      NA
      865  2022 FLA KEYS   S08    0                1259   THA BIFA 1      NA
      866  2022 FLA KEYS   S08    0                1263   THA BIFA 1      NA
      867  2022 FLA KEYS   S08    0                1268   THA BIFA 1      NA
      868  2022 FLA KEYS   S08    0                1273   THA BIFA 1      NA
      869  2022 FLA KEYS   S08    0                1279   THA BIFA 1      NA
      870  2022 FLA KEYS   S08    0                1294   THA BIFA 1      NA
      871  2022 FLA KEYS   S08    0                1301   THA BIFA 1      NA
      872  2022 FLA KEYS   S08    0                1303   THA BIFA 1      NA
      873  2022 FLA KEYS   S08    0                1305   THA BIFA 1      NA
      874  2022 FLA KEYS   S08    0                1315   THA BIFA 1      NA
      875  2022 FLA KEYS   S08    0                1702   THA BIFA 1      NA
      876  2022 FLA KEYS   S08    0                1781   THA BIFA 1      NA
      877  2022 FLA KEYS   S08    0                8021   THA BIFA 1      NA
      878  2022 FLA KEYS   S08    0                8022   THA BIFA 1      NA
      879  2022 FLA KEYS   S08    0                8023   THA BIFA 1      NA
      880  2022 FLA KEYS   S08    0                8024   THA BIFA 1      NA
      881  2022 FLA KEYS   S08    0                8027   THA BIFA 1      NA
      882  2022 FLA KEYS   S08    0                8028   THA BIFA 1      NA
      883  2022 FLA KEYS   S08    0                8029   THA BIFA 1      NA
      884  2022 FLA KEYS   S08    0                8030   THA BIFA 1      NA
      885  2022 FLA KEYS   S08    0                8033   THA BIFA 1      NA
      886  2022 FLA KEYS   S08    0                8042   THA BIFA 1      NA
      887  2022 FLA KEYS   S08    0                8043   THA BIFA 1      NA
      888  2022 FLA KEYS   S08    1                1455   THA BIFA 1      NA
      889  2022 FLA KEYS   S09    0                1100   THA BIFA 1      NA
      890  2022 FLA KEYS   S09    0                1175   THA BIFA 1      NA
      891  2022 FLA KEYS   S09    0                1208   THA BIFA 1      NA
      892  2022 FLA KEYS   S09    0                1210   THA BIFA 1      NA
      893  2022 FLA KEYS   S09    0                1212   THA BIFA 1      NA
      894  2022 FLA KEYS   S09    0                1215   THA BIFA 1      NA
      895  2022 FLA KEYS   S09    0                1218   THA BIFA 1      NA
      896  2022 FLA KEYS   S09    0                1219   THA BIFA 1      NA
      897  2022 FLA KEYS   S09    0                1220   THA BIFA 1      NA
      898  2022 FLA KEYS   S09    0                1222   THA BIFA 1      NA
      899  2022 FLA KEYS   S09    0                1226   THA BIFA 1      NA
      900  2022 FLA KEYS   S09    0                1229   THA BIFA 1      NA
      901  2022 FLA KEYS   S09    0                1230   THA BIFA 1      NA
      902  2022 FLA KEYS   S09    0                1232   THA BIFA 1      NA
      903  2022 FLA KEYS   S09    0                1234   THA BIFA 1      NA
      904  2022 FLA KEYS   S09    0                1235   THA BIFA 1      NA
      905  2022 FLA KEYS   S09    0                1236   THA BIFA 1      NA
      906  2022 FLA KEYS   S09    0                1237   THA BIFA 1      NA
      907  2022 FLA KEYS   S09    0                1238   THA BIFA 1      NA
      908  2022 FLA KEYS   S09    0                1239   THA BIFA 1      NA
      909  2022 FLA KEYS   S09    0                1241   THA BIFA 1      NA
      910  2022 FLA KEYS   S09    0                1242   THA BIFA 1      NA
      911  2022 FLA KEYS   S09    0                1244   THA BIFA 1      NA
      912  2022 FLA KEYS   S09    0                1245   THA BIFA 1      NA
      913  2022 FLA KEYS   S09    0                1246   THA BIFA 1      NA
      914  2022 FLA KEYS   S09    0                1248   THA BIFA 1      NA
      915  2022 FLA KEYS   S09    0                1249   THA BIFA 1      NA
      916  2022 FLA KEYS   S09    0                1250   THA BIFA 1      NA
      917  2022 FLA KEYS   S09    0                1251   THA BIFA 1      NA
      918  2022 FLA KEYS   S09    0                1256   THA BIFA 1      NA
      919  2022 FLA KEYS   S09    0                1257   THA BIFA 1      NA
      920  2022 FLA KEYS   S09    0                1262   THA BIFA 1      NA
      921  2022 FLA KEYS   S09    0                1265   THA BIFA 1      NA
      922  2022 FLA KEYS   S09    0                1269   THA BIFA 1      NA
      923  2022 FLA KEYS   S09    0                1271   THA BIFA 1      NA
      924  2022 FLA KEYS   S09    0                1275   THA BIFA 1      NA
      925  2022 FLA KEYS   S09    0                1278   THA BIFA 1      NA
      926  2022 FLA KEYS   S09    0                1282   THA BIFA 1      NA
      927  2022 FLA KEYS   S09    0                1283   THA BIFA 1      NA
      928  2022 FLA KEYS   S09    0                1284   THA BIFA 1      NA
      929  2022 FLA KEYS   S09    0                1285   THA BIFA 1      NA
      930  2022 FLA KEYS   S09    0                1286   THA BIFA 1      NA
      931  2022 FLA KEYS   S09    0                1288   THA BIFA 1      NA
      932  2022 FLA KEYS   S09    0                1290   THA BIFA 1      NA
      933  2022 FLA KEYS   S09    0                1291   THA BIFA 1      NA
      934  2022 FLA KEYS   S09    0                1292   THA BIFA 1      NA
      935  2022 FLA KEYS   S09    0                1293   THA BIFA 1      NA
      936  2022 FLA KEYS   S09    0                1295   THA BIFA 1      NA
      937  2022 FLA KEYS   S09    0                1296   THA BIFA 1      NA
      938  2022 FLA KEYS   S09    0                1297   THA BIFA 1      NA
      939  2022 FLA KEYS   S09    0                1298   THA BIFA 1      NA
      940  2022 FLA KEYS   S09    0                1299   THA BIFA 1      NA
      941  2022 FLA KEYS   S09    0                1300   THA BIFA 1      NA
      942  2022 FLA KEYS   S09    0                1302   THA BIFA 1      NA
      943  2022 FLA KEYS   S09    0                1307   THA BIFA 1      NA
      944  2022 FLA KEYS   S09    0                1308   THA BIFA 1      NA
      945  2022 FLA KEYS   S09    0                1309   THA BIFA 1      NA
      946  2022 FLA KEYS   S09    0                1310   THA BIFA 1      NA
      947  2022 FLA KEYS   S09    0                1312   THA BIFA 1      NA
      948  2022 FLA KEYS   S09    0                1313   THA BIFA 1      NA
      949  2022 FLA KEYS   S09    0                1314   THA BIFA 1      NA
      950  2022 FLA KEYS   S09    0                1316   THA BIFA 1      NA
      951  2022 FLA KEYS   S09    0                1323   THA BIFA 1      NA
      952  2022 FLA KEYS   S09    0                1331   THA BIFA 1      NA
      953  2022 FLA KEYS   S09    0                1332   THA BIFA 1      NA
      954  2022 FLA KEYS   S09    0                1346   THA BIFA 1      NA
      955  2022 FLA KEYS   S09    0                1348   THA BIFA 1      NA
      956  2022 FLA KEYS   S09    0                1638   THA BIFA 1      NA
      957  2022 FLA KEYS   S09    0                1696   THA BIFA 1      NA
      958  2022 FLA KEYS   S09    0                1700   THA BIFA 1      NA
      959  2022 FLA KEYS   S09    0                1714   THA BIFA 1      NA
      960  2022 FLA KEYS   S09    0                1716   THA BIFA 1      NA
      961  2022 FLA KEYS   S09    0                1743   THA BIFA 1      NA
      962  2022 FLA KEYS   S09    0                1748   THA BIFA 1      NA
      963  2022 FLA KEYS   S09    0                1751   THA BIFA 1      NA
      964  2022 FLA KEYS   S09    0                1768   THA BIFA 1      NA
      965  2022 FLA KEYS   S09    0                1769   THA BIFA 1      NA
      966  2022 FLA KEYS   S09    0                1770   THA BIFA 1      NA
      967  2022 FLA KEYS   S09    0                1771   THA BIFA 1      NA
      968  2022 FLA KEYS   S09    0                1787   THA BIFA 1      NA
      969  2022 FLA KEYS   S09    0                1842   THA BIFA 1      NA
      970  2022 FLA KEYS   S09    0                1939   THA BIFA 1      NA
      971  2022 FLA KEYS   S09    0                8031   THA BIFA 1      NA
      972  2022 FLA KEYS   S09    0                8034   THA BIFA 1      NA
      973  2022 FLA KEYS   S09    0                8035   THA BIFA 1      NA
      974  2022 FLA KEYS   S09    0                8036   THA BIFA 1      NA
      975  2022 FLA KEYS   S09    0                8037   THA BIFA 1      NA
      976  2022 FLA KEYS   S09    0                8038   THA BIFA 1      NA
      977  2022 FLA KEYS   S09    0                8039   THA BIFA 1      NA
      978  2022 FLA KEYS   S09    0                8040   THA BIFA 1      NA
      979  2022 FLA KEYS   S09    0                8041   THA BIFA 1      NA
      980  2022 FLA KEYS   S09    0                8044   THA BIFA 1      NA
      981  2022 FLA KEYS   S09    0                8045   THA BIFA 1      NA
      982  2022 FLA KEYS   S09    0                8046   THA BIFA 1      NA
      983  2022 FLA KEYS   S09    0                8047   THA BIFA 1      NA
      984  2022 FLA KEYS   S09    0                8048   THA BIFA 1      NA
      985  2022 FLA KEYS   S09    0                8049   THA BIFA 1      NA
      986  2022 FLA KEYS   S09    0                8050   THA BIFA 1      NA
      987  2022 FLA KEYS   S09    0                8054   THA BIFA 1      NA
      988  2022 FLA KEYS   S09    0                8057   THA BIFA 1      NA
      989  2022 FLA KEYS   S09    0                8058   THA BIFA 1      NA
      990  2022 FLA KEYS   S09    0                8076   THA BIFA 1      NA
      991  2022 FLA KEYS   S09    1                1446   THA BIFA 1      NA
      992  2022 FLA KEYS   S09    1                1456   THA BIFA 1      NA
      993  2022 FLA KEYS   S09    1                1457   THA BIFA 1      NA
      994  2022 FLA KEYS   S09    1                1458   THA BIFA 1      NA
      995  2022 FLA KEYS   S09    1                1462   THA BIFA 1      NA
      996  2022 FLA KEYS   S09    1                1465   THA BIFA 1      NA
      997  2022 FLA KEYS   S09    1                1466   THA BIFA 1      NA
      998  2022 FLA KEYS   S09    1                1467   THA BIFA 1      NA
      999  2022 FLA KEYS   S09    1                1468   THA BIFA 1      NA
      1000 2022 FLA KEYS   S09    1                1469   THA BIFA 1      NA
      1001 2022 FLA KEYS   S09    1                1470   THA BIFA 1      NA
      1002 2022 FLA KEYS   S09    1                1471   THA BIFA 1      NA
      1003 2022 FLA KEYS   S09    1                1472   THA BIFA 1      NA
      1004 2022 FLA KEYS   S09    1                1473   THA BIFA 1      NA
      1005 2022 FLA KEYS   S09    1                1474   THA BIFA 1      NA
      1006 2022 FLA KEYS   S09    1                1475   THA BIFA 1      NA
      1007 2022 FLA KEYS   S09    1                1476   THA BIFA 1      NA
      1008 2022 FLA KEYS   S09    1                1477   THA BIFA 1      NA
      1009 2022 FLA KEYS   S09    1                1478   THA BIFA 1      NA
      1010 2022 FLA KEYS   S09    1                1479   THA BIFA 1      NA
      1011 2022 FLA KEYS   S09    1                1480   THA BIFA 1      NA
      1012 2022 FLA KEYS   S09    1                1893   THA BIFA 1      NA
      1013 2022 FLA KEYS   S09    1                1960   THA BIFA 1      NA
      1014 2022 FLA KEYS   S11    0                1304   THA BIFA 1      NA
      1015 2022 FLA KEYS   S11    0                1306   THA BIFA 1      NA
      1016 2022 FLA KEYS   S11    0                1311   THA BIFA 1      NA
      1017 2022 FLA KEYS   S11    0                1317   THA BIFA 1      NA
      1018 2022 FLA KEYS   S11    0                1320   THA BIFA 1      NA
      1019 2022 FLA KEYS   S11    0                1321   THA BIFA 1      NA
      1020 2022 FLA KEYS   S11    0                1324   THA BIFA 1      NA
      1021 2022 FLA KEYS   S11    0                1325   THA BIFA 1      NA
      1022 2022 FLA KEYS   S11    0                1333   THA BIFA 1      NA
      1023 2022 FLA KEYS   S11    0                1334   THA BIFA 1      NA
      1024 2022 FLA KEYS   S11    0                1335   THA BIFA 1      NA
      1025 2022 FLA KEYS   S11    0                1336   THA BIFA 1      NA
      1026 2022 FLA KEYS   S11    0                1339   THA BIFA 1      NA
      1027 2022 FLA KEYS   S11    0                1340   THA BIFA 1      NA
      1028 2022 FLA KEYS   S11    0                1341   THA BIFA 1      NA
      1029 2022 FLA KEYS   S11    0                1342   THA BIFA 1      NA
      1030 2022 FLA KEYS   S11    0                1343   THA BIFA 1      NA
      1031 2022 FLA KEYS   S11    0                1347   THA BIFA 1      NA
      1032 2022 FLA KEYS   S11    0                1349   THA BIFA 1      NA
      1033 2022 FLA KEYS   S11    0                1350   THA BIFA 1      NA
      1034 2022 FLA KEYS   S11    0                1351   THA BIFA 1      NA
      1035 2022 FLA KEYS   S11    0                1353   THA BIFA 1      NA
      1036 2022 FLA KEYS   S11    0                1354   THA BIFA 1      NA
      1037 2022 FLA KEYS   S11    0                1355   THA BIFA 1      NA
      1038 2022 FLA KEYS   S11    0                1356   THA BIFA 1      NA
      1039 2022 FLA KEYS   S11    0                1357   THA BIFA 1      NA
      1040 2022 FLA KEYS   S11    0                1358   THA BIFA 1      NA
      1041 2022 FLA KEYS   S11    0                1359   THA BIFA 1      NA
      1042 2022 FLA KEYS   S11    0                1360   THA BIFA 1      NA
      1043 2022 FLA KEYS   S11    0                1361   THA BIFA 1      NA
      1044 2022 FLA KEYS   S11    0                1362   THA BIFA 1      NA
      1045 2022 FLA KEYS   S11    0                1364   THA BIFA 1      NA
      1046 2022 FLA KEYS   S11    0                1715   THA BIFA 1      NA
      1047 2022 FLA KEYS   S11    0                1780   THA BIFA 1      NA
      1048 2022 FLA KEYS   S11    0                1795   THA BIFA 1      NA
      1049 2022 FLA KEYS   S11    0                1797   THA BIFA 1      NA
      1050 2022 FLA KEYS   S11    0                1810   THA BIFA 1      NA
      1051 2022 FLA KEYS   S11    0                1814   THA BIFA 1      NA
      1052 2022 FLA KEYS   S11    0                1825   THA BIFA 1      NA
      1053 2022 FLA KEYS   S11    0                1841   THA BIFA 1      NA
      1054 2022 FLA KEYS   S11    0                1845   THA BIFA 1      NA
      1055 2022 FLA KEYS   S11    0                1846   THA BIFA 1      NA
      1056 2022 FLA KEYS   S11    0                8051   THA BIFA 1      NA
      1057 2022 FLA KEYS   S11    0                8052   THA BIFA 1      NA
      1058 2022 FLA KEYS   S11    0                8053   THA BIFA 1      NA
      1059 2022 FLA KEYS   S11    0                8055   THA BIFA 1      NA
      1060 2022 FLA KEYS   S11    0                8056   THA BIFA 1      NA
      1061 2022 FLA KEYS   S11    0                8059   THA BIFA 1      NA
      1062 2022 FLA KEYS   S11    0                8060   THA BIFA 1      NA
      1063 2022 FLA KEYS   S11    0                8061   THA BIFA 1      NA
      1064 2022 FLA KEYS   S11    0                8068   THA BIFA 1      NA
           abundance
      1         12.5
      2         17.0
      3         55.0
      4         14.0
      5         14.0
      6         31.0
      7         10.0
      8         13.5
      9         23.5
      10         5.5
      11        11.0
      12        17.5
      13        46.5
      14        47.5
      15        15.5
      16        40.5
      17        36.5
      18        19.0
      19        10.0
      20        26.0
      21        29.0
      22        44.5
      23        15.0
      24        32.5
      25        44.0
      26        27.0
      27        41.5
      28        35.0
      29        26.5
      30        20.0
      31        40.0
      32        11.5
      33        29.0
      34        19.0
      35        40.5
      36         8.0
      37        56.0
      38         9.0
      39        52.0
      40        33.0
      41         0.0
      42        25.0
      43        65.0
      44        52.5
      45       100.0
      46        32.5
      47         4.0
      48        67.5
      49       115.0
      50        26.0
      51         0.5
      52        74.5
      53        34.0
      54        35.0
      55        95.0
      56         9.0
      57        26.0
      58        33.5
      59        44.0
      60        38.0
      61        45.0
      62       102.5
      63        86.0
      64        77.5
      65        40.0
      66        46.0
      67         5.5
      68         3.0
      69        15.5
      70        46.5
      71        42.5
      72        66.0
      73        35.5
      74        48.5
      75        34.5
      76        47.0
      77        57.0
      78        71.5
      79       101.0
      80        36.5
      81        53.5
      82        34.5
      83        51.0
      84        40.0
      85        34.5
      86        42.0
      87        29.0
      88        78.0
      89        37.0
      90        47.5
      91        33.5
      92        74.0
      93        39.0
      94        54.0
      95        17.0
      96        24.5
      97        81.0
      98        85.5
      99        75.0
      100       59.0
      101       43.0
      102       56.0
      103       75.5
      104       40.5
      105       37.0
      106       55.0
      107       54.0
      108       51.5
      109      107.0
      110       38.0
      111       38.0
      112       41.0
      113       24.0
      114       81.0
      115       16.0
      116       66.5
      117       32.0
      118       63.5
      119       34.0
      120       66.5
      121       46.0
      122       59.5
      123        2.0
      124        8.0
      125       12.0
      126       25.5
      127       46.5
      128       46.5
      129       54.5
      130       73.5
      131       82.0
      132       20.5
      133       29.0
      134       24.0
      135       51.5
      136      132.0
      137       17.5
      138       22.0
      139       69.5
      140       24.5
      141       31.0
      142       59.5
      143       35.0
      144       58.5
      145       37.5
      146       88.0
      147       51.5
      148       54.0
      149       48.0
      150       68.5
      151       27.0
      152       41.5
      153       56.5
      154       59.0
      155       19.0
      156       44.0
      157       23.5
      158      100.0
      159       21.5
      160       79.0
      161       44.0
      162       50.0
      163       24.5
      164       54.5
      165       32.5
      166       36.0
      167        3.0
      168        6.0
      169       35.0
      170       32.5
      171       41.5
      172       50.0
      173       72.5
      174       74.0
      175       44.0
      176       99.0
      177        0.5
      178       14.5
      179       39.0
      180       58.0
      181       11.5
      182       24.5
      183       87.0
      184        7.5
      185       50.5
      186       38.5
      187       69.0
      188       18.0
      189       27.5
      190       35.0
      191       29.5
      192       29.0
      193       55.0
      194       10.0
      195       38.5
      196       29.5
      197       19.5
      198       42.0
      199       53.5
      200       37.5
      201       13.5
      202        9.0
      203       69.0
      204       41.5
      205       72.5
      206       31.5
      207        3.5
      208       30.5
      209       27.5
      210       30.0
      211       52.0
      212       53.0
      213       23.5
      214       43.5
      215       87.5
      216       93.0
      217       61.0
      218      115.0
      219      142.5
      220      170.0
      221       36.5
      222       18.5
      223        0.0
      224       56.5
      225       28.0
      226       20.0
      227       15.5
      228       12.5
      229       19.0
      230       18.5
      231       22.0
      232       23.0
      233       21.0
      234       38.5
      235       76.0
      236       17.0
      237       65.5
      238       70.5
      239      102.0
      240       29.0
      241       22.5
      242       28.5
      243       10.0
      244       14.0
      245       36.5
      246       92.0
      247       77.5
      248       63.0
      249       11.5
      250       35.5
      251       74.5
      252       46.0
      253       32.0
      254       18.0
      255       26.5
      256       60.0
      257       16.0
      258       30.5
      259       56.0
      260       55.5
      261       19.0
      262       53.5
      263       47.5
      264       39.0
      265      185.0
      266       50.0
      267      109.0
      268       12.5
      269       14.0
      270       48.5
      271      109.5
      272       13.0
      273       50.0
      274       10.0
      275       74.5
      276       93.0
      277       77.0
      278       44.5
      279       59.0
      280      103.5
      281       28.5
      282       15.0
      283       48.0
      284       69.5
      285       25.5
      286      111.5
      287       70.5
      288       28.5
      289       64.5
      290        0.0
      291        3.5
      292        2.0
      293        0.0
      294       11.5
      295        3.0
      296        0.0
      297        4.0
      298        0.0
      299        4.0
      300        0.0
      301        1.0
      302        3.0
      303       38.5
      304       15.0
      305        4.0
      306        2.0
      307       12.5
      308       36.0
      309        2.0
      310       14.5
      311       20.5
      312        4.0
      313       19.0
      314        0.0
      315        2.5
      316       16.5
      317        7.0
      318        3.5
      319        0.5
      320        0.5
      321        0.5
      322        3.0
      323        4.5
      324        1.0
      325       10.0
      326       12.0
      327       11.5
      328        5.0
      329        4.0
      330        5.0
      331        4.0
      332       19.5
      333        6.5
      334       21.0
      335       16.0
      336       12.0
      337        4.5
      338       17.5
      339       30.0
      340       44.0
      341       37.5
      342       17.0
      343        1.0
      344        1.5
      345        2.0
      346       15.0
      347       25.5
      348       15.5
      349       21.0
      350       11.5
      351       28.0
      352       31.5
      353       21.0
      354        4.0
      355       31.5
      356        4.0
      357       32.5
      358       13.5
      359        0.0
      360       16.0
      361       24.5
      362       12.5
      363       23.0
      364        9.0
      365        9.0
      366        0.0
      367        0.5
      368        3.0
      369        3.0
      370        4.0
      371        0.5
      372        0.0
      373       16.5
      374       13.5
      375        8.0
      376       38.0
      377       16.5
      378       10.0
      379       14.5
      380       38.5
      381       26.5
      382       15.5
      383       38.0
      384        5.0
      385       24.0
      386       15.5
      387       40.0
      388       16.5
      389       18.0
      390       30.5
      391        6.5
      392       26.0
      393        5.0
      394       23.0
      395       31.0
      396       40.0
      397       55.5
      398       26.0
      399       43.5
      400       21.0
      401       26.5
      402       18.0
      403       28.5
      404       48.0
      405       32.5
      406       34.5
      407       16.0
      408       31.5
      409       30.0
      410       37.0
      411       30.5
      412       49.0
      413       10.5
      414       16.5
      415        4.0
      416       20.0
      417        0.5
      418       15.5
      419        0.0
      420        1.0
      421        0.0
      422       42.0
      423        9.0
      424        0.0
      425        0.0
      426        0.0
      427        0.0
      428        0.0
      429        0.0
      430        0.0
      431       20.5
      432        0.0
      433        6.0
      434        0.0
      435        0.0
      436        0.0
      437        0.5
      438        8.5
      439       25.0
      440        0.0
      441        0.0
      442        0.0
      443        0.0
      444        0.0
      445        9.0
      446        6.5
      447        4.0
      448       13.0
      449       15.5
      450        7.5
      451        0.5
      452        7.5
      453        2.0
      454        3.5
      455        4.5
      456        6.5
      457        5.0
      458        2.5
      459        4.0
      460       18.5
      461        0.5
      462        4.0
      463       10.0
      464       45.0
      465        0.0
      466       11.5
      467       12.5
      468        0.0
      469        6.5
      470       18.0
      471        2.5
      472       15.5
      473        5.0
      474        7.5
      475        6.0
      476        0.5
      477        5.0
      478        7.0
      479       12.0
      480       15.0
      481        0.0
      482        0.5
      483       15.5
      484       18.0
      485       16.5
      486       19.5
      487        0.0
      488        0.0
      489        4.0
      490       16.0
      491        8.0
      492        9.5
      493       13.0
      494        2.5
      495        7.0
      496       10.5
      497       11.5
      498        4.5
      499        1.5
      500        4.0
      501        3.5
      502        0.0
      503        7.0
      504        0.0
      505        0.5
      506        0.5
      507        5.0
      508        6.5
      509        2.0
      510        5.0
      511       31.5
      512       18.5
      513        9.0
      514        1.0
      515       15.0
      516       15.5
      517        6.5
      518        5.5
      519       11.5
      520        4.5
      521       15.0
      522       10.5
      523       18.0
      524       37.5
      525       19.0
      526        3.0
      527        2.5
      528        3.5
      529       16.5
      530        1.0
      531        6.5
      532        5.0
      533        0.5
      534        9.0
      535        9.0
      536        1.5
      537        6.0
      538        2.0
      539        7.0
      540        9.0
      541       13.0
      542       10.5
      543        4.0
      544        1.0
      545        0.0
      546       33.0
      547       15.0
      548        1.0
      549       18.5
      550       22.0
      551       12.5
      552        4.5
      553       23.5
      554       13.5
      555       43.0
      556       35.5
      557       22.5
      558       18.5
      559        3.0
      560       22.5
      561       16.5
      562       22.5
      563        0.0
      564       26.0
      565        8.5
      566       16.0
      567        2.5
      568       23.5
      569        0.5
      570        7.0
      571        1.0
      572       28.5
      573       41.0
      574       10.5
      575        3.0
      576       16.5
      577       18.5
      578       20.0
      579        1.0
      580       26.0
      581       11.0
      582       26.0
      583       90.5
      584       18.0
      585       26.5
      586        4.5
      587       33.5
      588      100.0
      589       20.0
      590       21.5
      591        7.0
      592       25.0
      593       45.5
      594       19.0
      595       11.0
      596       64.0
      597        6.0
      598       35.0
      599       17.5
      600       45.0
      601      100.0
      602        0.0
      603        7.0
      604       15.0
      605        7.0
      606       23.5
      607       25.5
      608        7.0
      609       22.5
      610       19.5
      611       28.5
      612       14.0
      613        7.0
      614        3.5
      615       22.5
      616        0.0
      617       11.0
      618       12.0
      619       25.5
      620       40.0
      621       32.0
      622        5.0
      623       19.0
      624       31.0
      625       55.0
      626       23.0
      627       25.0
      628        7.5
      629       11.5
      630        0.0
      631       26.5
      632       16.5
      633        9.5
      634       24.0
      635       28.0
      636       38.0
      637       33.5
      638       30.5
      639       18.5
      640       15.5
      641        7.5
      642        7.0
      643        5.0
      644       18.0
      645       47.5
      646       34.0
      647       13.0
      648        9.0
      649       35.0
      650       82.5
      651       31.0
      652       17.5
      653       27.0
      654       15.0
      655      200.0
      656       45.0
      657      100.0
      658       65.0
      659       16.0
      660        4.0
      661       13.0
      662       75.0
      663       22.0
      664       38.0
      665       12.0
      666        7.5
      667       14.0
      668       20.5
      669        2.0
      670        4.0
      671        3.0
      672        7.5
      673       27.5
      674       23.5
      675       18.5
      676        8.0
      677       24.0
      678       20.0
      679        5.5
      680       42.5
      681       15.0
      682       27.5
      683        7.5
      684       13.0
      685       54.0
      686       35.5
      687       27.0
      688       38.5
      689       45.0
      690       14.0
      691       36.5
      692       47.5
      693       12.0
      694       20.0
      695       29.5
      696       34.0
      697       29.5
      698       16.5
      699        6.5
      700       34.5
      701       28.5
      702       60.0
      703      109.5
      704       17.5
      705       45.5
      706        3.0
      707       48.0
      708       23.5
      709       22.5
      710       45.0
      711       10.0
      712       32.5
      713       17.5
      714       26.5
      715       24.5
      716       17.5
      717       46.5
      718       12.0
      719       80.0
      720       14.0
      721       40.0
      722       21.5
      723       25.5
      724       48.5
      725       45.0
      726       40.5
      727       17.0
      728       24.5
      729       33.5
      730        7.5
      731      137.5
      732       50.0
      733       45.0
      734       53.0
      735       85.0
      736       30.0
      737       70.0
      738       16.0
      739       40.0
      740       22.5
      741       37.5
      742       32.5
      743       21.0
      744       12.5
      745       26.5
      746       60.0
      747       31.0
      748       10.0
      749       17.0
      750       12.0
      751       18.5
      752       40.5
      753       12.0
      754       67.0
      755       34.5
      756       16.5
      757       27.5
      758       37.0
      759      142.5
      760        8.5
      761       12.0
      762       14.5
      763       39.5
      764       30.5
      765       15.0
      766       25.0
      767       24.5
      768       15.5
      769       70.5
      770       13.0
      771       10.5
      772       16.0
      773       57.5
      774        9.0
      775       28.5
      776       19.0
      777       50.0
      778      130.0
      779       40.0
      780       11.0
      781        9.0
      782       25.0
      783       18.0
      784        9.5
      785       30.0
      786       22.0
      787       91.0
      788       11.5
      789       20.0
      790       11.0
      791       17.0
      792       13.5
      793       35.0
      794       57.5
      795       30.0
      796       17.5
      797       30.0
      798       60.0
      799       10.0
      800      155.0
      801        7.5
      802       15.0
      803       17.5
      804       45.0
      805       45.0
      806       11.5
      807       97.5
      808      250.0
      809       75.0
      810       75.0
      811       60.0
      812       21.0
      813       42.5
      814      160.0
      815       22.0
      816        0.0
      817       12.0
      818       10.0
      819       60.0
      820       80.0
      821       20.0
      822       15.0
      823       24.5
      824        7.5
      825       55.0
      826       30.0
      827       15.0
      828       35.0
      829      130.0
      830       65.0
      831      185.0
      832       60.0
      833       62.5
      834      180.0
      835       53.5
      836       17.5
      837       61.0
      838        0.0
      839      175.0
      840       55.0
      841       25.0
      842       80.0
      843       63.0
      844      115.0
      845       65.0
      846       65.0
      847       17.0
      848       20.0
      849      130.0
      850      180.0
      851       47.5
      852       37.5
      853        0.0
      854       11.5
      855       11.5
      856        5.0
      857       24.0
      858       68.5
      859       14.0
      860       16.0
      861        4.5
      862       10.0
      863       13.0
      864       22.5
      865       37.5
      866       15.5
      867       14.0
      868       19.0
      869      102.5
      870       18.0
      871       28.5
      872       25.0
      873       26.0
      874       13.5
      875        3.5
      876       33.5
      877       35.0
      878        0.5
      879       32.5
      880       36.0
      881       31.5
      882        5.5
      883       18.0
      884       35.0
      885       16.5
      886       29.5
      887       49.0
      888       26.5
      889       30.5
      890       30.5
      891       35.0
      892        5.5
      893       30.0
      894       36.0
      895       52.5
      896       21.0
      897       37.5
      898       25.5
      899       19.0
      900       34.0
      901       28.5
      902       28.0
      903      140.0
      904       50.5
      905       21.0
      906       10.0
      907       18.0
      908       38.5
      909       33.0
      910       15.0
      911       32.5
      912       20.0
      913       55.0
      914       15.0
      915        4.0
      916        7.5
      917       17.0
      918        4.5
      919        2.5
      920        8.0
      921       21.0
      922       24.0
      923       27.0
      924       26.0
      925       24.5
      926       37.5
      927       14.0
      928       25.0
      929       27.5
      930       20.0
      931       21.5
      932       20.0
      933       12.5
      934       42.5
      935       30.0
      936       29.5
      937       39.5
      938       13.0
      939       12.5
      940       72.5
      941       22.5
      942       13.5
      943       11.0
      944       17.5
      945       40.0
      946       46.0
      947       24.0
      948       17.0
      949       27.5
      950       25.0
      951       29.5
      952        8.0
      953       37.5
      954       21.5
      955       17.5
      956       29.0
      957       20.0
      958       20.0
      959       32.5
      960       19.5
      961       14.5
      962       36.5
      963       18.5
      964       28.5
      965       29.0
      966       27.5
      967       57.5
      968       22.5
      969       10.5
      970       19.5
      971       33.5
      972      100.0
      973       72.5
      974       32.5
      975       25.0
      976       31.5
      977       45.0
      978       15.0
      979       10.5
      980       34.5
      981       22.0
      982       57.5
      983       30.0
      984       21.0
      985       19.0
      986       19.0
      987       22.5
      988       20.5
      989       16.5
      990       24.5
      991       13.5
      992       39.0
      993       17.0
      994       28.0
      995       17.5
      996       27.0
      997       23.5
      998       22.0
      999       10.5
      1000      24.0
      1001      27.0
      1002      28.0
      1003      21.5
      1004      15.0
      1005      43.5
      1006      12.0
      1007      21.5
      1008      43.5
      1009      16.0
      1010      17.0
      1011       0.5
      1012      14.5
      1013      14.5
      1014      10.5
      1015      12.5
      1016      15.0
      1017       4.0
      1018      14.0
      1019       5.5
      1020      23.5
      1021      11.0
      1022      35.0
      1023       0.0
      1024       9.5
      1025       2.5
      1026       9.0
      1027      15.0
      1028      32.5
      1029       7.0
      1030       9.5
      1031      19.5
      1032      12.5
      1033      23.0
      1034      29.5
      1035      17.0
      1036      25.0
      1037      10.0
      1038       9.5
      1039       9.0
      1040       3.0
      1041      13.5
      1042       0.5
      1043       6.0
      1044      32.0
      1045       4.5
      1046      20.0
      1047      10.5
      1048      14.0
      1049       5.5
      1050      32.0
      1051       8.0
      1052      23.5
      1053      30.0
      1054      25.5
      1055       0.0
      1056       9.0
      1057       2.5
      1058      58.0
      1059      21.0
      1060      14.5
      1061      20.0
      1062      17.0
      1063      20.0
      1064      40.5

