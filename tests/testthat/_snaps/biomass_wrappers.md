# getDomainTotalBiomass handles Base Case (NULL growth parameters) exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD total_biomass        var   n  nm     N      NM
      1 2012 FLA KEYS   EPI MORI      447588.2 4693985527 416 803 14095 3190455
        STAGE_LEVEL
      1           2

# getDomainTotalBiomass handles custom list parameters exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD total_biomass        var   n  nm     N      NM
      1 2012 FLA KEYS   EPI MORI      447588.2 4693985527 416 803 14095 3190455
        STAGE_LEVEL
      1           2

# getDomainTotalBiomass handles custom dataframe parameters and grouping exactly as baseline

    Code
      res
    Output
        YEAR   REGION    GROUP total_biomass         var   n  nm     N      NM
      1 2012 FLA KEYS Groupers       1232560 18343305220 416 803 14095 3190455
        STAGE_LEVEL
      1           2

# getDomainTotalBiomass handles Kitchen Sink Filtering exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD total_biomass          var   n  nm     N      NM
      1 2012 FLA KEYS   EPI MORI      306.4363 7.443128e+04 302 587 13285 3007109
      2 2012 FLA KEYS   EPI MORI   419521.6681 4.655243e+09 302 587 13285 3007109
      3 2012 FLA KEYS   EPI MORI   419828.1044 4.653538e+09 302 587 13285 3007109
        STAGE_LEVEL length_class
      1           2         < 20
      2           2        >= 20
      3           2          all

# Biomass wrappers correctly calculate mixed designs across multiple years

    Code
      res_domain_tot
    Output
        YEAR   REGION SPECIES_CD total_biomass        var   n  nm      N      NM
      1 2012 FLA KEYS   EPI MORI      542207.9 7409135757 416 803  16977 3842807
      2 2022 FLA KEYS   EPI MORI      259569.7 2556386999 648  NA 270545 3827429
        STAGE_LEVEL
      1           2
      2           1

---

    Code
      res_strat_biomass
    Output
         YEAR   REGION STRAT PROT SPECIES_CD    biomass          var   n  nm      N
      1  2012 FLA KEYS  FDLR    0   EPI MORI 0.02408397 0.0002295476  40  77   2220
      2  2012 FLA KEYS  FMLR    0   EPI MORI 0.09609344 0.0014448747 129 245   6674
      3  2012 FLA KEYS  FMLR    1   EPI MORI 0.18764423 0.0051332700  43  84    272
      4  2012 FLA KEYS  FSLR    0   EPI MORI 0.03769080 0.0004157901  29  58    846
      5  2012 FLA KEYS  FSLR    1   EPI MORI 0.21935265 0.0075195562  22  39    120
      6  2012 FLA KEYS  HRRF    0   EPI MORI 0.00000000 0.0000000000   9  17    291
      7  2012 FLA KEYS  HRRF    1   EPI MORI 0.00000000 0.0000000000  17  30    149
      8  2012 FLA KEYS  INPR    0   EPI MORI 0.01400378 0.0001960788   6  12    386
      9  2012 FLA KEYS  INPR    1   EPI MORI 0.00000000 0.0000000000   3   6     35
      10 2012 FLA KEYS  MCPR    0   EPI MORI 0.28197647 0.0046092062  60 120   3711
      11 2012 FLA KEYS  MCPR    1   EPI MORI 0.34686693 0.0220646958  12  24     87
      12 2012 FLA KEYS  OFPR    0   EPI MORI 0.23667516 0.0033099892  29  58   2103
      13 2012 FLA KEYS  OFPR    1   EPI MORI 0.14016465 0.0032322235  17  33     83
      14 2022 FLA KEYS   S01    0   EPI MORI 0.01527685 0.0002329668  11  NA   6183
      15 2022 FLA KEYS   S01    1   EPI MORI 0.00000000 0.0000000000   6  NA    556
      16 2022 FLA KEYS   S02    0   EPI MORI 0.03739652 0.0006904562  17  NA  50062
      17 2022 FLA KEYS   S02    1   EPI MORI 0.86827015 0.7511188406   4  NA   1087
      18 2022 FLA KEYS   S03    0   EPI MORI 0.08257288 0.0005589312  79  NA   9507
      19 2022 FLA KEYS   S03    1   EPI MORI 0.04963021 0.0023481461  12  NA    257
      20 2022 FLA KEYS   S05    0   EPI MORI 0.08919625 0.0008046109  81  NA 102208
      21 2022 FLA KEYS   S05    1   EPI MORI 0.07567695 0.0017657643  32  NA   5062
      22 2022 FLA KEYS   S06    0   EPI MORI 0.11140417 0.0022482143  89  NA  20025
      23 2022 FLA KEYS   S06    1   EPI MORI 0.06517739 0.0010357577 106  NA   2402
      24 2022 FLA KEYS   S08    0   EPI MORI 0.02015114 0.0001303064  34  NA  29203
      25 2022 FLA KEYS   S08    1   EPI MORI 0.00000000           NA   1  NA   1014
      26 2022 FLA KEYS   S09    0   EPI MORI 0.06994927 0.0008177537 102  NA  18553
      27 2022 FLA KEYS   S09    1   EPI MORI 0.19653428 0.0080077194  23  NA    679
      28 2022 FLA KEYS   S11    0   EPI MORI 0.03086521 0.0006047783  51  NA  23747
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
           YEAR   REGION STRAT PROT PRIMARY_SAMPLE_UNIT SPECIES_CD m          var
      1    2012 FLA KEYS  FDLR    0                065U   EPI MORI 2 0.000000e+00
      2    2012 FLA KEYS  FDLR    0                066U   EPI MORI 2 0.000000e+00
      3    2012 FLA KEYS  FDLR    0                074U   EPI MORI 2 0.000000e+00
      4    2012 FLA KEYS  FDLR    0                077U   EPI MORI 1           NA
      5    2012 FLA KEYS  FDLR    0                087U   EPI MORI 2 0.000000e+00
      6    2012 FLA KEYS  FDLR    0                090U   EPI MORI 2 0.000000e+00
      7    2012 FLA KEYS  FDLR    0                109U   EPI MORI 2 0.000000e+00
      8    2012 FLA KEYS  FDLR    0                156U   EPI MORI 2 0.000000e+00
      9    2012 FLA KEYS  FDLR    0                157U   EPI MORI 2 0.000000e+00
      10   2012 FLA KEYS  FDLR    0                160U   EPI MORI 2 5.870565e-01
      11   2012 FLA KEYS  FDLR    0                161U   EPI MORI 2 0.000000e+00
      12   2012 FLA KEYS  FDLR    0                213U   EPI MORI 2 0.000000e+00
      13   2012 FLA KEYS  FDLR    0                214U   EPI MORI 2 0.000000e+00
      14   2012 FLA KEYS  FDLR    0                226U   EPI MORI 2 0.000000e+00
      15   2012 FLA KEYS  FDLR    0                230U   EPI MORI 2 0.000000e+00
      16   2012 FLA KEYS  FDLR    0                234U   EPI MORI 2 0.000000e+00
      17   2012 FLA KEYS  FDLR    0                235U   EPI MORI 2 0.000000e+00
      18   2012 FLA KEYS  FDLR    0                237U   EPI MORI 2 0.000000e+00
      19   2012 FLA KEYS  FDLR    0                238U   EPI MORI 2 0.000000e+00
      20   2012 FLA KEYS  FDLR    0                239U   EPI MORI 2 0.000000e+00
      21   2012 FLA KEYS  FDLR    0                240U   EPI MORI 2 0.000000e+00
      22   2012 FLA KEYS  FDLR    0                241U   EPI MORI 2 0.000000e+00
      23   2012 FLA KEYS  FDLR    0                242U   EPI MORI 2 0.000000e+00
      24   2012 FLA KEYS  FDLR    0                243U   EPI MORI 2 0.000000e+00
      25   2012 FLA KEYS  FDLR    0                244U   EPI MORI 2 0.000000e+00
      26   2012 FLA KEYS  FDLR    0                246U   EPI MORI 2 0.000000e+00
      27   2012 FLA KEYS  FDLR    0                247U   EPI MORI 2 0.000000e+00
      28   2012 FLA KEYS  FDLR    0                249U   EPI MORI 2 0.000000e+00
      29   2012 FLA KEYS  FDLR    0                291U   EPI MORI 2 0.000000e+00
      30   2012 FLA KEYS  FDLR    0                313U   EPI MORI 2 0.000000e+00
      31   2012 FLA KEYS  FDLR    0                320U   EPI MORI 2 0.000000e+00
      32   2012 FLA KEYS  FDLR    0                323H   EPI MORI 1           NA
      33   2012 FLA KEYS  FDLR    0                323U   EPI MORI 1           NA
      34   2012 FLA KEYS  FDLR    0                384U   EPI MORI 2 0.000000e+00
      35   2012 FLA KEYS  FDLR    0                436U   EPI MORI 2 0.000000e+00
      36   2012 FLA KEYS  FDLR    0                906U   EPI MORI 2 0.000000e+00
      37   2012 FLA KEYS  FDLR    0                944U   EPI MORI 2 0.000000e+00
      38   2012 FLA KEYS  FDLR    0                955U   EPI MORI 2 0.000000e+00
      39   2012 FLA KEYS  FDLR    0                959U   EPI MORI 2 0.000000e+00
      40   2012 FLA KEYS  FDLR    0                960U   EPI MORI 2 7.283721e-02
      41   2012 FLA KEYS  FMLR    0                022H   EPI MORI 1           NA
      42   2012 FLA KEYS  FMLR    0                022U   EPI MORI 1           NA
      43   2012 FLA KEYS  FMLR    0                036U   EPI MORI 2 0.000000e+00
      44   2012 FLA KEYS  FMLR    0                040U   EPI MORI 2 0.000000e+00
      45   2012 FLA KEYS  FMLR    0                041U   EPI MORI 2 2.032670e-01
      46   2012 FLA KEYS  FMLR    0                046H   EPI MORI 1           NA
      47   2012 FLA KEYS  FMLR    0                046U   EPI MORI 1           NA
      48   2012 FLA KEYS  FMLR    0                047U   EPI MORI 2 0.000000e+00
      49   2012 FLA KEYS  FMLR    0                051H   EPI MORI 1           NA
      50   2012 FLA KEYS  FMLR    0                053U   EPI MORI 2 3.470806e-05
      51   2012 FLA KEYS  FMLR    0                055H   EPI MORI 1           NA
      52   2012 FLA KEYS  FMLR    0                056U   EPI MORI 2 0.000000e+00
      53   2012 FLA KEYS  FMLR    0                057U   EPI MORI 2 0.000000e+00
      54   2012 FLA KEYS  FMLR    0                058U   EPI MORI 2 0.000000e+00
      55   2012 FLA KEYS  FMLR    0                059U   EPI MORI 2 0.000000e+00
      56   2012 FLA KEYS  FMLR    0                061H   EPI MORI 1           NA
      57   2012 FLA KEYS  FMLR    0                061U   EPI MORI 1           NA
      58   2012 FLA KEYS  FMLR    0                062U   EPI MORI 2 0.000000e+00
      59   2012 FLA KEYS  FMLR    0                063U   EPI MORI 2 0.000000e+00
      60   2012 FLA KEYS  FMLR    0                064U   EPI MORI 2 0.000000e+00
      61   2012 FLA KEYS  FMLR    0                068U   EPI MORI 2 0.000000e+00
      62   2012 FLA KEYS  FMLR    0                069U   EPI MORI 2 0.000000e+00
      63   2012 FLA KEYS  FMLR    0                070U   EPI MORI 2 0.000000e+00
      64   2012 FLA KEYS  FMLR    0                071U   EPI MORI 2 0.000000e+00
      65   2012 FLA KEYS  FMLR    0                072U   EPI MORI 2 0.000000e+00
      66   2012 FLA KEYS  FMLR    0                073U   EPI MORI 2 0.000000e+00
      67   2012 FLA KEYS  FMLR    0                075U   EPI MORI 2 0.000000e+00
      68   2012 FLA KEYS  FMLR    0                076U   EPI MORI 2 0.000000e+00
      69   2012 FLA KEYS  FMLR    0                078U   EPI MORI 2 0.000000e+00
      70   2012 FLA KEYS  FMLR    0                079U   EPI MORI 2 0.000000e+00
      71   2012 FLA KEYS  FMLR    0                080U   EPI MORI 2 0.000000e+00
      72   2012 FLA KEYS  FMLR    0                082U   EPI MORI 2 7.283721e-02
      73   2012 FLA KEYS  FMLR    0                083U   EPI MORI 2 4.421401e-02
      74   2012 FLA KEYS  FMLR    0                084U   EPI MORI 2 0.000000e+00
      75   2012 FLA KEYS  FMLR    0                085U   EPI MORI 2 0.000000e+00
      76   2012 FLA KEYS  FMLR    0                086U   EPI MORI 2 0.000000e+00
      77   2012 FLA KEYS  FMLR    0                088U   EPI MORI 2 0.000000e+00
      78   2012 FLA KEYS  FMLR    0                089U   EPI MORI 2 0.000000e+00
      79   2012 FLA KEYS  FMLR    0                107U   EPI MORI 2 0.000000e+00
      80   2012 FLA KEYS  FMLR    0                116U   EPI MORI 2 0.000000e+00
      81   2012 FLA KEYS  FMLR    0                118U   EPI MORI 2 5.906056e-01
      82   2012 FLA KEYS  FMLR    0                121U   EPI MORI 2 2.957417e-03
      83   2012 FLA KEYS  FMLR    0                122U   EPI MORI 2 0.000000e+00
      84   2012 FLA KEYS  FMLR    0                123U   EPI MORI 2 0.000000e+00
      85   2012 FLA KEYS  FMLR    0                124U   EPI MORI 2 0.000000e+00
      86   2012 FLA KEYS  FMLR    0                125U   EPI MORI 2 0.000000e+00
      87   2012 FLA KEYS  FMLR    0                126U   EPI MORI 2 0.000000e+00
      88   2012 FLA KEYS  FMLR    0                127U   EPI MORI 2 1.141080e-02
      89   2012 FLA KEYS  FMLR    0                130U   EPI MORI 2 0.000000e+00
      90   2012 FLA KEYS  FMLR    0                132U   EPI MORI 2 7.093765e-02
      91   2012 FLA KEYS  FMLR    0                133U   EPI MORI 2 0.000000e+00
      92   2012 FLA KEYS  FMLR    0                134U   EPI MORI 2 0.000000e+00
      93   2012 FLA KEYS  FMLR    0                135U   EPI MORI 2 1.356351e+00
      94   2012 FLA KEYS  FMLR    0                136U   EPI MORI 2 0.000000e+00
      95   2012 FLA KEYS  FMLR    0                137U   EPI MORI 2 9.150503e-03
      96   2012 FLA KEYS  FMLR    0                138U   EPI MORI 2 3.708036e-02
      97   2012 FLA KEYS  FMLR    0                139U   EPI MORI 2 0.000000e+00
      98   2012 FLA KEYS  FMLR    0                141U   EPI MORI 2 0.000000e+00
      99   2012 FLA KEYS  FMLR    0                142U   EPI MORI 2 0.000000e+00
      100  2012 FLA KEYS  FMLR    0                143U   EPI MORI 2 0.000000e+00
      101  2012 FLA KEYS  FMLR    0                144U   EPI MORI 2 0.000000e+00
      102  2012 FLA KEYS  FMLR    0                145U   EPI MORI 2 0.000000e+00
      103  2012 FLA KEYS  FMLR    0                146U   EPI MORI 2 0.000000e+00
      104  2012 FLA KEYS  FMLR    0                147U   EPI MORI 2 0.000000e+00
      105  2012 FLA KEYS  FMLR    0                148U   EPI MORI 2 0.000000e+00
      106  2012 FLA KEYS  FMLR    0                149U   EPI MORI 2 0.000000e+00
      107  2012 FLA KEYS  FMLR    0                150U   EPI MORI 2 0.000000e+00
      108  2012 FLA KEYS  FMLR    0                151U   EPI MORI 2 0.000000e+00
      109  2012 FLA KEYS  FMLR    0                152U   EPI MORI 2 0.000000e+00
      110  2012 FLA KEYS  FMLR    0                154U   EPI MORI 2 0.000000e+00
      111  2012 FLA KEYS  FMLR    0                155U   EPI MORI 2 0.000000e+00
      112  2012 FLA KEYS  FMLR    0                158U   EPI MORI 2 0.000000e+00
      113  2012 FLA KEYS  FMLR    0                159U   EPI MORI 2 0.000000e+00
      114  2012 FLA KEYS  FMLR    0                210U   EPI MORI 2 0.000000e+00
      115  2012 FLA KEYS  FMLR    0                211U   EPI MORI 2 0.000000e+00
      116  2012 FLA KEYS  FMLR    0                212U   EPI MORI 2 0.000000e+00
      117  2012 FLA KEYS  FMLR    0                216U   EPI MORI 2 0.000000e+00
      118  2012 FLA KEYS  FMLR    0                217U   EPI MORI 2 0.000000e+00
      119  2012 FLA KEYS  FMLR    0                218U   EPI MORI 2 0.000000e+00
      120  2012 FLA KEYS  FMLR    0                219U   EPI MORI 2 0.000000e+00
      121  2012 FLA KEYS  FMLR    0                220U   EPI MORI 2 0.000000e+00
      122  2012 FLA KEYS  FMLR    0                222U   EPI MORI 2 8.527644e-02
      123  2012 FLA KEYS  FMLR    0                223U   EPI MORI 2 0.000000e+00
      124  2012 FLA KEYS  FMLR    0                224U   EPI MORI 2 1.244418e-01
      125  2012 FLA KEYS  FMLR    0                225U   EPI MORI 2 9.485819e-01
      126  2012 FLA KEYS  FMLR    0                227U   EPI MORI 1           NA
      127  2012 FLA KEYS  FMLR    0                228U   EPI MORI 2 0.000000e+00
      128  2012 FLA KEYS  FMLR    0                229U   EPI MORI 2 0.000000e+00
      129  2012 FLA KEYS  FMLR    0                231U   EPI MORI 2 0.000000e+00
      130  2012 FLA KEYS  FMLR    0                232U   EPI MORI 2 0.000000e+00
      131  2012 FLA KEYS  FMLR    0                233U   EPI MORI 2 0.000000e+00
      132  2012 FLA KEYS  FMLR    0                236U   EPI MORI 2 9.944227e-02
      133  2012 FLA KEYS  FMLR    0                245U   EPI MORI 2 0.000000e+00
      134  2012 FLA KEYS  FMLR    0                289U   EPI MORI 2 0.000000e+00
      135  2012 FLA KEYS  FMLR    0                293U   EPI MORI 2 0.000000e+00
      136  2012 FLA KEYS  FMLR    0                295U   EPI MORI 2 0.000000e+00
      137  2012 FLA KEYS  FMLR    0                297U   EPI MORI 2 0.000000e+00
      138  2012 FLA KEYS  FMLR    0                300U   EPI MORI 2 0.000000e+00
      139  2012 FLA KEYS  FMLR    0                301U   EPI MORI 2 0.000000e+00
      140  2012 FLA KEYS  FMLR    0                303U   EPI MORI 2 0.000000e+00
      141  2012 FLA KEYS  FMLR    0                305U   EPI MORI 2 0.000000e+00
      142  2012 FLA KEYS  FMLR    0                306U   EPI MORI 2 0.000000e+00
      143  2012 FLA KEYS  FMLR    0                307U   EPI MORI 2 0.000000e+00
      144  2012 FLA KEYS  FMLR    0                310U   EPI MORI 2 0.000000e+00
      145  2012 FLA KEYS  FMLR    0                311U   EPI MORI 2 1.622751e-01
      146  2012 FLA KEYS  FMLR    0                312U   EPI MORI 2 0.000000e+00
      147  2012 FLA KEYS  FMLR    0                314U   EPI MORI 2 9.944227e-02
      148  2012 FLA KEYS  FMLR    0                315U   EPI MORI 2 0.000000e+00
      149  2012 FLA KEYS  FMLR    0                316U   EPI MORI 2 0.000000e+00
      150  2012 FLA KEYS  FMLR    0                317U   EPI MORI 2 0.000000e+00
      151  2012 FLA KEYS  FMLR    0                319U   EPI MORI 2 0.000000e+00
      152  2012 FLA KEYS  FMLR    0                321U   EPI MORI 2 2.116569e-02
      153  2012 FLA KEYS  FMLR    0                322U   EPI MORI 2 0.000000e+00
      154  2012 FLA KEYS  FMLR    0                324U   EPI MORI 2 3.708036e-02
      155  2012 FLA KEYS  FMLR    0                325U   EPI MORI 2 0.000000e+00
      156  2012 FLA KEYS  FMLR    0                326U   EPI MORI 2 8.539274e-01
      157  2012 FLA KEYS  FMLR    0                855U   EPI MORI 2 1.734574e-02
      158  2012 FLA KEYS  FMLR    0                856U   EPI MORI 2 0.000000e+00
      159  2012 FLA KEYS  FMLR    0                889U   EPI MORI 2 0.000000e+00
      160  2012 FLA KEYS  FMLR    0                898U   EPI MORI 2 0.000000e+00
      161  2012 FLA KEYS  FMLR    0                899U   EPI MORI 2 0.000000e+00
      162  2012 FLA KEYS  FMLR    0                901H   EPI MORI 1           NA
      163  2012 FLA KEYS  FMLR    0                901U   EPI MORI 1           NA
      164  2012 FLA KEYS  FMLR    0                910U   EPI MORI 2 0.000000e+00
      165  2012 FLA KEYS  FMLR    0                938U   EPI MORI 2 0.000000e+00
      166  2012 FLA KEYS  FMLR    0                941U   EPI MORI 2 0.000000e+00
      167  2012 FLA KEYS  FMLR    0                988H   EPI MORI 1           NA
      168  2012 FLA KEYS  FMLR    0                988U   EPI MORI 1           NA
      169  2012 FLA KEYS  FMLR    0                A03U   EPI MORI 2 0.000000e+00
      170  2012 FLA KEYS  FMLR    1                248U   EPI MORI 2 0.000000e+00
      171  2012 FLA KEYS  FMLR    1                342U   EPI MORI 2 1.638206e+00
      172  2012 FLA KEYS  FMLR    1                343U   EPI MORI 2 0.000000e+00
      173  2012 FLA KEYS  FMLR    1                347U   EPI MORI 2 0.000000e+00
      174  2012 FLA KEYS  FMLR    1                349U   EPI MORI 2 0.000000e+00
      175  2012 FLA KEYS  FMLR    1                359U   EPI MORI 2 1.734574e-02
      176  2012 FLA KEYS  FMLR    1                360U   EPI MORI 2 0.000000e+00
      177  2012 FLA KEYS  FMLR    1                365U   EPI MORI 2 0.000000e+00
      178  2012 FLA KEYS  FMLR    1                366U   EPI MORI 2 0.000000e+00
      179  2012 FLA KEYS  FMLR    1                367U   EPI MORI 2 0.000000e+00
      180  2012 FLA KEYS  FMLR    1                368U   EPI MORI 2 0.000000e+00
      181  2012 FLA KEYS  FMLR    1                370U   EPI MORI 2 0.000000e+00
      182  2012 FLA KEYS  FMLR    1                371U   EPI MORI 2 2.646677e-01
      183  2012 FLA KEYS  FMLR    1                380U   EPI MORI 2 0.000000e+00
      184  2012 FLA KEYS  FMLR    1                381U   EPI MORI 2 0.000000e+00
      185  2012 FLA KEYS  FMLR    1                382U   EPI MORI 2 1.894310e+00
      186  2012 FLA KEYS  FMLR    1                383U   EPI MORI 2 0.000000e+00
      187  2012 FLA KEYS  FMLR    1                385U   EPI MORI 2 0.000000e+00
      188  2012 FLA KEYS  FMLR    1                386U   EPI MORI 2 4.216662e+00
      189  2012 FLA KEYS  FMLR    1                387U   EPI MORI 2 0.000000e+00
      190  2012 FLA KEYS  FMLR    1                389U   EPI MORI 2 1.734574e-02
      191  2012 FLA KEYS  FMLR    1                390U   EPI MORI 2 1.337181e-01
      192  2012 FLA KEYS  FMLR    1                391U   EPI MORI 2 0.000000e+00
      193  2012 FLA KEYS  FMLR    1                395U   EPI MORI 2 0.000000e+00
      194  2012 FLA KEYS  FMLR    1                396U   EPI MORI 2 0.000000e+00
      195  2012 FLA KEYS  FMLR    1                398U   EPI MORI 2 7.134856e-03
      196  2012 FLA KEYS  FMLR    1                400U   EPI MORI 2 0.000000e+00
      197  2012 FLA KEYS  FMLR    1                401U   EPI MORI 2 7.410991e-02
      198  2012 FLA KEYS  FMLR    1                406U   EPI MORI 2 0.000000e+00
      199  2012 FLA KEYS  FMLR    1                407U   EPI MORI 2 9.944227e-02
      200  2012 FLA KEYS  FMLR    1                421U   EPI MORI 2 0.000000e+00
      201  2012 FLA KEYS  FMLR    1                422U   EPI MORI 2 0.000000e+00
      202  2012 FLA KEYS  FMLR    1                425H   EPI MORI 1           NA
      203  2012 FLA KEYS  FMLR    1                428U   EPI MORI 1           NA
      204  2012 FLA KEYS  FMLR    1                431U   EPI MORI 2 0.000000e+00
      205  2012 FLA KEYS  FMLR    1                432U   EPI MORI 2 0.000000e+00
      206  2012 FLA KEYS  FMLR    1                433U   EPI MORI 2 2.265654e+00
      207  2012 FLA KEYS  FMLR    1                434U   EPI MORI 2 0.000000e+00
      208  2012 FLA KEYS  FMLR    1                435U   EPI MORI 2 0.000000e+00
      209  2012 FLA KEYS  FMLR    1                437U   EPI MORI 2 0.000000e+00
      210  2012 FLA KEYS  FMLR    1                438U   EPI MORI 2 0.000000e+00
      211  2012 FLA KEYS  FMLR    1                439U   EPI MORI 2 8.762483e-01
      212  2012 FLA KEYS  FMLR    1                440U   EPI MORI 2 0.000000e+00
      213  2012 FLA KEYS  FSLR    0                037U   EPI MORI 2 0.000000e+00
      214  2012 FLA KEYS  FSLR    0                038U   EPI MORI 2 0.000000e+00
      215  2012 FLA KEYS  FSLR    0                039U   EPI MORI 2 0.000000e+00
      216  2012 FLA KEYS  FSLR    0                043U   EPI MORI 2 0.000000e+00
      217  2012 FLA KEYS  FSLR    0                044U   EPI MORI 2 0.000000e+00
      218  2012 FLA KEYS  FSLR    0                045U   EPI MORI 2 0.000000e+00
      219  2012 FLA KEYS  FSLR    0                048U   EPI MORI 2 0.000000e+00
      220  2012 FLA KEYS  FSLR    0                049U   EPI MORI 2 0.000000e+00
      221  2012 FLA KEYS  FSLR    0                052U   EPI MORI 2 0.000000e+00
      222  2012 FLA KEYS  FSLR    0                110U   EPI MORI 2 0.000000e+00
      223  2012 FLA KEYS  FSLR    0                113U   EPI MORI 2 0.000000e+00
      224  2012 FLA KEYS  FSLR    0                183U   EPI MORI 2 1.411961e-02
      225  2012 FLA KEYS  FSLR    0                202U   EPI MORI 2 1.231720e-01
      226  2012 FLA KEYS  FSLR    0                203U   EPI MORI 2 0.000000e+00
      227  2012 FLA KEYS  FSLR    0                204U   EPI MORI 2 4.421401e-02
      228  2012 FLA KEYS  FSLR    0                205U   EPI MORI 2 0.000000e+00
      229  2012 FLA KEYS  FSLR    0                206U   EPI MORI 2 2.566417e-02
      230  2012 FLA KEYS  FSLR    0                207U   EPI MORI 2 0.000000e+00
      231  2012 FLA KEYS  FSLR    0                208U   EPI MORI 2 0.000000e+00
      232  2012 FLA KEYS  FSLR    0                287U   EPI MORI 2 0.000000e+00
      233  2012 FLA KEYS  FSLR    0                288U   EPI MORI 2 0.000000e+00
      234  2012 FLA KEYS  FSLR    0                290U   EPI MORI 2 0.000000e+00
      235  2012 FLA KEYS  FSLR    0                292U   EPI MORI 2 0.000000e+00
      236  2012 FLA KEYS  FSLR    0                302U   EPI MORI 2 0.000000e+00
      237  2012 FLA KEYS  FSLR    0                304U   EPI MORI 2 0.000000e+00
      238  2012 FLA KEYS  FSLR    0                308U   EPI MORI 2 0.000000e+00
      239  2012 FLA KEYS  FSLR    0                309U   EPI MORI 2 0.000000e+00
      240  2012 FLA KEYS  FSLR    0                393U   EPI MORI 2 0.000000e+00
      241  2012 FLA KEYS  FSLR    0                883U   EPI MORI 2 0.000000e+00
      242  2012 FLA KEYS  FSLR    1                341H   EPI MORI 1           NA
      243  2012 FLA KEYS  FSLR    1                341U   EPI MORI 1           NA
      244  2012 FLA KEYS  FSLR    1                344U   EPI MORI 2 0.000000e+00
      245  2012 FLA KEYS  FSLR    1                348H   EPI MORI 1           NA
      246  2012 FLA KEYS  FSLR    1                351U   EPI MORI 2 0.000000e+00
      247  2012 FLA KEYS  FSLR    1                352U   EPI MORI 2 2.032670e-01
      248  2012 FLA KEYS  FSLR    1                355U   EPI MORI 2 4.476609e-03
      249  2012 FLA KEYS  FSLR    1                358U   EPI MORI 2 0.000000e+00
      250  2012 FLA KEYS  FSLR    1                361U   EPI MORI 2 1.165319e+00
      251  2012 FLA KEYS  FSLR    1                369U   EPI MORI 2 1.566982e+00
      252  2012 FLA KEYS  FSLR    1                388U   EPI MORI 2 0.000000e+00
      253  2012 FLA KEYS  FSLR    1                392H   EPI MORI 1           NA
      254  2012 FLA KEYS  FSLR    1                392U   EPI MORI 1           NA
      255  2012 FLA KEYS  FSLR    1                399U   EPI MORI 2 0.000000e+00
      256  2012 FLA KEYS  FSLR    1                402U   EPI MORI 2 0.000000e+00
      257  2012 FLA KEYS  FSLR    1                403U   EPI MORI 2 0.000000e+00
      258  2012 FLA KEYS  FSLR    1                405U   EPI MORI 2 0.000000e+00
      259  2012 FLA KEYS  FSLR    1                419U   EPI MORI 2 1.288306e+00
      260  2012 FLA KEYS  FSLR    1                420U   EPI MORI 2 0.000000e+00
      261  2012 FLA KEYS  FSLR    1                429U   EPI MORI 2 7.213507e-02
      262  2012 FLA KEYS  FSLR    1                A89U   EPI MORI 2 0.000000e+00
      263  2012 FLA KEYS  FSLR    1                A90U   EPI MORI 2 1.877202e-01
      264  2012 FLA KEYS  HRRF    0                042U   EPI MORI 2 0.000000e+00
      265  2012 FLA KEYS  HRRF    0                050U   EPI MORI 2 0.000000e+00
      266  2012 FLA KEYS  HRRF    0                051U   EPI MORI 1           NA
      267  2012 FLA KEYS  HRRF    0                067U   EPI MORI 2 0.000000e+00
      268  2012 FLA KEYS  HRRF    0                196U   EPI MORI 2 0.000000e+00
      269  2012 FLA KEYS  HRRF    0                198U   EPI MORI 2 0.000000e+00
      270  2012 FLA KEYS  HRRF    0                215U   EPI MORI 2 0.000000e+00
      271  2012 FLA KEYS  HRRF    0                294U   EPI MORI 2 0.000000e+00
      272  2012 FLA KEYS  HRRF    0                957U   EPI MORI 2 0.000000e+00
      273  2012 FLA KEYS  HRRF    1                346U   EPI MORI 2 0.000000e+00
      274  2012 FLA KEYS  HRRF    1                348U   EPI MORI 1           NA
      275  2012 FLA KEYS  HRRF    1                350U   EPI MORI 2 0.000000e+00
      276  2012 FLA KEYS  HRRF    1                353U   EPI MORI 2 0.000000e+00
      277  2012 FLA KEYS  HRRF    1                354U   EPI MORI 2 0.000000e+00
      278  2012 FLA KEYS  HRRF    1                356U   EPI MORI 2 0.000000e+00
      279  2012 FLA KEYS  HRRF    1                357U   EPI MORI 2 0.000000e+00
      280  2012 FLA KEYS  HRRF    1                394U   EPI MORI 2 0.000000e+00
      281  2012 FLA KEYS  HRRF    1                404U   EPI MORI 2 0.000000e+00
      282  2012 FLA KEYS  HRRF    1                418H   EPI MORI 1           NA
      283  2012 FLA KEYS  HRRF    1                423U   EPI MORI 2 0.000000e+00
      284  2012 FLA KEYS  HRRF    1                424U   EPI MORI 2 0.000000e+00
      285  2012 FLA KEYS  HRRF    1                425U   EPI MORI 1           NA
      286  2012 FLA KEYS  HRRF    1                426U   EPI MORI 2 0.000000e+00
      287  2012 FLA KEYS  HRRF    1                427U   EPI MORI 2 0.000000e+00
      288  2012 FLA KEYS  HRRF    1                428H   EPI MORI 1           NA
      289  2012 FLA KEYS  HRRF    1                430U   EPI MORI 2 0.000000e+00
      290  2012 FLA KEYS  INPR    0                091U   EPI MORI 2 0.000000e+00
      291  2012 FLA KEYS  INPR    0                092U   EPI MORI 2 0.000000e+00
      292  2012 FLA KEYS  INPR    0                093U   EPI MORI 2 0.000000e+00
      293  2012 FLA KEYS  INPR    0                250U   EPI MORI 2 1.411961e-02
      294  2012 FLA KEYS  INPR    0                253U   EPI MORI 2 0.000000e+00
      295  2012 FLA KEYS  INPR    0                257U   EPI MORI 2 0.000000e+00
      296  2012 FLA KEYS  INPR    1                327U   EPI MORI 2 0.000000e+00
      297  2012 FLA KEYS  INPR    1                329U   EPI MORI 2 0.000000e+00
      298  2012 FLA KEYS  INPR    1                330U   EPI MORI 2 0.000000e+00
      299  2012 FLA KEYS  MCPR    0                005U   EPI MORI 2 9.035042e+00
      300  2012 FLA KEYS  MCPR    0                006U   EPI MORI 2 0.000000e+00
      301  2012 FLA KEYS  MCPR    0                008U   EPI MORI 2 0.000000e+00
      302  2012 FLA KEYS  MCPR    0                009U   EPI MORI 2 1.773473e-01
      303  2012 FLA KEYS  MCPR    0                010U   EPI MORI 2 2.032670e-01
      304  2012 FLA KEYS  MCPR    0                011U   EPI MORI 2 0.000000e+00
      305  2012 FLA KEYS  MCPR    0                012U   EPI MORI 2 7.890580e-01
      306  2012 FLA KEYS  MCPR    0                013U   EPI MORI 2 1.355016e+01
      307  2012 FLA KEYS  MCPR    0                023U   EPI MORI 2 0.000000e+00
      308  2012 FLA KEYS  MCPR    0                024U   EPI MORI 2 2.511474e-01
      309  2012 FLA KEYS  MCPR    0                025U   EPI MORI 2 4.421401e-02
      310  2012 FLA KEYS  MCPR    0                095U   EPI MORI 2 0.000000e+00
      311  2012 FLA KEYS  MCPR    0                096U   EPI MORI 2 0.000000e+00
      312  2012 FLA KEYS  MCPR    0                097U   EPI MORI 2 1.141080e-02
      313  2012 FLA KEYS  MCPR    0                098U   EPI MORI 2 0.000000e+00
      314  2012 FLA KEYS  MCPR    0                099U   EPI MORI 2 3.660168e-02
      315  2012 FLA KEYS  MCPR    0                101U   EPI MORI 2 5.013964e-03
      316  2012 FLA KEYS  MCPR    0                164U   EPI MORI 2 1.515004e+00
      317  2012 FLA KEYS  MCPR    0                166U   EPI MORI 2 2.032670e-01
      318  2012 FLA KEYS  MCPR    0                167U   EPI MORI 2 0.000000e+00
      319  2012 FLA KEYS  MCPR    0                170U   EPI MORI 2 8.527644e-02
      320  2012 FLA KEYS  MCPR    0                171U   EPI MORI 2 1.161786e+00
      321  2012 FLA KEYS  MCPR    0                172U   EPI MORI 2 8.527644e-02
      322  2012 FLA KEYS  MCPR    0                173U   EPI MORI 2 0.000000e+00
      323  2012 FLA KEYS  MCPR    0                174U   EPI MORI 2 3.708036e-02
      324  2012 FLA KEYS  MCPR    0                175U   EPI MORI 2 1.337181e-01
      325  2012 FLA KEYS  MCPR    0                177U   EPI MORI 2 2.566417e-02
      326  2012 FLA KEYS  MCPR    0                178U   EPI MORI 2 1.421950e+00
      327  2012 FLA KEYS  MCPR    0                179U   EPI MORI 2 0.000000e+00
      328  2012 FLA KEYS  MCPR    0                180U   EPI MORI 2 1.052298e+00
      329  2012 FLA KEYS  MCPR    0                258U   EPI MORI 2 0.000000e+00
      330  2012 FLA KEYS  MCPR    0                259U   EPI MORI 2 1.084180e-03
      331  2012 FLA KEYS  MCPR    0                260U   EPI MORI 2 0.000000e+00
      332  2012 FLA KEYS  MCPR    0                261U   EPI MORI 2 0.000000e+00
      333  2012 FLA KEYS  MCPR    0                262U   EPI MORI 2 2.032670e-01
      334  2012 FLA KEYS  MCPR    0                263U   EPI MORI 2 2.566417e-02
      335  2012 FLA KEYS  MCPR    0                264U   EPI MORI 2 2.322774e-01
      336  2012 FLA KEYS  MCPR    0                265U   EPI MORI 2 0.000000e+00
      337  2012 FLA KEYS  MCPR    0                266U   EPI MORI 2 0.000000e+00
      338  2012 FLA KEYS  MCPR    0                267U   EPI MORI 2 0.000000e+00
      339  2012 FLA KEYS  MCPR    0                268U   EPI MORI 2 0.000000e+00
      340  2012 FLA KEYS  MCPR    0                269U   EPI MORI 2 0.000000e+00
      341  2012 FLA KEYS  MCPR    0                270U   EPI MORI 2 0.000000e+00
      342  2012 FLA KEYS  MCPR    0                271U   EPI MORI 2 1.734574e-02
      343  2012 FLA KEYS  MCPR    0                272U   EPI MORI 2 0.000000e+00
      344  2012 FLA KEYS  MCPR    0                273U   EPI MORI 2 0.000000e+00
      345  2012 FLA KEYS  MCPR    0                274U   EPI MORI 2 0.000000e+00
      346  2012 FLA KEYS  MCPR    0                275U   EPI MORI 2 9.944227e-02
      347  2012 FLA KEYS  MCPR    0                276U   EPI MORI 2 0.000000e+00
      348  2012 FLA KEYS  MCPR    0                277U   EPI MORI 2 0.000000e+00
      349  2012 FLA KEYS  MCPR    0                279U   EPI MORI 2 0.000000e+00
      350  2012 FLA KEYS  MCPR    0                280U   EPI MORI 2 0.000000e+00
      351  2012 FLA KEYS  MCPR    0                281U   EPI MORI 2 0.000000e+00
      352  2012 FLA KEYS  MCPR    0                282U   EPI MORI 2 0.000000e+00
      353  2012 FLA KEYS  MCPR    0                414U   EPI MORI 2 0.000000e+00
      354  2012 FLA KEYS  MCPR    0                834U   EPI MORI 2 1.836207e-02
      355  2012 FLA KEYS  MCPR    0                876U   EPI MORI 2 0.000000e+00
      356  2012 FLA KEYS  MCPR    0                917U   EPI MORI 2 2.461453e+00
      357  2012 FLA KEYS  MCPR    0                974U   EPI MORI 2 0.000000e+00
      358  2012 FLA KEYS  MCPR    0                980U   EPI MORI 2 0.000000e+00
      359  2012 FLA KEYS  MCPR    1                331U   EPI MORI 2 1.420073e+00
      360  2012 FLA KEYS  MCPR    1                332U   EPI MORI 2 8.130680e-01
      361  2012 FLA KEYS  MCPR    1                333U   EPI MORI 2 0.000000e+00
      362  2012 FLA KEYS  MCPR    1                334U   EPI MORI 2 9.163906e-01
      363  2012 FLA KEYS  MCPR    1                372U   EPI MORI 2 0.000000e+00
      364  2012 FLA KEYS  MCPR    1                373U   EPI MORI 2 0.000000e+00
      365  2012 FLA KEYS  MCPR    1                374U   EPI MORI 2 5.252334e-01
      366  2012 FLA KEYS  MCPR    1                375U   EPI MORI 2 0.000000e+00
      367  2012 FLA KEYS  MCPR    1                376U   EPI MORI 2 1.734574e-02
      368  2012 FLA KEYS  MCPR    1                377U   EPI MORI 2 9.944227e-02
      369  2012 FLA KEYS  MCPR    1                378U   EPI MORI 2 0.000000e+00
      370  2012 FLA KEYS  MCPR    1                379U   EPI MORI 2 1.734574e-02
      371  2012 FLA KEYS  OFPR    0                027U   EPI MORI 2 2.566417e-02
      372  2012 FLA KEYS  OFPR    0                029U   EPI MORI 2 2.261431e-01
      373  2012 FLA KEYS  OFPR    0                030U   EPI MORI 2 0.000000e+00
      374  2012 FLA KEYS  OFPR    0                031U   EPI MORI 2 0.000000e+00
      375  2012 FLA KEYS  OFPR    0                033U   EPI MORI 2 0.000000e+00
      376  2012 FLA KEYS  OFPR    0                034U   EPI MORI 2 1.155221e-01
      377  2012 FLA KEYS  OFPR    0                181U   EPI MORI 2 4.421401e-02
      378  2012 FLA KEYS  OFPR    0                182U   EPI MORI 2 6.346025e-01
      379  2012 FLA KEYS  OFPR    0                185U   EPI MORI 2 1.467012e+00
      380  2012 FLA KEYS  OFPR    0                186U   EPI MORI 2 0.000000e+00
      381  2012 FLA KEYS  OFPR    0                187U   EPI MORI 2 1.542483e-01
      382  2012 FLA KEYS  OFPR    0                188U   EPI MORI 2 5.013904e-01
      383  2012 FLA KEYS  OFPR    0                189U   EPI MORI 2 2.522978e-01
      384  2012 FLA KEYS  OFPR    0                190U   EPI MORI 2 0.000000e+00
      385  2012 FLA KEYS  OFPR    0                191U   EPI MORI 2 2.154153e+00
      386  2012 FLA KEYS  OFPR    0                192U   EPI MORI 2 1.337181e-01
      387  2012 FLA KEYS  OFPR    0                193U   EPI MORI 2 7.283721e-02
      388  2012 FLA KEYS  OFPR    0                194U   EPI MORI 2 1.165319e+00
      389  2012 FLA KEYS  OFPR    0                195U   EPI MORI 2 0.000000e+00
      390  2012 FLA KEYS  OFPR    0                197U   EPI MORI 2 0.000000e+00
      391  2012 FLA KEYS  OFPR    0                199U   EPI MORI 2 2.116569e-02
      392  2012 FLA KEYS  OFPR    0                200U   EPI MORI 2 0.000000e+00
      393  2012 FLA KEYS  OFPR    0                201U   EPI MORI 2 4.865339e-03
      394  2012 FLA KEYS  OFPR    0                283U   EPI MORI 2 0.000000e+00
      395  2012 FLA KEYS  OFPR    0                284U   EPI MORI 2 0.000000e+00
      396  2012 FLA KEYS  OFPR    0                285U   EPI MORI 2 3.228668e-02
      397  2012 FLA KEYS  OFPR    0                286U   EPI MORI 2 0.000000e+00
      398  2012 FLA KEYS  OFPR    0                879U   EPI MORI 2 0.000000e+00
      399  2012 FLA KEYS  OFPR    0                925U   EPI MORI 2 2.913489e-01
      400  2012 FLA KEYS  OFPR    1                336U   EPI MORI 2 0.000000e+00
      401  2012 FLA KEYS  OFPR    1                337U   EPI MORI 2 1.337181e-01
      402  2012 FLA KEYS  OFPR    1                338U   EPI MORI 2 0.000000e+00
      403  2012 FLA KEYS  OFPR    1                339U   EPI MORI 2 0.000000e+00
      404  2012 FLA KEYS  OFPR    1                340U   EPI MORI 2 0.000000e+00
      405  2012 FLA KEYS  OFPR    1                408U   EPI MORI 2 0.000000e+00
      406  2012 FLA KEYS  OFPR    1                409U   EPI MORI 2 9.537889e-01
      407  2012 FLA KEYS  OFPR    1                410U   EPI MORI 2 3.135971e-04
      408  2012 FLA KEYS  OFPR    1                411U   EPI MORI 2 2.841885e-02
      409  2012 FLA KEYS  OFPR    1                412U   EPI MORI 2 0.000000e+00
      410  2012 FLA KEYS  OFPR    1                413U   EPI MORI 2 0.000000e+00
      411  2012 FLA KEYS  OFPR    1                415U   EPI MORI 2 0.000000e+00
      412  2012 FLA KEYS  OFPR    1                416U   EPI MORI 2 0.000000e+00
      413  2012 FLA KEYS  OFPR    1                417U   EPI MORI 2 0.000000e+00
      414  2012 FLA KEYS  OFPR    1                418U   EPI MORI 1           NA
      415  2012 FLA KEYS  OFPR    1                835U   EPI MORI 2 0.000000e+00
      416  2012 FLA KEYS  OFPR    1                A24U   EPI MORI 2 0.000000e+00
      417  2022 FLA KEYS   S01    0                1004   EPI MORI 1           NA
      418  2022 FLA KEYS   S01    0                1005   EPI MORI 1           NA
      419  2022 FLA KEYS   S01    0                1006   EPI MORI 1           NA
      420  2022 FLA KEYS   S01    0                1010   EPI MORI 1           NA
      421  2022 FLA KEYS   S01    0                1011   EPI MORI 1           NA
      422  2022 FLA KEYS   S01    0                1365   EPI MORI 1           NA
      423  2022 FLA KEYS   S01    0                1490   EPI MORI 1           NA
      424  2022 FLA KEYS   S01    0                1491   EPI MORI 1           NA
      425  2022 FLA KEYS   S01    0                1963   EPI MORI 1           NA
      426  2022 FLA KEYS   S01    0                8069   EPI MORI 1           NA
      427  2022 FLA KEYS   S01    0                8071   EPI MORI 1           NA
      428  2022 FLA KEYS   S01    1                1002   EPI MORI 1           NA
      429  2022 FLA KEYS   S01    1                1367   EPI MORI 1           NA
      430  2022 FLA KEYS   S01    1                1368   EPI MORI 1           NA
      431  2022 FLA KEYS   S01    1                1369   EPI MORI 1           NA
      432  2022 FLA KEYS   S01    1                1849   EPI MORI 1           NA
      433  2022 FLA KEYS   S01    1                9003   EPI MORI 1           NA
      434  2022 FLA KEYS   S02    0                1015   EPI MORI 1           NA
      435  2022 FLA KEYS   S02    0                1018   EPI MORI 1           NA
      436  2022 FLA KEYS   S02    0                1023   EPI MORI 1           NA
      437  2022 FLA KEYS   S02    0                1025   EPI MORI 1           NA
      438  2022 FLA KEYS   S02    0                1039   EPI MORI 1           NA
      439  2022 FLA KEYS   S02    0                1072   EPI MORI 1           NA
      440  2022 FLA KEYS   S02    0                1077   EPI MORI 1           NA
      441  2022 FLA KEYS   S02    0                1497   EPI MORI 1           NA
      442  2022 FLA KEYS   S02    0                1514   EPI MORI 1           NA
      443  2022 FLA KEYS   S02    0                1556   EPI MORI 1           NA
      444  2022 FLA KEYS   S02    0                1962   EPI MORI 1           NA
      445  2022 FLA KEYS   S02    0                1966   EPI MORI 1           NA
      446  2022 FLA KEYS   S02    0                1968   EPI MORI 1           NA
      447  2022 FLA KEYS   S02    0                1970   EPI MORI 1           NA
      448  2022 FLA KEYS   S02    0                1972   EPI MORI 1           NA
      449  2022 FLA KEYS   S02    0                1974   EPI MORI 1           NA
      450  2022 FLA KEYS   S02    0                1978   EPI MORI 1           NA
      451  2022 FLA KEYS   S02    1                1372   EPI MORI 1           NA
      452  2022 FLA KEYS   S02    1                1373   EPI MORI 1           NA
      453  2022 FLA KEYS   S02    1                1374   EPI MORI 1           NA
      454  2022 FLA KEYS   S02    1                1857   EPI MORI 1           NA
      455  2022 FLA KEYS   S03    0                1016   EPI MORI 1           NA
      456  2022 FLA KEYS   S03    0                1020   EPI MORI 1           NA
      457  2022 FLA KEYS   S03    0                1022   EPI MORI 1           NA
      458  2022 FLA KEYS   S03    0                1026   EPI MORI 1           NA
      459  2022 FLA KEYS   S03    0                1027   EPI MORI 1           NA
      460  2022 FLA KEYS   S03    0                1028   EPI MORI 1           NA
      461  2022 FLA KEYS   S03    0                1030   EPI MORI 1           NA
      462  2022 FLA KEYS   S03    0                1031   EPI MORI 1           NA
      463  2022 FLA KEYS   S03    0                1032   EPI MORI 1           NA
      464  2022 FLA KEYS   S03    0                1033   EPI MORI 1           NA
      465  2022 FLA KEYS   S03    0                1035   EPI MORI 1           NA
      466  2022 FLA KEYS   S03    0                1037   EPI MORI 1           NA
      467  2022 FLA KEYS   S03    0                1038   EPI MORI 1           NA
      468  2022 FLA KEYS   S03    0                1042   EPI MORI 1           NA
      469  2022 FLA KEYS   S03    0                1043   EPI MORI 1           NA
      470  2022 FLA KEYS   S03    0                1044   EPI MORI 1           NA
      471  2022 FLA KEYS   S03    0                1045   EPI MORI 1           NA
      472  2022 FLA KEYS   S03    0                1046   EPI MORI 1           NA
      473  2022 FLA KEYS   S03    0                1048   EPI MORI 1           NA
      474  2022 FLA KEYS   S03    0                1050   EPI MORI 1           NA
      475  2022 FLA KEYS   S03    0                1052   EPI MORI 1           NA
      476  2022 FLA KEYS   S03    0                1053   EPI MORI 1           NA
      477  2022 FLA KEYS   S03    0                1055   EPI MORI 1           NA
      478  2022 FLA KEYS   S03    0                1056   EPI MORI 1           NA
      479  2022 FLA KEYS   S03    0                1058   EPI MORI 1           NA
      480  2022 FLA KEYS   S03    0                1060   EPI MORI 1           NA
      481  2022 FLA KEYS   S03    0                1063   EPI MORI 1           NA
      482  2022 FLA KEYS   S03    0                1066   EPI MORI 1           NA
      483  2022 FLA KEYS   S03    0                1068   EPI MORI 1           NA
      484  2022 FLA KEYS   S03    0                1069   EPI MORI 1           NA
      485  2022 FLA KEYS   S03    0                1073   EPI MORI 1           NA
      486  2022 FLA KEYS   S03    0                1075   EPI MORI 1           NA
      487  2022 FLA KEYS   S03    0                1078   EPI MORI 1           NA
      488  2022 FLA KEYS   S03    0                1081   EPI MORI 1           NA
      489  2022 FLA KEYS   S03    0                1083   EPI MORI 1           NA
      490  2022 FLA KEYS   S03    0                1504   EPI MORI 1           NA
      491  2022 FLA KEYS   S03    0                1505   EPI MORI 1           NA
      492  2022 FLA KEYS   S03    0                1507   EPI MORI 1           NA
      493  2022 FLA KEYS   S03    0                1508   EPI MORI 1           NA
      494  2022 FLA KEYS   S03    0                1509   EPI MORI 1           NA
      495  2022 FLA KEYS   S03    0                1511   EPI MORI 1           NA
      496  2022 FLA KEYS   S03    0                1512   EPI MORI 1           NA
      497  2022 FLA KEYS   S03    0                1513   EPI MORI 1           NA
      498  2022 FLA KEYS   S03    0                1515   EPI MORI 1           NA
      499  2022 FLA KEYS   S03    0                1520   EPI MORI 1           NA
      500  2022 FLA KEYS   S03    0                1521   EPI MORI 1           NA
      501  2022 FLA KEYS   S03    0                1523   EPI MORI 1           NA
      502  2022 FLA KEYS   S03    0                1524   EPI MORI 1           NA
      503  2022 FLA KEYS   S03    0                1525   EPI MORI 1           NA
      504  2022 FLA KEYS   S03    0                1538   EPI MORI 1           NA
      505  2022 FLA KEYS   S03    0                1541   EPI MORI 1           NA
      506  2022 FLA KEYS   S03    0                1544   EPI MORI 1           NA
      507  2022 FLA KEYS   S03    0                1546   EPI MORI 1           NA
      508  2022 FLA KEYS   S03    0                1558   EPI MORI 1           NA
      509  2022 FLA KEYS   S03    0                1561   EPI MORI 1           NA
      510  2022 FLA KEYS   S03    0                1563   EPI MORI 1           NA
      511  2022 FLA KEYS   S03    0                1964   EPI MORI 1           NA
      512  2022 FLA KEYS   S03    0                1965   EPI MORI 1           NA
      513  2022 FLA KEYS   S03    0                1969   EPI MORI 1           NA
      514  2022 FLA KEYS   S03    0                1971   EPI MORI 1           NA
      515  2022 FLA KEYS   S03    0                1973   EPI MORI 1           NA
      516  2022 FLA KEYS   S03    0                1975   EPI MORI 1           NA
      517  2022 FLA KEYS   S03    0                1976   EPI MORI 1           NA
      518  2022 FLA KEYS   S03    0                1977   EPI MORI 1           NA
      519  2022 FLA KEYS   S03    0                1979   EPI MORI 1           NA
      520  2022 FLA KEYS   S03    0                1980   EPI MORI 1           NA
      521  2022 FLA KEYS   S03    0                1981   EPI MORI 1           NA
      522  2022 FLA KEYS   S03    0                1982   EPI MORI 1           NA
      523  2022 FLA KEYS   S03    0                1983   EPI MORI 1           NA
      524  2022 FLA KEYS   S03    0                1984   EPI MORI 1           NA
      525  2022 FLA KEYS   S03    0                1985   EPI MORI 1           NA
      526  2022 FLA KEYS   S03    0                1986   EPI MORI 1           NA
      527  2022 FLA KEYS   S03    0                1987   EPI MORI 1           NA
      528  2022 FLA KEYS   S03    0                1988   EPI MORI 1           NA
      529  2022 FLA KEYS   S03    0                9018   EPI MORI 1           NA
      530  2022 FLA KEYS   S03    0                9020   EPI MORI 1           NA
      531  2022 FLA KEYS   S03    0                9210   EPI MORI 1           NA
      532  2022 FLA KEYS   S03    0                9211   EPI MORI 1           NA
      533  2022 FLA KEYS   S03    0                9215   EPI MORI 1           NA
      534  2022 FLA KEYS   S03    1                1371   EPI MORI 1           NA
      535  2022 FLA KEYS   S03    1                1375   EPI MORI 1           NA
      536  2022 FLA KEYS   S03    1                1376   EPI MORI 1           NA
      537  2022 FLA KEYS   S03    1                1379   EPI MORI 1           NA
      538  2022 FLA KEYS   S03    1                1380   EPI MORI 1           NA
      539  2022 FLA KEYS   S03    1                1381   EPI MORI 1           NA
      540  2022 FLA KEYS   S03    1                1382   EPI MORI 1           NA
      541  2022 FLA KEYS   S03    1                1861   EPI MORI 1           NA
      542  2022 FLA KEYS   S03    1                9013   EPI MORI 1           NA
      543  2022 FLA KEYS   S03    1                9112   EPI MORI 1           NA
      544  2022 FLA KEYS   S03    1                9119   EPI MORI 1           NA
      545  2022 FLA KEYS   S03    1                9122   EPI MORI 1           NA
      546  2022 FLA KEYS   S05    0                1024   EPI MORI 1           NA
      547  2022 FLA KEYS   S05    0                1047   EPI MORI 1           NA
      548  2022 FLA KEYS   S05    0                1084   EPI MORI 1           NA
      549  2022 FLA KEYS   S05    0                1086   EPI MORI 1           NA
      550  2022 FLA KEYS   S05    0                1087   EPI MORI 1           NA
      551  2022 FLA KEYS   S05    0                1089   EPI MORI 1           NA
      552  2022 FLA KEYS   S05    0                1095   EPI MORI 1           NA
      553  2022 FLA KEYS   S05    0                1101   EPI MORI 1           NA
      554  2022 FLA KEYS   S05    0                1105   EPI MORI 1           NA
      555  2022 FLA KEYS   S05    0                1113   EPI MORI 1           NA
      556  2022 FLA KEYS   S05    0                1114   EPI MORI 1           NA
      557  2022 FLA KEYS   S05    0                1115   EPI MORI 1           NA
      558  2022 FLA KEYS   S05    0                1118   EPI MORI 1           NA
      559  2022 FLA KEYS   S05    0                1119   EPI MORI 1           NA
      560  2022 FLA KEYS   S05    0                1122   EPI MORI 1           NA
      561  2022 FLA KEYS   S05    0                1123   EPI MORI 1           NA
      562  2022 FLA KEYS   S05    0                1124   EPI MORI 1           NA
      563  2022 FLA KEYS   S05    0                1125   EPI MORI 1           NA
      564  2022 FLA KEYS   S05    0                1126   EPI MORI 1           NA
      565  2022 FLA KEYS   S05    0                1127   EPI MORI 1           NA
      566  2022 FLA KEYS   S05    0                1133   EPI MORI 1           NA
      567  2022 FLA KEYS   S05    0                1134   EPI MORI 1           NA
      568  2022 FLA KEYS   S05    0                1135   EPI MORI 1           NA
      569  2022 FLA KEYS   S05    0                1137   EPI MORI 1           NA
      570  2022 FLA KEYS   S05    0                1140   EPI MORI 1           NA
      571  2022 FLA KEYS   S05    0                1141   EPI MORI 1           NA
      572  2022 FLA KEYS   S05    0                1142   EPI MORI 1           NA
      573  2022 FLA KEYS   S05    0                1146   EPI MORI 1           NA
      574  2022 FLA KEYS   S05    0                1147   EPI MORI 1           NA
      575  2022 FLA KEYS   S05    0                1148   EPI MORI 1           NA
      576  2022 FLA KEYS   S05    0                1151   EPI MORI 1           NA
      577  2022 FLA KEYS   S05    0                1155   EPI MORI 1           NA
      578  2022 FLA KEYS   S05    0                1158   EPI MORI 1           NA
      579  2022 FLA KEYS   S05    0                1162   EPI MORI 1           NA
      580  2022 FLA KEYS   S05    0                1163   EPI MORI 1           NA
      581  2022 FLA KEYS   S05    0                1165   EPI MORI 1           NA
      582  2022 FLA KEYS   S05    0                1166   EPI MORI 1           NA
      583  2022 FLA KEYS   S05    0                1168   EPI MORI 1           NA
      584  2022 FLA KEYS   S05    0                1170   EPI MORI 1           NA
      585  2022 FLA KEYS   S05    0                1178   EPI MORI 1           NA
      586  2022 FLA KEYS   S05    0                1179   EPI MORI 1           NA
      587  2022 FLA KEYS   S05    0                1180   EPI MORI 1           NA
      588  2022 FLA KEYS   S05    0                1181   EPI MORI 1           NA
      589  2022 FLA KEYS   S05    0                1182   EPI MORI 1           NA
      590  2022 FLA KEYS   S05    0                1192   EPI MORI 1           NA
      591  2022 FLA KEYS   S05    0                1197   EPI MORI 1           NA
      592  2022 FLA KEYS   S05    0                1198   EPI MORI 1           NA
      593  2022 FLA KEYS   S05    0                1204   EPI MORI 1           NA
      594  2022 FLA KEYS   S05    0                1253   EPI MORI 1           NA
      595  2022 FLA KEYS   S05    0                1266   EPI MORI 1           NA
      596  2022 FLA KEYS   S05    0                1281   EPI MORI 1           NA
      597  2022 FLA KEYS   S05    0                1406   EPI MORI 1           NA
      598  2022 FLA KEYS   S05    0                1564   EPI MORI 1           NA
      599  2022 FLA KEYS   S05    0                1575   EPI MORI 1           NA
      600  2022 FLA KEYS   S05    0                1582   EPI MORI 1           NA
      601  2022 FLA KEYS   S05    0                1592   EPI MORI 1           NA
      602  2022 FLA KEYS   S05    0                1598   EPI MORI 1           NA
      603  2022 FLA KEYS   S05    0                1611   EPI MORI 1           NA
      604  2022 FLA KEYS   S05    0                1622   EPI MORI 1           NA
      605  2022 FLA KEYS   S05    0                1623   EPI MORI 1           NA
      606  2022 FLA KEYS   S05    0                1635   EPI MORI 1           NA
      607  2022 FLA KEYS   S05    0                1636   EPI MORI 1           NA
      608  2022 FLA KEYS   S05    0                1637   EPI MORI 1           NA
      609  2022 FLA KEYS   S05    0                1641   EPI MORI 1           NA
      610  2022 FLA KEYS   S05    0                1642   EPI MORI 1           NA
      611  2022 FLA KEYS   S05    0                1665   EPI MORI 1           NA
      612  2022 FLA KEYS   S05    0                1992   EPI MORI 1           NA
      613  2022 FLA KEYS   S05    0                1993   EPI MORI 1           NA
      614  2022 FLA KEYS   S05    0                1994   EPI MORI 1           NA
      615  2022 FLA KEYS   S05    0                1995   EPI MORI 1           NA
      616  2022 FLA KEYS   S05    0                1997   EPI MORI 1           NA
      617  2022 FLA KEYS   S05    0                1998   EPI MORI 1           NA
      618  2022 FLA KEYS   S05    0                1999   EPI MORI 1           NA
      619  2022 FLA KEYS   S05    0                8001   EPI MORI 1           NA
      620  2022 FLA KEYS   S05    0                8002   EPI MORI 1           NA
      621  2022 FLA KEYS   S05    0                8003   EPI MORI 1           NA
      622  2022 FLA KEYS   S05    0                8004   EPI MORI 1           NA
      623  2022 FLA KEYS   S05    0                8005   EPI MORI 1           NA
      624  2022 FLA KEYS   S05    0                8011   EPI MORI 1           NA
      625  2022 FLA KEYS   S05    0                8017   EPI MORI 1           NA
      626  2022 FLA KEYS   S05    0                8019   EPI MORI 1           NA
      627  2022 FLA KEYS   S05    1                1384   EPI MORI 1           NA
      628  2022 FLA KEYS   S05    1                1385   EPI MORI 1           NA
      629  2022 FLA KEYS   S05    1                1386   EPI MORI 1           NA
      630  2022 FLA KEYS   S05    1                1391   EPI MORI 1           NA
      631  2022 FLA KEYS   S05    1                1393   EPI MORI 1           NA
      632  2022 FLA KEYS   S05    1                1394   EPI MORI 1           NA
      633  2022 FLA KEYS   S05    1                1396   EPI MORI 1           NA
      634  2022 FLA KEYS   S05    1                1398   EPI MORI 1           NA
      635  2022 FLA KEYS   S05    1                1399   EPI MORI 1           NA
      636  2022 FLA KEYS   S05    1                1400   EPI MORI 1           NA
      637  2022 FLA KEYS   S05    1                1401   EPI MORI 1           NA
      638  2022 FLA KEYS   S05    1                1404   EPI MORI 1           NA
      639  2022 FLA KEYS   S05    1                1407   EPI MORI 1           NA
      640  2022 FLA KEYS   S05    1                1408   EPI MORI 1           NA
      641  2022 FLA KEYS   S05    1                1411   EPI MORI 1           NA
      642  2022 FLA KEYS   S05    1                1412   EPI MORI 1           NA
      643  2022 FLA KEYS   S05    1                1428   EPI MORI 1           NA
      644  2022 FLA KEYS   S05    1                1429   EPI MORI 1           NA
      645  2022 FLA KEYS   S05    1                1431   EPI MORI 1           NA
      646  2022 FLA KEYS   S05    1                1432   EPI MORI 1           NA
      647  2022 FLA KEYS   S05    1                1435   EPI MORI 1           NA
      648  2022 FLA KEYS   S05    1                1438   EPI MORI 1           NA
      649  2022 FLA KEYS   S05    1                1449   EPI MORI 1           NA
      650  2022 FLA KEYS   S05    1                1450   EPI MORI 1           NA
      651  2022 FLA KEYS   S05    1                1454   EPI MORI 1           NA
      652  2022 FLA KEYS   S05    1                1464   EPI MORI 1           NA
      653  2022 FLA KEYS   S05    1                1913   EPI MORI 1           NA
      654  2022 FLA KEYS   S05    1                9063   EPI MORI 1           NA
      655  2022 FLA KEYS   S05    1                9068   EPI MORI 1           NA
      656  2022 FLA KEYS   S05    1                9069   EPI MORI 1           NA
      657  2022 FLA KEYS   S05    1                9194   EPI MORI 1           NA
      658  2022 FLA KEYS   S05    1                9255   EPI MORI 1           NA
      659  2022 FLA KEYS   S06    0                1036   EPI MORI 1           NA
      660  2022 FLA KEYS   S06    0                1090   EPI MORI 1           NA
      661  2022 FLA KEYS   S06    0                1097   EPI MORI 1           NA
      662  2022 FLA KEYS   S06    0                1102   EPI MORI 1           NA
      663  2022 FLA KEYS   S06    0                1107   EPI MORI 1           NA
      664  2022 FLA KEYS   S06    0                1110   EPI MORI 1           NA
      665  2022 FLA KEYS   S06    0                1117   EPI MORI 1           NA
      666  2022 FLA KEYS   S06    0                1120   EPI MORI 1           NA
      667  2022 FLA KEYS   S06    0                1121   EPI MORI 1           NA
      668  2022 FLA KEYS   S06    0                1128   EPI MORI 1           NA
      669  2022 FLA KEYS   S06    0                1129   EPI MORI 1           NA
      670  2022 FLA KEYS   S06    0                1130   EPI MORI 1           NA
      671  2022 FLA KEYS   S06    0                1131   EPI MORI 1           NA
      672  2022 FLA KEYS   S06    0                1132   EPI MORI 1           NA
      673  2022 FLA KEYS   S06    0                1136   EPI MORI 1           NA
      674  2022 FLA KEYS   S06    0                1138   EPI MORI 1           NA
      675  2022 FLA KEYS   S06    0                1144   EPI MORI 1           NA
      676  2022 FLA KEYS   S06    0                1149   EPI MORI 1           NA
      677  2022 FLA KEYS   S06    0                1150   EPI MORI 1           NA
      678  2022 FLA KEYS   S06    0                1152   EPI MORI 1           NA
      679  2022 FLA KEYS   S06    0                1153   EPI MORI 1           NA
      680  2022 FLA KEYS   S06    0                1154   EPI MORI 1           NA
      681  2022 FLA KEYS   S06    0                1159   EPI MORI 1           NA
      682  2022 FLA KEYS   S06    0                1161   EPI MORI 1           NA
      683  2022 FLA KEYS   S06    0                1169   EPI MORI 1           NA
      684  2022 FLA KEYS   S06    0                1172   EPI MORI 1           NA
      685  2022 FLA KEYS   S06    0                1173   EPI MORI 1           NA
      686  2022 FLA KEYS   S06    0                1174   EPI MORI 1           NA
      687  2022 FLA KEYS   S06    0                1177   EPI MORI 1           NA
      688  2022 FLA KEYS   S06    0                1183   EPI MORI 1           NA
      689  2022 FLA KEYS   S06    0                1184   EPI MORI 1           NA
      690  2022 FLA KEYS   S06    0                1185   EPI MORI 1           NA
      691  2022 FLA KEYS   S06    0                1186   EPI MORI 1           NA
      692  2022 FLA KEYS   S06    0                1187   EPI MORI 1           NA
      693  2022 FLA KEYS   S06    0                1188   EPI MORI 1           NA
      694  2022 FLA KEYS   S06    0                1189   EPI MORI 1           NA
      695  2022 FLA KEYS   S06    0                1190   EPI MORI 1           NA
      696  2022 FLA KEYS   S06    0                1191   EPI MORI 1           NA
      697  2022 FLA KEYS   S06    0                1193   EPI MORI 1           NA
      698  2022 FLA KEYS   S06    0                1195   EPI MORI 1           NA
      699  2022 FLA KEYS   S06    0                1196   EPI MORI 1           NA
      700  2022 FLA KEYS   S06    0                1203   EPI MORI 1           NA
      701  2022 FLA KEYS   S06    0                1205   EPI MORI 1           NA
      702  2022 FLA KEYS   S06    0                1206   EPI MORI 1           NA
      703  2022 FLA KEYS   S06    0                1213   EPI MORI 1           NA
      704  2022 FLA KEYS   S06    0                1231   EPI MORI 1           NA
      705  2022 FLA KEYS   S06    0                1240   EPI MORI 1           NA
      706  2022 FLA KEYS   S06    0                1247   EPI MORI 1           NA
      707  2022 FLA KEYS   S06    0                1272   EPI MORI 1           NA
      708  2022 FLA KEYS   S06    0                1280   EPI MORI 1           NA
      709  2022 FLA KEYS   S06    0                1492   EPI MORI 1           NA
      710  2022 FLA KEYS   S06    0                1577   EPI MORI 1           NA
      711  2022 FLA KEYS   S06    0                1639   EPI MORI 1           NA
      712  2022 FLA KEYS   S06    0                1640   EPI MORI 1           NA
      713  2022 FLA KEYS   S06    0                1643   EPI MORI 1           NA
      714  2022 FLA KEYS   S06    0                1644   EPI MORI 1           NA
      715  2022 FLA KEYS   S06    0                1657   EPI MORI 1           NA
      716  2022 FLA KEYS   S06    0                1664   EPI MORI 1           NA
      717  2022 FLA KEYS   S06    0                1666   EPI MORI 1           NA
      718  2022 FLA KEYS   S06    0                1667   EPI MORI 1           NA
      719  2022 FLA KEYS   S06    0                1668   EPI MORI 1           NA
      720  2022 FLA KEYS   S06    0                1669   EPI MORI 1           NA
      721  2022 FLA KEYS   S06    0                1670   EPI MORI 1           NA
      722  2022 FLA KEYS   S06    0                1673   EPI MORI 1           NA
      723  2022 FLA KEYS   S06    0                1680   EPI MORI 1           NA
      724  2022 FLA KEYS   S06    0                1682   EPI MORI 1           NA
      725  2022 FLA KEYS   S06    0                1731   EPI MORI 1           NA
      726  2022 FLA KEYS   S06    0                1959   EPI MORI 1           NA
      727  2022 FLA KEYS   S06    0                1967   EPI MORI 1           NA
      728  2022 FLA KEYS   S06    0                1991   EPI MORI 1           NA
      729  2022 FLA KEYS   S06    0                1996   EPI MORI 1           NA
      730  2022 FLA KEYS   S06    0                8006   EPI MORI 1           NA
      731  2022 FLA KEYS   S06    0                8007   EPI MORI 1           NA
      732  2022 FLA KEYS   S06    0                8008   EPI MORI 1           NA
      733  2022 FLA KEYS   S06    0                8009   EPI MORI 1           NA
      734  2022 FLA KEYS   S06    0                8010   EPI MORI 1           NA
      735  2022 FLA KEYS   S06    0                8012   EPI MORI 1           NA
      736  2022 FLA KEYS   S06    0                8013   EPI MORI 1           NA
      737  2022 FLA KEYS   S06    0                8014   EPI MORI 1           NA
      738  2022 FLA KEYS   S06    0                8015   EPI MORI 1           NA
      739  2022 FLA KEYS   S06    0                8016   EPI MORI 1           NA
      740  2022 FLA KEYS   S06    0                8018   EPI MORI 1           NA
      741  2022 FLA KEYS   S06    0                8032   EPI MORI 1           NA
      742  2022 FLA KEYS   S06    0                8065   EPI MORI 1           NA
      743  2022 FLA KEYS   S06    0                8067   EPI MORI 1           NA
      744  2022 FLA KEYS   S06    0                9029   EPI MORI 1           NA
      745  2022 FLA KEYS   S06    0                9030   EPI MORI 1           NA
      746  2022 FLA KEYS   S06    0                9097   EPI MORI 1           NA
      747  2022 FLA KEYS   S06    0                9298   EPI MORI 1           NA
      748  2022 FLA KEYS   S06    1                1092   EPI MORI 1           NA
      749  2022 FLA KEYS   S06    1                1383   EPI MORI 1           NA
      750  2022 FLA KEYS   S06    1                1387   EPI MORI 1           NA
      751  2022 FLA KEYS   S06    1                1388   EPI MORI 1           NA
      752  2022 FLA KEYS   S06    1                1389   EPI MORI 1           NA
      753  2022 FLA KEYS   S06    1                1390   EPI MORI 1           NA
      754  2022 FLA KEYS   S06    1                1392   EPI MORI 1           NA
      755  2022 FLA KEYS   S06    1                1395   EPI MORI 1           NA
      756  2022 FLA KEYS   S06    1                1397   EPI MORI 1           NA
      757  2022 FLA KEYS   S06    1                1402   EPI MORI 1           NA
      758  2022 FLA KEYS   S06    1                1403   EPI MORI 1           NA
      759  2022 FLA KEYS   S06    1                1405   EPI MORI 1           NA
      760  2022 FLA KEYS   S06    1                1409   EPI MORI 1           NA
      761  2022 FLA KEYS   S06    1                1410   EPI MORI 1           NA
      762  2022 FLA KEYS   S06    1                1413   EPI MORI 1           NA
      763  2022 FLA KEYS   S06    1                1415   EPI MORI 1           NA
      764  2022 FLA KEYS   S06    1                1416   EPI MORI 1           NA
      765  2022 FLA KEYS   S06    1                1417   EPI MORI 1           NA
      766  2022 FLA KEYS   S06    1                1418   EPI MORI 1           NA
      767  2022 FLA KEYS   S06    1                1419   EPI MORI 1           NA
      768  2022 FLA KEYS   S06    1                1420   EPI MORI 1           NA
      769  2022 FLA KEYS   S06    1                1421   EPI MORI 1           NA
      770  2022 FLA KEYS   S06    1                1422   EPI MORI 1           NA
      771  2022 FLA KEYS   S06    1                1423   EPI MORI 1           NA
      772  2022 FLA KEYS   S06    1                1424   EPI MORI 1           NA
      773  2022 FLA KEYS   S06    1                1425   EPI MORI 1           NA
      774  2022 FLA KEYS   S06    1                1426   EPI MORI 1           NA
      775  2022 FLA KEYS   S06    1                1427   EPI MORI 1           NA
      776  2022 FLA KEYS   S06    1                1430   EPI MORI 1           NA
      777  2022 FLA KEYS   S06    1                1434   EPI MORI 1           NA
      778  2022 FLA KEYS   S06    1                1436   EPI MORI 1           NA
      779  2022 FLA KEYS   S06    1                1437   EPI MORI 1           NA
      780  2022 FLA KEYS   S06    1                1439   EPI MORI 1           NA
      781  2022 FLA KEYS   S06    1                1440   EPI MORI 1           NA
      782  2022 FLA KEYS   S06    1                1442   EPI MORI 1           NA
      783  2022 FLA KEYS   S06    1                1443   EPI MORI 1           NA
      784  2022 FLA KEYS   S06    1                1444   EPI MORI 1           NA
      785  2022 FLA KEYS   S06    1                1445   EPI MORI 1           NA
      786  2022 FLA KEYS   S06    1                1447   EPI MORI 1           NA
      787  2022 FLA KEYS   S06    1                1448   EPI MORI 1           NA
      788  2022 FLA KEYS   S06    1                1452   EPI MORI 1           NA
      789  2022 FLA KEYS   S06    1                1459   EPI MORI 1           NA
      790  2022 FLA KEYS   S06    1                1460   EPI MORI 1           NA
      791  2022 FLA KEYS   S06    1                1461   EPI MORI 1           NA
      792  2022 FLA KEYS   S06    1                1463   EPI MORI 1           NA
      793  2022 FLA KEYS   S06    1                1921   EPI MORI 1           NA
      794  2022 FLA KEYS   S06    1                9026   EPI MORI 1           NA
      795  2022 FLA KEYS   S06    1                9027   EPI MORI 1           NA
      796  2022 FLA KEYS   S06    1                9028   EPI MORI 1           NA
      797  2022 FLA KEYS   S06    1                9036   EPI MORI 1           NA
      798  2022 FLA KEYS   S06    1                9037   EPI MORI 1           NA
      799  2022 FLA KEYS   S06    1                9038   EPI MORI 1           NA
      800  2022 FLA KEYS   S06    1                9039   EPI MORI 1           NA
      801  2022 FLA KEYS   S06    1                9040   EPI MORI 1           NA
      802  2022 FLA KEYS   S06    1                9041   EPI MORI 1           NA
      803  2022 FLA KEYS   S06    1                9042   EPI MORI 1           NA
      804  2022 FLA KEYS   S06    1                9043   EPI MORI 1           NA
      805  2022 FLA KEYS   S06    1                9045   EPI MORI 1           NA
      806  2022 FLA KEYS   S06    1                9046   EPI MORI 1           NA
      807  2022 FLA KEYS   S06    1                9047   EPI MORI 1           NA
      808  2022 FLA KEYS   S06    1                9048   EPI MORI 1           NA
      809  2022 FLA KEYS   S06    1                9049   EPI MORI 1           NA
      810  2022 FLA KEYS   S06    1                9050   EPI MORI 1           NA
      811  2022 FLA KEYS   S06    1                9052   EPI MORI 1           NA
      812  2022 FLA KEYS   S06    1                9053   EPI MORI 1           NA
      813  2022 FLA KEYS   S06    1                9054   EPI MORI 1           NA
      814  2022 FLA KEYS   S06    1                9070   EPI MORI 1           NA
      815  2022 FLA KEYS   S06    1                9071   EPI MORI 1           NA
      816  2022 FLA KEYS   S06    1                9072   EPI MORI 1           NA
      817  2022 FLA KEYS   S06    1                9073   EPI MORI 1           NA
      818  2022 FLA KEYS   S06    1                9074   EPI MORI 1           NA
      819  2022 FLA KEYS   S06    1                9076   EPI MORI 1           NA
      820  2022 FLA KEYS   S06    1                9079   EPI MORI 1           NA
      821  2022 FLA KEYS   S06    1                9080   EPI MORI 1           NA
      822  2022 FLA KEYS   S06    1                9081   EPI MORI 1           NA
      823  2022 FLA KEYS   S06    1                9082   EPI MORI 1           NA
      824  2022 FLA KEYS   S06    1                9083   EPI MORI 1           NA
      825  2022 FLA KEYS   S06    1                9084   EPI MORI 1           NA
      826  2022 FLA KEYS   S06    1                9085   EPI MORI 1           NA
      827  2022 FLA KEYS   S06    1                9086   EPI MORI 1           NA
      828  2022 FLA KEYS   S06    1                9087   EPI MORI 1           NA
      829  2022 FLA KEYS   S06    1                9088   EPI MORI 1           NA
      830  2022 FLA KEYS   S06    1                9089   EPI MORI 1           NA
      831  2022 FLA KEYS   S06    1                9090   EPI MORI 1           NA
      832  2022 FLA KEYS   S06    1                9091   EPI MORI 1           NA
      833  2022 FLA KEYS   S06    1                9092   EPI MORI 1           NA
      834  2022 FLA KEYS   S06    1                9093   EPI MORI 1           NA
      835  2022 FLA KEYS   S06    1                9100   EPI MORI 1           NA
      836  2022 FLA KEYS   S06    1                9128   EPI MORI 1           NA
      837  2022 FLA KEYS   S06    1                9138   EPI MORI 1           NA
      838  2022 FLA KEYS   S06    1                9139   EPI MORI 1           NA
      839  2022 FLA KEYS   S06    1                9142   EPI MORI 1           NA
      840  2022 FLA KEYS   S06    1                9144   EPI MORI 1           NA
      841  2022 FLA KEYS   S06    1                9151   EPI MORI 1           NA
      842  2022 FLA KEYS   S06    1                9161   EPI MORI 1           NA
      843  2022 FLA KEYS   S06    1                9166   EPI MORI 1           NA
      844  2022 FLA KEYS   S06    1                9167   EPI MORI 1           NA
      845  2022 FLA KEYS   S06    1                9176   EPI MORI 1           NA
      846  2022 FLA KEYS   S06    1                9180   EPI MORI 1           NA
      847  2022 FLA KEYS   S06    1                9195   EPI MORI 1           NA
      848  2022 FLA KEYS   S06    1                9196   EPI MORI 1           NA
      849  2022 FLA KEYS   S06    1                9199   EPI MORI 1           NA
      850  2022 FLA KEYS   S06    1                9228   EPI MORI 1           NA
      851  2022 FLA KEYS   S06    1                9235   EPI MORI 1           NA
      852  2022 FLA KEYS   S06    1                9256   EPI MORI 1           NA
      853  2022 FLA KEYS   S06    1                9265   EPI MORI 1           NA
      854  2022 FLA KEYS   S08    0                1209   EPI MORI 1           NA
      855  2022 FLA KEYS   S08    0                1211   EPI MORI 1           NA
      856  2022 FLA KEYS   S08    0                1214   EPI MORI 1           NA
      857  2022 FLA KEYS   S08    0                1216   EPI MORI 1           NA
      858  2022 FLA KEYS   S08    0                1217   EPI MORI 1           NA
      859  2022 FLA KEYS   S08    0                1221   EPI MORI 1           NA
      860  2022 FLA KEYS   S08    0                1224   EPI MORI 1           NA
      861  2022 FLA KEYS   S08    0                1227   EPI MORI 1           NA
      862  2022 FLA KEYS   S08    0                1228   EPI MORI 1           NA
      863  2022 FLA KEYS   S08    0                1233   EPI MORI 1           NA
      864  2022 FLA KEYS   S08    0                1252   EPI MORI 1           NA
      865  2022 FLA KEYS   S08    0                1259   EPI MORI 1           NA
      866  2022 FLA KEYS   S08    0                1263   EPI MORI 1           NA
      867  2022 FLA KEYS   S08    0                1268   EPI MORI 1           NA
      868  2022 FLA KEYS   S08    0                1273   EPI MORI 1           NA
      869  2022 FLA KEYS   S08    0                1279   EPI MORI 1           NA
      870  2022 FLA KEYS   S08    0                1294   EPI MORI 1           NA
      871  2022 FLA KEYS   S08    0                1301   EPI MORI 1           NA
      872  2022 FLA KEYS   S08    0                1303   EPI MORI 1           NA
      873  2022 FLA KEYS   S08    0                1305   EPI MORI 1           NA
      874  2022 FLA KEYS   S08    0                1315   EPI MORI 1           NA
      875  2022 FLA KEYS   S08    0                1702   EPI MORI 1           NA
      876  2022 FLA KEYS   S08    0                1781   EPI MORI 1           NA
      877  2022 FLA KEYS   S08    0                8021   EPI MORI 1           NA
      878  2022 FLA KEYS   S08    0                8022   EPI MORI 1           NA
      879  2022 FLA KEYS   S08    0                8023   EPI MORI 1           NA
      880  2022 FLA KEYS   S08    0                8024   EPI MORI 1           NA
      881  2022 FLA KEYS   S08    0                8027   EPI MORI 1           NA
      882  2022 FLA KEYS   S08    0                8028   EPI MORI 1           NA
      883  2022 FLA KEYS   S08    0                8029   EPI MORI 1           NA
      884  2022 FLA KEYS   S08    0                8030   EPI MORI 1           NA
      885  2022 FLA KEYS   S08    0                8033   EPI MORI 1           NA
      886  2022 FLA KEYS   S08    0                8042   EPI MORI 1           NA
      887  2022 FLA KEYS   S08    0                8043   EPI MORI 1           NA
      888  2022 FLA KEYS   S08    1                1455   EPI MORI 1           NA
      889  2022 FLA KEYS   S09    0                1100   EPI MORI 1           NA
      890  2022 FLA KEYS   S09    0                1175   EPI MORI 1           NA
      891  2022 FLA KEYS   S09    0                1208   EPI MORI 1           NA
      892  2022 FLA KEYS   S09    0                1210   EPI MORI 1           NA
      893  2022 FLA KEYS   S09    0                1212   EPI MORI 1           NA
      894  2022 FLA KEYS   S09    0                1215   EPI MORI 1           NA
      895  2022 FLA KEYS   S09    0                1218   EPI MORI 1           NA
      896  2022 FLA KEYS   S09    0                1219   EPI MORI 1           NA
      897  2022 FLA KEYS   S09    0                1220   EPI MORI 1           NA
      898  2022 FLA KEYS   S09    0                1222   EPI MORI 1           NA
      899  2022 FLA KEYS   S09    0                1226   EPI MORI 1           NA
      900  2022 FLA KEYS   S09    0                1229   EPI MORI 1           NA
      901  2022 FLA KEYS   S09    0                1230   EPI MORI 1           NA
      902  2022 FLA KEYS   S09    0                1232   EPI MORI 1           NA
      903  2022 FLA KEYS   S09    0                1234   EPI MORI 1           NA
      904  2022 FLA KEYS   S09    0                1235   EPI MORI 1           NA
      905  2022 FLA KEYS   S09    0                1236   EPI MORI 1           NA
      906  2022 FLA KEYS   S09    0                1237   EPI MORI 1           NA
      907  2022 FLA KEYS   S09    0                1238   EPI MORI 1           NA
      908  2022 FLA KEYS   S09    0                1239   EPI MORI 1           NA
      909  2022 FLA KEYS   S09    0                1241   EPI MORI 1           NA
      910  2022 FLA KEYS   S09    0                1242   EPI MORI 1           NA
      911  2022 FLA KEYS   S09    0                1244   EPI MORI 1           NA
      912  2022 FLA KEYS   S09    0                1245   EPI MORI 1           NA
      913  2022 FLA KEYS   S09    0                1246   EPI MORI 1           NA
      914  2022 FLA KEYS   S09    0                1248   EPI MORI 1           NA
      915  2022 FLA KEYS   S09    0                1249   EPI MORI 1           NA
      916  2022 FLA KEYS   S09    0                1250   EPI MORI 1           NA
      917  2022 FLA KEYS   S09    0                1251   EPI MORI 1           NA
      918  2022 FLA KEYS   S09    0                1256   EPI MORI 1           NA
      919  2022 FLA KEYS   S09    0                1257   EPI MORI 1           NA
      920  2022 FLA KEYS   S09    0                1262   EPI MORI 1           NA
      921  2022 FLA KEYS   S09    0                1265   EPI MORI 1           NA
      922  2022 FLA KEYS   S09    0                1269   EPI MORI 1           NA
      923  2022 FLA KEYS   S09    0                1271   EPI MORI 1           NA
      924  2022 FLA KEYS   S09    0                1275   EPI MORI 1           NA
      925  2022 FLA KEYS   S09    0                1278   EPI MORI 1           NA
      926  2022 FLA KEYS   S09    0                1282   EPI MORI 1           NA
      927  2022 FLA KEYS   S09    0                1283   EPI MORI 1           NA
      928  2022 FLA KEYS   S09    0                1284   EPI MORI 1           NA
      929  2022 FLA KEYS   S09    0                1285   EPI MORI 1           NA
      930  2022 FLA KEYS   S09    0                1286   EPI MORI 1           NA
      931  2022 FLA KEYS   S09    0                1288   EPI MORI 1           NA
      932  2022 FLA KEYS   S09    0                1290   EPI MORI 1           NA
      933  2022 FLA KEYS   S09    0                1291   EPI MORI 1           NA
      934  2022 FLA KEYS   S09    0                1292   EPI MORI 1           NA
      935  2022 FLA KEYS   S09    0                1293   EPI MORI 1           NA
      936  2022 FLA KEYS   S09    0                1295   EPI MORI 1           NA
      937  2022 FLA KEYS   S09    0                1296   EPI MORI 1           NA
      938  2022 FLA KEYS   S09    0                1297   EPI MORI 1           NA
      939  2022 FLA KEYS   S09    0                1298   EPI MORI 1           NA
      940  2022 FLA KEYS   S09    0                1299   EPI MORI 1           NA
      941  2022 FLA KEYS   S09    0                1300   EPI MORI 1           NA
      942  2022 FLA KEYS   S09    0                1302   EPI MORI 1           NA
      943  2022 FLA KEYS   S09    0                1307   EPI MORI 1           NA
      944  2022 FLA KEYS   S09    0                1308   EPI MORI 1           NA
      945  2022 FLA KEYS   S09    0                1309   EPI MORI 1           NA
      946  2022 FLA KEYS   S09    0                1310   EPI MORI 1           NA
      947  2022 FLA KEYS   S09    0                1312   EPI MORI 1           NA
      948  2022 FLA KEYS   S09    0                1313   EPI MORI 1           NA
      949  2022 FLA KEYS   S09    0                1314   EPI MORI 1           NA
      950  2022 FLA KEYS   S09    0                1316   EPI MORI 1           NA
      951  2022 FLA KEYS   S09    0                1323   EPI MORI 1           NA
      952  2022 FLA KEYS   S09    0                1331   EPI MORI 1           NA
      953  2022 FLA KEYS   S09    0                1332   EPI MORI 1           NA
      954  2022 FLA KEYS   S09    0                1346   EPI MORI 1           NA
      955  2022 FLA KEYS   S09    0                1348   EPI MORI 1           NA
      956  2022 FLA KEYS   S09    0                1638   EPI MORI 1           NA
      957  2022 FLA KEYS   S09    0                1696   EPI MORI 1           NA
      958  2022 FLA KEYS   S09    0                1700   EPI MORI 1           NA
      959  2022 FLA KEYS   S09    0                1714   EPI MORI 1           NA
      960  2022 FLA KEYS   S09    0                1716   EPI MORI 1           NA
      961  2022 FLA KEYS   S09    0                1743   EPI MORI 1           NA
      962  2022 FLA KEYS   S09    0                1748   EPI MORI 1           NA
      963  2022 FLA KEYS   S09    0                1751   EPI MORI 1           NA
      964  2022 FLA KEYS   S09    0                1768   EPI MORI 1           NA
      965  2022 FLA KEYS   S09    0                1769   EPI MORI 1           NA
      966  2022 FLA KEYS   S09    0                1770   EPI MORI 1           NA
      967  2022 FLA KEYS   S09    0                1771   EPI MORI 1           NA
      968  2022 FLA KEYS   S09    0                1787   EPI MORI 1           NA
      969  2022 FLA KEYS   S09    0                1842   EPI MORI 1           NA
      970  2022 FLA KEYS   S09    0                1939   EPI MORI 1           NA
      971  2022 FLA KEYS   S09    0                8031   EPI MORI 1           NA
      972  2022 FLA KEYS   S09    0                8034   EPI MORI 1           NA
      973  2022 FLA KEYS   S09    0                8035   EPI MORI 1           NA
      974  2022 FLA KEYS   S09    0                8036   EPI MORI 1           NA
      975  2022 FLA KEYS   S09    0                8037   EPI MORI 1           NA
      976  2022 FLA KEYS   S09    0                8038   EPI MORI 1           NA
      977  2022 FLA KEYS   S09    0                8039   EPI MORI 1           NA
      978  2022 FLA KEYS   S09    0                8040   EPI MORI 1           NA
      979  2022 FLA KEYS   S09    0                8041   EPI MORI 1           NA
      980  2022 FLA KEYS   S09    0                8044   EPI MORI 1           NA
      981  2022 FLA KEYS   S09    0                8045   EPI MORI 1           NA
      982  2022 FLA KEYS   S09    0                8046   EPI MORI 1           NA
      983  2022 FLA KEYS   S09    0                8047   EPI MORI 1           NA
      984  2022 FLA KEYS   S09    0                8048   EPI MORI 1           NA
      985  2022 FLA KEYS   S09    0                8049   EPI MORI 1           NA
      986  2022 FLA KEYS   S09    0                8050   EPI MORI 1           NA
      987  2022 FLA KEYS   S09    0                8054   EPI MORI 1           NA
      988  2022 FLA KEYS   S09    0                8057   EPI MORI 1           NA
      989  2022 FLA KEYS   S09    0                8058   EPI MORI 1           NA
      990  2022 FLA KEYS   S09    0                8076   EPI MORI 1           NA
      991  2022 FLA KEYS   S09    1                1446   EPI MORI 1           NA
      992  2022 FLA KEYS   S09    1                1456   EPI MORI 1           NA
      993  2022 FLA KEYS   S09    1                1457   EPI MORI 1           NA
      994  2022 FLA KEYS   S09    1                1458   EPI MORI 1           NA
      995  2022 FLA KEYS   S09    1                1462   EPI MORI 1           NA
      996  2022 FLA KEYS   S09    1                1465   EPI MORI 1           NA
      997  2022 FLA KEYS   S09    1                1466   EPI MORI 1           NA
      998  2022 FLA KEYS   S09    1                1467   EPI MORI 1           NA
      999  2022 FLA KEYS   S09    1                1468   EPI MORI 1           NA
      1000 2022 FLA KEYS   S09    1                1469   EPI MORI 1           NA
      1001 2022 FLA KEYS   S09    1                1470   EPI MORI 1           NA
      1002 2022 FLA KEYS   S09    1                1471   EPI MORI 1           NA
      1003 2022 FLA KEYS   S09    1                1472   EPI MORI 1           NA
      1004 2022 FLA KEYS   S09    1                1473   EPI MORI 1           NA
      1005 2022 FLA KEYS   S09    1                1474   EPI MORI 1           NA
      1006 2022 FLA KEYS   S09    1                1475   EPI MORI 1           NA
      1007 2022 FLA KEYS   S09    1                1476   EPI MORI 1           NA
      1008 2022 FLA KEYS   S09    1                1477   EPI MORI 1           NA
      1009 2022 FLA KEYS   S09    1                1478   EPI MORI 1           NA
      1010 2022 FLA KEYS   S09    1                1479   EPI MORI 1           NA
      1011 2022 FLA KEYS   S09    1                1480   EPI MORI 1           NA
      1012 2022 FLA KEYS   S09    1                1893   EPI MORI 1           NA
      1013 2022 FLA KEYS   S09    1                1960   EPI MORI 1           NA
      1014 2022 FLA KEYS   S11    0                1304   EPI MORI 1           NA
      1015 2022 FLA KEYS   S11    0                1306   EPI MORI 1           NA
      1016 2022 FLA KEYS   S11    0                1311   EPI MORI 1           NA
      1017 2022 FLA KEYS   S11    0                1317   EPI MORI 1           NA
      1018 2022 FLA KEYS   S11    0                1320   EPI MORI 1           NA
      1019 2022 FLA KEYS   S11    0                1321   EPI MORI 1           NA
      1020 2022 FLA KEYS   S11    0                1324   EPI MORI 1           NA
      1021 2022 FLA KEYS   S11    0                1325   EPI MORI 1           NA
      1022 2022 FLA KEYS   S11    0                1333   EPI MORI 1           NA
      1023 2022 FLA KEYS   S11    0                1334   EPI MORI 1           NA
      1024 2022 FLA KEYS   S11    0                1335   EPI MORI 1           NA
      1025 2022 FLA KEYS   S11    0                1336   EPI MORI 1           NA
      1026 2022 FLA KEYS   S11    0                1339   EPI MORI 1           NA
      1027 2022 FLA KEYS   S11    0                1340   EPI MORI 1           NA
      1028 2022 FLA KEYS   S11    0                1341   EPI MORI 1           NA
      1029 2022 FLA KEYS   S11    0                1342   EPI MORI 1           NA
      1030 2022 FLA KEYS   S11    0                1343   EPI MORI 1           NA
      1031 2022 FLA KEYS   S11    0                1347   EPI MORI 1           NA
      1032 2022 FLA KEYS   S11    0                1349   EPI MORI 1           NA
      1033 2022 FLA KEYS   S11    0                1350   EPI MORI 1           NA
      1034 2022 FLA KEYS   S11    0                1351   EPI MORI 1           NA
      1035 2022 FLA KEYS   S11    0                1353   EPI MORI 1           NA
      1036 2022 FLA KEYS   S11    0                1354   EPI MORI 1           NA
      1037 2022 FLA KEYS   S11    0                1355   EPI MORI 1           NA
      1038 2022 FLA KEYS   S11    0                1356   EPI MORI 1           NA
      1039 2022 FLA KEYS   S11    0                1357   EPI MORI 1           NA
      1040 2022 FLA KEYS   S11    0                1358   EPI MORI 1           NA
      1041 2022 FLA KEYS   S11    0                1359   EPI MORI 1           NA
      1042 2022 FLA KEYS   S11    0                1360   EPI MORI 1           NA
      1043 2022 FLA KEYS   S11    0                1361   EPI MORI 1           NA
      1044 2022 FLA KEYS   S11    0                1362   EPI MORI 1           NA
      1045 2022 FLA KEYS   S11    0                1364   EPI MORI 1           NA
      1046 2022 FLA KEYS   S11    0                1715   EPI MORI 1           NA
      1047 2022 FLA KEYS   S11    0                1780   EPI MORI 1           NA
      1048 2022 FLA KEYS   S11    0                1795   EPI MORI 1           NA
      1049 2022 FLA KEYS   S11    0                1797   EPI MORI 1           NA
      1050 2022 FLA KEYS   S11    0                1810   EPI MORI 1           NA
      1051 2022 FLA KEYS   S11    0                1814   EPI MORI 1           NA
      1052 2022 FLA KEYS   S11    0                1825   EPI MORI 1           NA
      1053 2022 FLA KEYS   S11    0                1841   EPI MORI 1           NA
      1054 2022 FLA KEYS   S11    0                1845   EPI MORI 1           NA
      1055 2022 FLA KEYS   S11    0                1846   EPI MORI 1           NA
      1056 2022 FLA KEYS   S11    0                8051   EPI MORI 1           NA
      1057 2022 FLA KEYS   S11    0                8052   EPI MORI 1           NA
      1058 2022 FLA KEYS   S11    0                8053   EPI MORI 1           NA
      1059 2022 FLA KEYS   S11    0                8055   EPI MORI 1           NA
      1060 2022 FLA KEYS   S11    0                8056   EPI MORI 1           NA
      1061 2022 FLA KEYS   S11    0                8059   EPI MORI 1           NA
      1062 2022 FLA KEYS   S11    0                8060   EPI MORI 1           NA
      1063 2022 FLA KEYS   S11    0                8061   EPI MORI 1           NA
      1064 2022 FLA KEYS   S11    0                8068   EPI MORI 1           NA
               biomass
      1    0.000000000
      2    0.000000000
      3    0.000000000
      4    0.000000000
      5    0.000000000
      6    0.000000000
      7    0.000000000
      8    0.000000000
      9    0.000000000
      10   0.541782457
      11   0.000000000
      12   0.000000000
      13   0.000000000
      14   0.000000000
      15   0.000000000
      16   0.000000000
      17   0.000000000
      18   0.000000000
      19   0.000000000
      20   0.000000000
      21   0.000000000
      22   0.000000000
      23   0.000000000
      24   0.000000000
      25   0.000000000
      26   0.000000000
      27   0.000000000
      28   0.000000000
      29   0.000000000
      30   0.000000000
      31   0.000000000
      32   0.226557598
      33   0.004182186
      34   0.000000000
      35   0.000000000
      36   0.000000000
      37   0.000000000
      38   0.000000000
      39   0.000000000
      40   0.190836599
      41   0.000000000
      42   0.000000000
      43   0.000000000
      44   0.000000000
      45   0.318800087
      46   0.000000000
      47   0.000000000
      48   0.000000000
      49   0.000000000
      50   0.479459191
      51   0.000000000
      52   0.000000000
      53   0.000000000
      54   0.000000000
      55   0.000000000
      56   0.054409469
      57   4.474799836
      58   0.000000000
      59   0.000000000
      60   0.000000000
      61   0.000000000
      62   0.000000000
      63   0.000000000
      64   0.000000000
      65   0.000000000
      66   0.000000000
      67   0.000000000
      68   0.000000000
      69   0.000000000
      70   0.000000000
      71   0.000000000
      72   0.190836599
      73   0.148684250
      74   0.000000000
      75   0.000000000
      76   0.000000000
      77   0.000000000
      78   0.000000000
      79   0.000000000
      80   0.000000000
      81   0.543417716
      82   0.420127180
      83   0.000000000
      84   0.000000000
      85   0.000000000
      86   0.000000000
      87   0.000000000
      88   0.075534103
      89   0.000000000
      90   1.058316260
      91   0.000000000
      92   0.000000000
      93   0.823514227
      94   0.000000000
      95   0.067640604
      96   0.136162327
      97   0.000000000
      98   0.000000000
      99   0.000000000
      100  0.000000000
      101  0.000000000
      102  0.000000000
      103  0.000000000
      104  0.000000000
      105  0.000000000
      106  0.000000000
      107  0.000000000
      108  0.000000000
      109  0.000000000
      110  0.000000000
      111  0.000000000
      112  0.000000000
      113  0.000000000
      114  0.000000000
      115  0.000000000
      116  0.000000000
      117  0.000000000
      118  0.000000000
      119  0.000000000
      120  0.000000000
      121  0.000000000
      122  0.206490243
      123  0.000000000
      124  0.249441126
      125  0.688687837
      126  0.000000000
      127  0.000000000
      128  0.000000000
      129  0.000000000
      130  0.000000000
      131  0.000000000
      132  0.222982369
      133  0.000000000
      134  0.000000000
      135  0.000000000
      136  0.000000000
      137  0.000000000
      138  0.000000000
      139  0.000000000
      140  0.000000000
      141  0.000000000
      142  0.000000000
      143  0.000000000
      144  0.000000000
      145  0.284846577
      146  0.000000000
      147  0.222982369
      148  0.000000000
      149  0.000000000
      150  0.000000000
      151  0.000000000
      152  0.102872950
      153  0.000000000
      154  0.136162327
      155  0.000000000
      156  1.396757837
      157  0.093128254
      158  0.000000000
      159  0.000000000
      160  0.000000000
      161  0.000000000
      162  0.000000000
      163  0.000000000
      164  0.000000000
      165  0.000000000
      166  0.000000000
      167  0.000000000
      168  0.000000000
      169  0.000000000
      170  0.000000000
      171  0.905043040
      172  0.000000000
      173  0.000000000
      174  0.000000000
      175  0.093128254
      176  0.000000000
      177  0.000000000
      178  0.000000000
      179  0.000000000
      180  0.000000000
      181  0.000000000
      182  0.363777179
      183  0.000000000
      184  0.000000000
      185  0.973218800
      186  0.000000000
      187  0.000000000
      188  1.452009340
      189  0.000000000
      190  0.093128254
      191  0.258571139
      192  0.000000000
      193  0.000000000
      194  0.000000000
      195  0.166829650
      196  0.000000000
      197  0.287118080
      198  0.000000000
      199  0.222982369
      200  0.000000000
      201  0.000000000
      202  0.000000000
      203  0.000000000
      204  0.000000000
      205  0.000000000
      206  2.590986367
      207  0.000000000
      208  0.000000000
      209  0.000000000
      210  0.000000000
      211  0.661909489
      212  0.000000000
      213  0.000000000
      214  0.000000000
      215  0.000000000
      216  0.000000000
      217  0.000000000
      218  0.000000000
      219  0.000000000
      220  0.000000000
      221  0.000000000
      222  0.000000000
      223  0.000000000
      224  0.356347305
      225  0.474722888
      226  0.000000000
      227  0.148684250
      228  0.000000000
      229  0.113278799
      230  0.000000000
      231  0.000000000
      232  0.000000000
      233  0.000000000
      234  0.000000000
      235  0.000000000
      236  0.000000000
      237  0.000000000
      238  0.000000000
      239  0.000000000
      240  0.000000000
      241  0.000000000
      242  0.000000000
      243  0.000000000
      244  0.000000000
      245  0.000000000
      246  0.000000000
      247  0.318800087
      248  0.047310721
      249  0.000000000
      250  0.763321504
      251  0.885150298
      252  0.000000000
      253  0.000000000
      254  0.000000000
      255  0.000000000
      256  0.000000000
      257  0.000000000
      258  0.000000000
      259  0.802591399
      260  0.000000000
      261  1.496472373
      262  0.000000000
      263  0.512111827
      264  0.000000000
      265  0.000000000
      266  0.000000000
      267  0.000000000
      268  0.000000000
      269  0.000000000
      270  0.000000000
      271  0.000000000
      272  0.000000000
      273  0.000000000
      274  0.000000000
      275  0.000000000
      276  0.000000000
      277  0.000000000
      278  0.000000000
      279  0.000000000
      280  0.000000000
      281  0.000000000
      282  0.000000000
      283  0.000000000
      284  0.000000000
      285  0.000000000
      286  0.000000000
      287  0.000000000
      288  0.000000000
      289  0.000000000
      290  0.000000000
      291  0.000000000
      292  0.000000000
      293  0.084022650
      294  0.000000000
      295  0.000000000
      296  0.000000000
      297  0.000000000
      298  0.000000000
      299  2.125446097
      300  0.000000000
      301  0.000000000
      302  0.297781234
      303  0.318800087
      304  0.000000000
      305  0.628115421
      306  2.602898007
      307  0.000000000
      308  1.607055350
      309  0.148684250
      310  0.000000000
      311  0.000000000
      312  0.075534103
      313  0.000000000
      314  0.432649103
      315  0.496034508
      316  1.076091749
      317  0.318800087
      318  0.000000000
      319  0.206490243
      320  0.762163294
      321  0.206490243
      322  0.000000000
      323  0.136162327
      324  0.258571139
      325  0.113278799
      326  0.843193461
      327  0.000000000
      328  0.725361177
      329  0.000000000
      330  0.023282823
      331  0.000000000
      332  0.000000000
      333  1.094360447
      334  0.113278799
      335  0.340791278
      336  0.000000000
      337  0.000000000
      338  0.000000000
      339  0.000000000
      340  0.000000000
      341  0.000000000
      342  0.093128254
      343  0.000000000
      344  0.000000000
      345  0.000000000
      346  0.222982369
      347  0.000000000
      348  0.000000000
      349  0.000000000
      350  0.000000000
      351  0.000000000
      352  0.000000000
      353  0.000000000
      354  0.541782457
      355  0.000000000
      356  1.109381127
      357  0.000000000
      358  0.000000000
      359  0.842636539
      360  0.637600174
      361  0.000000000
      362  1.760466160
      363  0.000000000
      364  0.000000000
      365  0.512461429
      366  0.000000000
      367  0.093128254
      368  0.222982369
      369  0.000000000
      370  0.093128254
      371  0.113278799
      372  0.336261168
      373  0.000000000
      374  0.000000000
      375  0.000000000
      376  0.240335241
      377  0.148684250
      378  0.563295008
      379  0.856449757
      380  0.000000000
      381  0.277712364
      382  0.500694733
      383  0.355174493
      384  0.000000000
      385  1.264380482
      386  0.258571139
      387  0.190836599
      388  0.763321504
      389  0.000000000
      390  0.000000000
      391  0.102872950
      392  0.000000000
      393  0.156423801
      394  0.000000000
      395  0.000000000
      396  0.353614040
      397  0.000000000
      398  0.000000000
      399  0.381673198
      400  0.000000000
      401  0.258571139
      402  0.000000000
      403  0.000000000
      404  0.000000000
      405  0.000000000
      406  0.690575465
      407  0.284846577
      408  0.511205691
      409  0.000000000
      410  0.000000000
      411  0.000000000
      412  0.000000000
      413  0.000000000
      414  0.637600174
      415  0.000000000
      416  0.000000000
      417  0.000000000
      418  0.000000000
      419  0.000000000
      420  0.000000000
      421  0.168045300
      422  0.000000000
      423  0.000000000
      424  0.000000000
      425  0.000000000
      426  0.000000000
      427  0.000000000
      428  0.000000000
      429  0.000000000
      430  0.000000000
      431  0.000000000
      432  0.000000000
      433  0.000000000
      434  0.387005038
      435  0.000000000
      436  0.000000000
      437  0.000000000
      438  0.000000000
      439  0.000000000
      440  0.000000000
      441  0.000000000
      442  0.000000000
      443  0.000000000
      444  0.248735776
      445  0.000000000
      446  0.000000000
      447  0.000000000
      448  0.000000000
      449  0.000000000
      450  0.000000000
      451  0.000000000
      452  3.473080606
      453  0.000000000
      454  0.000000000
      455  0.205745900
      456  0.000000000
      457  0.000000000
      458  0.000000000
      459  0.000000000
      460  0.000000000
      461  0.000000000
      462  0.000000000
      463  0.248735776
      464  0.000000000
      465  0.000000000
      466  0.000000000
      467  0.000000000
      468  0.072660986
      469  0.000000000
      470  0.151068206
      471  0.000000000
      472  0.000000000
      473  0.000000000
      474  0.517142278
      475  0.000000000
      476  0.000000000
      477  0.000000000
      478  0.000000000
      479  0.000000000
      480  0.412980485
      481  0.000000000
      482  0.033224689
      483  0.000000000
      484  0.000000000
      485  0.000000000
      486  0.000000000
      487  0.000000000
      488  0.000000000
      489  0.517142278
      490  0.000000000
      491  0.000000000
      492  0.000000000
      493  0.000000000
      494  0.381673198
      495  0.094621443
      496  0.000000000
      497  1.394996518
      498  0.000000000
      499  0.000000000
      500  0.000000000
      501  0.000000000
      502  0.000000000
      503  0.323911627
      504  0.000000000
      505  0.000000000
      506  0.000000000
      507  0.000000000
      508  0.000000000
      509  0.000000000
      510  0.000000000
      511  0.000000000
      512  0.000000000
      513  0.000000000
      514  0.000000000
      515  0.000000000
      516  0.651710638
      517  0.000000000
      518  0.000000000
      519  0.000000000
      520  0.000000000
      521  0.000000000
      522  0.465413801
      523  0.205745900
      524  0.186256508
      525  0.000000000
      526  0.454481676
      527  0.000000000
      528  0.205745900
      529  0.000000000
      530  0.000000000
      531  0.000000000
      532  0.000000000
      533  0.000000000
      534  0.000000000
      535  0.000000000
      536  0.000000000
      537  0.000000000
      538  0.000000000
      539  0.595562468
      540  0.000000000
      541  0.000000000
      542  0.000000000
      543  0.000000000
      544  0.000000000
      545  0.000000000
      546  0.000000000
      547  0.000000000
      548  0.000000000
      549  0.000000000
      550  0.000000000
      551  0.000000000
      552  0.000000000
      553  1.055397178
      554  0.000000000
      555  0.000000000
      556  0.000000000
      557  0.000000000
      558  0.418533070
      559  0.167205987
      560  0.000000000
      561  0.351998393
      562  0.000000000
      563  0.000000000
      564  0.000000000
      565  0.248735776
      566  0.000000000
      567  0.323911627
      568  0.000000000
      569  0.000000000
      570  0.000000000
      571  0.000000000
      572  0.000000000
      573  0.000000000
      574  0.000000000
      575  0.000000000
      576  0.151068206
      577  0.000000000
      578  0.000000000
      579  0.000000000
      580  0.000000000
      581  0.000000000
      582  0.000000000
      583  0.000000000
      584  0.000000000
      585  0.000000000
      586  0.000000000
      587  0.000000000
      588  0.000000000
      589  0.272324654
      590  0.323911627
      591  0.000000000
      592  1.150987195
      593  0.000000000
      594  0.000000000
      595  0.151068206
      596  0.272324654
      597  0.000000000
      598  0.000000000
      599  0.000000000
      600  0.207942193
      601  1.526643007
      602  0.000000000
      603  0.000000000
      604  0.000000000
      605  0.000000000
      606  0.000000000
      607  0.000000000
      608  0.000000000
      609  0.000000000
      610  0.495743119
      611  0.000000000
      612  0.000000000
      613  0.000000000
      614  0.000000000
      615  0.000000000
      616  0.000000000
      617  0.000000000
      618  0.000000000
      619  0.000000000
      620  0.000000000
      621  0.000000000
      622  0.000000000
      623  0.107101702
      624  0.000000000
      625  0.000000000
      626  0.000000000
      627  0.000000000
      628  0.000000000
      629  0.000000000
      630  0.000000000
      631  0.000000000
      632  0.000000000
      633  0.168045300
      634  0.000000000
      635  0.000000000
      636  0.000000000
      637  0.409325984
      638  0.000000000
      639  0.000000000
      640  0.000000000
      641  0.000000000
      642  0.000000000
      643  0.000000000
      644  0.000000000
      645  1.079552751
      646  0.764738334
      647  0.000000000
      648  0.000000000
      649  0.000000000
      650  0.000000000
      651  0.000000000
      652  0.000000000
      653  0.000000000
      654  0.000000000
      655  0.000000000
      656  0.000000000
      657  0.000000000
      658  0.000000000
      659  0.000000000
      660  0.000000000
      661  0.000000000
      662  0.480670482
      663  0.000000000
      664  0.000000000
      665  0.000000000
      666  0.000000000
      667  0.000000000
      668  0.000000000
      669  0.000000000
      670  0.000000000
      671  0.000000000
      672  1.858046155
      673  0.632221246
      674  0.000000000
      675  0.381673198
      676  0.000000000
      677  0.000000000
      678  0.000000000
      679  0.000000000
      680  0.000000000
      681  0.000000000
      682  0.000000000
      683  0.000000000
      684  0.000000000
      685  0.775560359
      686  0.000000000
      687  0.000000000
      688  0.000000000
      689  0.000000000
      690  0.000000000
      691  0.000000000
      692  0.000000000
      693  0.000000000
      694  0.000000000
      695  0.000000000
      696  0.000000000
      697  2.241811649
      698  0.000000000
      699  0.000000000
      700  0.000000000
      701  0.000000000
      702  0.000000000
      703  0.000000000
      704  0.000000000
      705  0.000000000
      706  0.000000000
      707  0.000000000
      708  0.381673198
      709  2.957568845
      710  0.000000000
      711  0.205745900
      712  0.000000000
      713  0.000000000
      714  0.000000000
      715  0.000000000
      716  0.000000000
      717  0.000000000
      718  0.000000000
      719  0.000000000
      720  0.000000000
      721  0.000000000
      722  0.000000000
      723  0.000000000
      724  0.000000000
      725  0.000000000
      726  0.000000000
      727  0.000000000
      728  0.000000000
      729  0.000000000
      730  0.000000000
      731  0.000000000
      732  0.000000000
      733  0.000000000
      734  0.000000000
      735  0.000000000
      736  0.000000000
      737  0.000000000
      738  0.000000000
      739  0.000000000
      740  0.000000000
      741  0.000000000
      742  0.000000000
      743  0.000000000
      744  0.000000000
      745  0.000000000
      746  0.000000000
      747  0.000000000
      748  0.000000000
      749  0.000000000
      750  0.000000000
      751  0.000000000
      752  0.000000000
      753  0.323911627
      754  0.000000000
      755  0.000000000
      756  0.000000000
      757  0.634693214
      758  0.000000000
      759  0.000000000
      760  0.000000000
      761  0.000000000
      762  0.000000000
      763  0.000000000
      764  0.000000000
      765  0.000000000
      766  0.000000000
      767  0.000000000
      768  0.000000000
      769  0.000000000
      770  0.000000000
      771  0.000000000
      772  0.000000000
      773  0.000000000
      774  0.000000000
      775  0.000000000
      776  0.000000000
      777  0.000000000
      778  0.000000000
      779  0.000000000
      780  0.000000000
      781  0.000000000
      782  0.000000000
      783  0.000000000
      784  0.000000000
      785  0.000000000
      786  0.000000000
      787  0.000000000
      788  0.000000000
      789  0.000000000
      790  2.317373938
      791  0.000000000
      792  0.000000000
      793  0.000000000
      794  0.000000000
      795  0.000000000
      796  0.000000000
      797  0.000000000
      798  0.000000000
      799  0.000000000
      800  0.000000000
      801  0.000000000
      802  0.000000000
      803  0.000000000
      804  0.000000000
      805  0.000000000
      806  0.000000000
      807  0.000000000
      808  0.000000000
      809  0.000000000
      810  0.000000000
      811  0.000000000
      812  0.000000000
      813  0.000000000
      814  0.000000000
      815  0.000000000
      816  0.000000000
      817  0.000000000
      818  0.000000000
      819  0.000000000
      820  0.000000000
      821  0.000000000
      822  0.000000000
      823  0.000000000
      824  0.000000000
      825  0.000000000
      826  0.000000000
      827  0.000000000
      828  0.000000000
      829  1.686386922
      830  0.000000000
      831  0.000000000
      832  0.000000000
      833  0.000000000
      834  0.000000000
      835  0.000000000
      836  0.000000000
      837  0.000000000
      838  0.000000000
      839  0.000000000
      840  0.000000000
      841  0.000000000
      842  0.000000000
      843  0.000000000
      844  0.000000000
      845  0.000000000
      846  0.000000000
      847  0.000000000
      848  1.946437599
      849  0.000000000
      850  0.000000000
      851  0.000000000
      852  0.000000000
      853  0.000000000
      854  0.000000000
      855  0.000000000
      856  0.000000000
      857  0.000000000
      858  0.000000000
      859  0.000000000
      860  0.272324654
      861  0.000000000
      862  0.000000000
      863  0.000000000
      864  0.000000000
      865  0.000000000
      866  0.226557598
      867  0.000000000
      868  0.000000000
      869  0.000000000
      870  0.000000000
      871  0.000000000
      872  0.000000000
      873  0.000000000
      874  0.000000000
      875  0.000000000
      876  0.000000000
      877  0.000000000
      878  0.000000000
      879  0.000000000
      880  0.000000000
      881  0.186256508
      882  0.000000000
      883  0.000000000
      884  0.000000000
      885  0.000000000
      886  0.000000000
      887  0.000000000
      888  0.000000000
      889  1.507227928
      890  0.000000000
      891  0.555424727
      892  0.000000000
      893  0.000000000
      894  0.381673198
      895  0.000000000
      896  0.000000000
      897  0.000000000
      898  0.000000000
      899  0.000000000
      900  0.000000000
      901  1.238223693
      902  0.000000000
      903  1.072567005
      904  0.000000000
      905  0.000000000
      906  0.000000000
      907  0.000000000
      908  0.000000000
      909  0.000000000
      910  0.000000000
      911  0.000000000
      912  0.000000000
      913  0.000000000
      914  0.000000000
      915  0.000000000
      916  0.000000000
      917  0.000000000
      918  0.000000000
      919  0.000000000
      920  0.000000000
      921  0.000000000
      922  0.000000000
      923  0.000000000
      924  0.000000000
      925  0.000000000
      926  0.000000000
      927  0.000000000
      928  0.000000000
      929  0.000000000
      930  0.000000000
      931  0.000000000
      932  0.000000000
      933  0.000000000
      934  0.000000000
      935  0.000000000
      936  0.000000000
      937  0.000000000
      938  0.000000000
      939  0.000000000
      940  0.000000000
      941  0.000000000
      942  0.000000000
      943  0.000000000
      944  0.000000000
      945  0.000000000
      946  0.000000000
      947  0.000000000
      948  0.000000000
      949  0.000000000
      950  0.000000000
      951  0.000000000
      952  0.000000000
      953  0.000000000
      954  0.000000000
      955  0.000000000
      956  0.000000000
      957  0.000000000
      958  0.000000000
      959  0.000000000
      960  0.000000000
      961  0.000000000
      962  0.000000000
      963  0.000000000
      964  0.000000000
      965  0.000000000
      966  0.000000000
      967  0.000000000
      968  0.000000000
      969  0.000000000
      970  1.793648420
      971  0.586060490
      972  0.000000000
      973  0.000000000
      974  0.000000000
      975  0.000000000
      976  0.000000000
      977  0.000000000
      978  0.000000000
      979  0.000000000
      980  0.000000000
      981  0.000000000
      982  0.000000000
      983  0.000000000
      984  0.000000000
      985  0.000000000
      986  0.000000000
      987  0.000000000
      988  0.000000000
      989  0.000000000
      990  0.000000000
      991  0.000000000
      992  0.000000000
      993  0.434992284
      994  0.000000000
      995  1.172328196
      996  0.000000000
      997  0.000000000
      998  0.297368500
      999  0.000000000
      1000 0.000000000
      1001 0.000000000
      1002 0.000000000
      1003 0.000000000
      1004 0.000000000
      1005 0.000000000
      1006 1.377375673
      1007 0.000000000
      1008 1.238223693
      1009 0.000000000
      1010 0.000000000
      1011 0.000000000
      1012 0.000000000
      1013 0.000000000
      1014 0.000000000
      1015 0.000000000
      1016 0.000000000
      1017 0.215261785
      1018 0.000000000
      1019 0.000000000
      1020 0.000000000
      1021 0.000000000
      1022 0.000000000
      1023 0.000000000
      1024 0.000000000
      1025 0.000000000
      1026 0.000000000
      1027 0.000000000
      1028 0.000000000
      1029 0.000000000
      1030 0.000000000
      1031 0.000000000
      1032 0.000000000
      1033 0.000000000
      1034 0.000000000
      1035 0.000000000
      1036 0.000000000
      1037 0.000000000
      1038 0.000000000
      1039 0.000000000
      1040 0.000000000
      1041 0.000000000
      1042 0.000000000
      1043 0.000000000
      1044 0.000000000
      1045 0.120640342
      1046 0.000000000
      1047 0.000000000
      1048 0.000000000
      1049 1.238223693
      1050 0.000000000
      1051 0.000000000
      1052 0.000000000
      1053 0.000000000
      1054 0.000000000
      1055 0.000000000
      1056 0.000000000
      1057 0.000000000
      1058 0.000000000
      1059 0.000000000
      1060 0.000000000
      1061 0.000000000
      1062 0.000000000
      1063 0.000000000
      1064 0.000000000

