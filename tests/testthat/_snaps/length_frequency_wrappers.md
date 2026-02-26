# getDomainLengthFrequency handles Base Case exactly as baseline

    Code
      res
    Output
         YEAR   REGION SPECIES_CD length_class    frequency
      1  2012 FLA KEYS   EPI MORI            5 0.0184779166
      2  2012 FLA KEYS   EPI MORI            8 0.0184779166
      3  2012 FLA KEYS   EPI MORI           19 0.0047892944
      4  2012 FLA KEYS   EPI MORI           20 0.0079104824
      5  2012 FLA KEYS   EPI MORI           22 0.0047892944
      6  2012 FLA KEYS   EPI MORI           23 0.0079104824
      7  2012 FLA KEYS   EPI MORI           24 0.0020599550
      8  2012 FLA KEYS   EPI MORI           25 0.0119479476
      9  2012 FLA KEYS   EPI MORI           27 0.0079104824
      10 2012 FLA KEYS   EPI MORI           28 0.0126997768
      11 2012 FLA KEYS   EPI MORI           29 0.0406800409
      12 2012 FLA KEYS   EPI MORI           30 0.0696696527
      13 2012 FLA KEYS   EPI MORI           31 0.0361423747
      14 2012 FLA KEYS   EPI MORI           32 0.0889850787
      15 2012 FLA KEYS   EPI MORI           33 0.0339034684
      16 2012 FLA KEYS   EPI MORI           34 0.0832674569
      17 2012 FLA KEYS   EPI MORI           35 0.0895654812
      18 2012 FLA KEYS   EPI MORI           36 0.0079104824
      19 2012 FLA KEYS   EPI MORI           37 0.0047892944
      20 2012 FLA KEYS   EPI MORI           38 0.0618605712
      21 2012 FLA KEYS   EPI MORI           39 0.0222286642
      22 2012 FLA KEYS   EPI MORI           40 0.0748216852
      23 2012 FLA KEYS   EPI MORI           41 0.0142684805
      24 2012 FLA KEYS   EPI MORI           42 0.0317618868
      25 2012 FLA KEYS   EPI MORI           43 0.0094791861
      26 2012 FLA KEYS   EPI MORI           44 0.0253995535
      27 2012 FLA KEYS   EPI MORI           45 0.0532413751
      28 2012 FLA KEYS   EPI MORI           46 0.0047892944
      29 2012 FLA KEYS   EPI MORI           47 0.0094732488
      30 2012 FLA KEYS   EPI MORI           48 0.0238967705
      31 2012 FLA KEYS   EPI MORI           50 0.0198584300
      32 2012 FLA KEYS   EPI MORI           51 0.0012095301
      33 2012 FLA KEYS   EPI MORI           53 0.0003532364
      34 2012 FLA KEYS   EPI MORI           54 0.0006431338
      35 2012 FLA KEYS   EPI MORI           55 0.0206102591
      36 2012 FLA KEYS   EPI MORI           57 0.0008504249
      37 2012 FLA KEYS   EPI MORI           58 0.0091200125
      38 2012 FLA KEYS   EPI MORI           59 0.0047892944
      39 2012 FLA KEYS   EPI MORI           60 0.0174882642
      40 2012 FLA KEYS   EPI MORI           61 0.0008504249
      41 2012 FLA KEYS   EPI MORI           62 0.0056397193
      42 2012 FLA KEYS   EPI MORI           63 0.0008504249
      43 2012 FLA KEYS   EPI MORI           65 0.0107881188
      44 2012 FLA KEYS   EPI MORI           70 0.0051425307
      45 2012 FLA KEYS   EPI MORI           73 0.0047892944
      46 2012 FLA KEYS   EPI MORI           75 0.0079104824
      47 2012 FLA KEYS   EPI MORI           80 0.0059988244

# getDomainLengthFrequency handles Sequence Length Bins exactly as baseline

    Code
      res
    Output
        YEAR   REGION SPECIES_CD length_class   frequency
      1 2012 FLA KEYS   EPI MORI       [0,10) 0.036955833
      2 2012 FLA KEYS   EPI MORI      [10,20) 0.004789294
      3 2012 FLA KEYS   EPI MORI      [20,30) 0.095908462
      4 2012 FLA KEYS   EPI MORI      [30,40) 0.498322525
      5 2012 FLA KEYS   EPI MORI      [40,50) 0.247131481
      6 2012 FLA KEYS   EPI MORI      [50,60) 0.057434321
      7 2012 FLA KEYS   EPI MORI      [60,70) 0.035616952
      8 2012 FLA KEYS   EPI MORI      [70,80) 0.017842308
      9 2012 FLA KEYS   EPI MORI      [80,90) 0.005998824

# getDomainLengthFrequency handles Split Protected Status exactly as baseline

    Code
      res
    Output
         YEAR   REGION SPECIES_CD length_class   frequency protected_status
      1  2012 FLA KEYS   EPI MORI            5 0.019332722                0
      2  2012 FLA KEYS   EPI MORI            8 0.019332722                0
      3  2012 FLA KEYS   EPI MORI           19 0.005010851                0
      4  2012 FLA KEYS   EPI MORI           20 0.008276428                0
      5  2012 FLA KEYS   EPI MORI           22 0.005010851                0
      6  2012 FLA KEYS   EPI MORI           23 0.008276428                0
      7  2012 FLA KEYS   EPI MORI           25 0.009969702                0
      8  2012 FLA KEYS   EPI MORI           27 0.008276428                0
      9  2012 FLA KEYS   EPI MORI           28 0.013287280                0
      10 2012 FLA KEYS   EPI MORI           29 0.042561936                0
      11 2012 FLA KEYS   EPI MORI           30 0.064639124                0
      12 2012 FLA KEYS   EPI MORI           31 0.036251701                0
      13 2012 FLA KEYS   EPI MORI           32 0.091836124                0
      14 2012 FLA KEYS   EPI MORI           33 0.034798987                0
      15 2012 FLA KEYS   EPI MORI           34 0.085773713                0
      16 2012 FLA KEYS   EPI MORI           35 0.092666397                0
      17 2012 FLA KEYS   EPI MORI           36 0.008276428                0
      18 2012 FLA KEYS   EPI MORI           37 0.005010851                0
      19 2012 FLA KEYS   EPI MORI           38 0.064049411                0
      20 2012 FLA KEYS   EPI MORI           39 0.023256982                0
      21 2012 FLA KEYS   EPI MORI           40 0.074123114                0
      22 2012 FLA KEYS   EPI MORI           41 0.014928553                0
      23 2012 FLA KEYS   EPI MORI           42 0.031292850                0
      24 2012 FLA KEYS   EPI MORI           43 0.009917702                0
      25 2012 FLA KEYS   EPI MORI           44 0.026574560                0
      26 2012 FLA KEYS   EPI MORI           45 0.052663407                0
      27 2012 FLA KEYS   EPI MORI           46 0.005010851                0
      28 2012 FLA KEYS   EPI MORI           47 0.008276428                0
      29 2012 FLA KEYS   EPI MORI           48 0.025002257                0
      30 2012 FLA KEYS   EPI MORI           50 0.018246131                0
      31 2012 FLA KEYS   EPI MORI           55 0.021563708                0
      32 2012 FLA KEYS   EPI MORI           58 0.008276428                0
      33 2012 FLA KEYS   EPI MORI           59 0.005010851                0
      34 2012 FLA KEYS   EPI MORI           60 0.014876553                0
      35 2012 FLA KEYS   EPI MORI           62 0.005010851                0
      36 2012 FLA KEYS   EPI MORI           65 0.010021703                0
      37 2012 FLA KEYS   EPI MORI           70 0.005010851                0
      38 2012 FLA KEYS   EPI MORI           73 0.005010851                0
      39 2012 FLA KEYS   EPI MORI           75 0.008276428                0
      40 2012 FLA KEYS   EPI MORI           80 0.005010851                0
      41 2012 FLA KEYS   EPI MORI           24 0.046589031                1
      42 2012 FLA KEYS   EPI MORI           25 0.054710744                1
      43 2012 FLA KEYS   EPI MORI           30 0.178412221                1
      44 2012 FLA KEYS   EPI MORI           31 0.033779113                1
      45 2012 FLA KEYS   EPI MORI           32 0.027355372                1
      46 2012 FLA KEYS   EPI MORI           33 0.014545455                1
      47 2012 FLA KEYS   EPI MORI           34 0.029090909                1
      48 2012 FLA KEYS   EPI MORI           35 0.022534435                1
      49 2012 FLA KEYS   EPI MORI           38 0.014545455                1
      50 2012 FLA KEYS   EPI MORI           40 0.089922364                1
      51 2012 FLA KEYS   EPI MORI           42 0.041900826                1
      52 2012 FLA KEYS   EPI MORI           45 0.065735036                1
      53 2012 FLA KEYS   EPI MORI           47 0.035344353                1
      54 2012 FLA KEYS   EPI MORI           50 0.054710744                1
      55 2012 FLA KEYS   EPI MORI           51 0.027355372                1
      56 2012 FLA KEYS   EPI MORI           53 0.007988981                1
      57 2012 FLA KEYS   EPI MORI           54 0.014545455                1
      58 2012 FLA KEYS   EPI MORI           57 0.019233659                1
      59 2012 FLA KEYS   EPI MORI           58 0.027355372                1
      60 2012 FLA KEYS   EPI MORI           60 0.073944403                1
      61 2012 FLA KEYS   EPI MORI           61 0.019233659                1
      62 2012 FLA KEYS   EPI MORI           62 0.019233659                1
      63 2012 FLA KEYS   EPI MORI           63 0.019233659                1
      64 2012 FLA KEYS   EPI MORI           65 0.027355372                1
      65 2012 FLA KEYS   EPI MORI           70 0.007988981                1
      66 2012 FLA KEYS   EPI MORI           80 0.027355372                1

# Length Frequency wrappers correctly calculate mixed designs across multiple years

    Code
      res_domain
    Output
        YEAR   REGION SPECIES_CD length_class  frequency
      1 2012 FLA KEYS   EPI MORI       [0,20) 0.04962777
      2 2012 FLA KEYS   EPI MORI      [20,40) 0.57901653
      3 2012 FLA KEYS   EPI MORI      [40,60) 0.31331518
      4 2012 FLA KEYS   EPI MORI         <NA> 0.05804052
      5 2022 FLA KEYS   EPI MORI       [0,20) 0.01743955
      6 2022 FLA KEYS   EPI MORI      [20,40) 0.78278857
      7 2022 FLA KEYS   EPI MORI      [40,60) 0.16665789
      8 2022 FLA KEYS   EPI MORI         <NA> 0.03311399

---

    Code
      res_domain_2
    Output
         YEAR   REGION SPECIES_CD length_class    frequency
      1  2022 FLA KEYS   OCY CHRY          1.0 4.029828e-04
      2  2022 FLA KEYS   OCY CHRY          2.0 1.742595e-03
      3  2022 FLA KEYS   OCY CHRY          3.0 3.389534e-03
      4  2022 FLA KEYS   OCY CHRY          3.5 6.935022e-05
      5  2022 FLA KEYS   OCY CHRY          4.0 5.351673e-03
      6  2022 FLA KEYS   OCY CHRY          4.5 1.033904e-03
      7  2022 FLA KEYS   OCY CHRY          5.0 1.157483e-02
      8  2022 FLA KEYS   OCY CHRY          5.5 1.436492e-04
      9  2022 FLA KEYS   OCY CHRY          6.0 1.277419e-02
      10 2022 FLA KEYS   OCY CHRY          6.5 5.320927e-04
      11 2022 FLA KEYS   OCY CHRY          7.0 1.442822e-02
      12 2022 FLA KEYS   OCY CHRY          7.5 5.341017e-04
      13 2022 FLA KEYS   OCY CHRY          8.0 9.690311e-03
      14 2022 FLA KEYS   OCY CHRY          8.5 1.571624e-03
      15 2022 FLA KEYS   OCY CHRY          9.0 1.560792e-02
      16 2022 FLA KEYS   OCY CHRY          9.5 1.372007e-03
      17 2022 FLA KEYS   OCY CHRY         10.0 2.347566e-02
      18 2022 FLA KEYS   OCY CHRY         10.5 1.747746e-03
      19 2022 FLA KEYS   OCY CHRY         11.0 2.770625e-02
      20 2022 FLA KEYS   OCY CHRY         11.5 3.967500e-03
      21 2022 FLA KEYS   OCY CHRY         12.0 2.678656e-02
      22 2022 FLA KEYS   OCY CHRY         12.5 5.388481e-03
      23 2022 FLA KEYS   OCY CHRY         13.0 1.978453e-02
      24 2022 FLA KEYS   OCY CHRY         13.5 1.093833e-02
      25 2022 FLA KEYS   OCY CHRY         14.0 3.021061e-02
      26 2022 FLA KEYS   OCY CHRY         14.5 9.068016e-03
      27 2022 FLA KEYS   OCY CHRY         15.0 4.444227e-02
      28 2022 FLA KEYS   OCY CHRY         15.5 6.623411e-03
      29 2022 FLA KEYS   OCY CHRY         16.0 5.697944e-02
      30 2022 FLA KEYS   OCY CHRY         16.5 4.477527e-03
      31 2022 FLA KEYS   OCY CHRY         17.0 5.861458e-02
      32 2022 FLA KEYS   OCY CHRY         17.5 5.314583e-03
      33 2022 FLA KEYS   OCY CHRY         18.0 9.347823e-02
      34 2022 FLA KEYS   OCY CHRY         18.5 1.525081e-02
      35 2022 FLA KEYS   OCY CHRY         19.0 7.039787e-02
      36 2022 FLA KEYS   OCY CHRY         19.5 3.602279e-03
      37 2022 FLA KEYS   OCY CHRY         20.0 7.929206e-02
      38 2022 FLA KEYS   OCY CHRY         20.5 9.270107e-03
      39 2022 FLA KEYS   OCY CHRY         21.0 5.397662e-02
      40 2022 FLA KEYS   OCY CHRY         21.5 1.002055e-02
      41 2022 FLA KEYS   OCY CHRY         22.0 5.153104e-02
      42 2022 FLA KEYS   OCY CHRY         22.5 2.819044e-03
      43 2022 FLA KEYS   OCY CHRY         23.0 2.690378e-02
      44 2022 FLA KEYS   OCY CHRY         23.5 1.360441e-02
      45 2022 FLA KEYS   OCY CHRY         24.0 2.972541e-02
      46 2022 FLA KEYS   OCY CHRY         24.5 3.817907e-03
      47 2022 FLA KEYS   OCY CHRY         25.0 4.050591e-02
      48 2022 FLA KEYS   OCY CHRY         25.5 1.119299e-03
      49 2022 FLA KEYS   OCY CHRY         26.0 1.334498e-02
      50 2022 FLA KEYS   OCY CHRY         26.5 8.070295e-03
      51 2022 FLA KEYS   OCY CHRY         27.0 1.530455e-02
      52 2022 FLA KEYS   OCY CHRY         27.5 2.782395e-03
      53 2022 FLA KEYS   OCY CHRY         28.0 1.421545e-02
      54 2022 FLA KEYS   OCY CHRY         28.5 3.821411e-04
      55 2022 FLA KEYS   OCY CHRY         29.0 4.862197e-03
      56 2022 FLA KEYS   OCY CHRY         29.5 3.936236e-05
      57 2022 FLA KEYS   OCY CHRY         30.0 5.654956e-03
      58 2022 FLA KEYS   OCY CHRY         30.5 1.719140e-04
      59 2022 FLA KEYS   OCY CHRY         31.0 2.700343e-03
      60 2022 FLA KEYS   OCY CHRY         31.5 1.660579e-05
      61 2022 FLA KEYS   OCY CHRY         32.0 3.970864e-03
      62 2022 FLA KEYS   OCY CHRY         32.5 1.094757e-04
      63 2022 FLA KEYS   OCY CHRY         33.0 1.315323e-03
      64 2022 FLA KEYS   OCY CHRY         34.0 3.911659e-04
      65 2022 FLA KEYS   OCY CHRY         35.0 2.645487e-03
      66 2022 FLA KEYS   OCY CHRY         36.0 2.068092e-03
      67 2022 FLA KEYS   OCY CHRY         36.5 3.438281e-04
      68 2022 FLA KEYS   OCY CHRY         37.0 3.231734e-05
      69 2022 FLA KEYS   OCY CHRY         38.0 1.668653e-04
      70 2022 FLA KEYS   OCY CHRY         40.0 1.997285e-04
      71 2022 FLA KEYS   OCY CHRY         41.0 6.876562e-05
      72 2022 FLA KEYS   OCY CHRY         42.0 1.431358e-05
      73 2022 FLA KEYS   OCY CHRY         52.0 6.876562e-05

---

    Code
      res_strat
    Output
         YEAR   REGION STRAT PROT SPECIES_CD length_class  frequency
      1  2012 FLA KEYS  FDLR    0   EPI MORI       [0,20) 0.33333333
      2  2012 FLA KEYS  FDLR    0   EPI MORI      [20,40) 0.33333333
      3  2012 FLA KEYS  FDLR    0   EPI MORI      [40,60) 0.33333333
      4  2012 FLA KEYS  FMLR    0   EPI MORI      [20,40) 0.71739130
      5  2012 FLA KEYS  FMLR    0   EPI MORI      [40,60) 0.26086957
      6  2012 FLA KEYS  FMLR    0   EPI MORI         <NA> 0.02173913
      7  2012 FLA KEYS  FMLR    1   EPI MORI      [20,40) 0.40000000
      8  2012 FLA KEYS  FMLR    1   EPI MORI      [40,60) 0.40000000
      9  2012 FLA KEYS  FMLR    1   EPI MORI         <NA> 0.20000000
      10 2012 FLA KEYS  FSLR    0   EPI MORI      [20,40) 0.87500000
      11 2012 FLA KEYS  FSLR    0   EPI MORI      [40,60) 0.12500000
      12 2012 FLA KEYS  FSLR    1   EPI MORI      [20,40) 0.36363636
      13 2012 FLA KEYS  FSLR    1   EPI MORI      [40,60) 0.27272727
      14 2012 FLA KEYS  FSLR    1   EPI MORI         <NA> 0.36363636
      15 2012 FLA KEYS  INPR    0   EPI MORI      [20,40) 1.00000000
      16 2012 FLA KEYS  MCPR    0   EPI MORI       [0,20) 0.02127660
      17 2012 FLA KEYS  MCPR    0   EPI MORI      [20,40) 0.40425532
      18 2012 FLA KEYS  MCPR    0   EPI MORI      [40,60) 0.44680851
      19 2012 FLA KEYS  MCPR    0   EPI MORI         <NA> 0.12765957
      20 2012 FLA KEYS  MCPR    1   EPI MORI      [20,40) 0.25000000
      21 2012 FLA KEYS  MCPR    1   EPI MORI      [40,60) 0.66666667
      22 2012 FLA KEYS  MCPR    1   EPI MORI         <NA> 0.08333333
      23 2012 FLA KEYS  OFPR    0   EPI MORI      [20,40) 0.55555556
      24 2012 FLA KEYS  OFPR    0   EPI MORI      [40,60) 0.33333333
      25 2012 FLA KEYS  OFPR    0   EPI MORI         <NA> 0.11111111
      26 2012 FLA KEYS  OFPR    1   EPI MORI      [20,40) 0.70000000
      27 2012 FLA KEYS  OFPR    1   EPI MORI      [40,60) 0.30000000
      28 2022 FLA KEYS   S01    0   EPI MORI      [20,40) 1.00000000
      29 2022 FLA KEYS   S02    0   EPI MORI      [20,40) 1.00000000
      30 2022 FLA KEYS   S02    1   EPI MORI         <NA> 1.00000000
      31 2022 FLA KEYS   S03    0   EPI MORI       [0,20) 0.04545455
      32 2022 FLA KEYS   S03    0   EPI MORI      [20,40) 0.72727273
      33 2022 FLA KEYS   S03    0   EPI MORI      [40,60) 0.22727273
      34 2022 FLA KEYS   S03    1   EPI MORI      [40,60) 1.00000000
      35 2022 FLA KEYS   S05    0   EPI MORI       [0,20) 0.04166667
      36 2022 FLA KEYS   S05    0   EPI MORI      [20,40) 0.79166667
      37 2022 FLA KEYS   S05    0   EPI MORI      [40,60) 0.12500000
      38 2022 FLA KEYS   S05    0   EPI MORI         <NA> 0.04166667
      39 2022 FLA KEYS   S05    1   EPI MORI      [20,40) 0.75000000
      40 2022 FLA KEYS   S05    1   EPI MORI      [40,60) 0.25000000
      41 2022 FLA KEYS   S06    0   EPI MORI      [20,40) 0.38461538
      42 2022 FLA KEYS   S06    0   EPI MORI      [40,60) 0.46153846
      43 2022 FLA KEYS   S06    0   EPI MORI         <NA> 0.15384615
      44 2022 FLA KEYS   S06    1   EPI MORI      [20,40) 0.60000000
      45 2022 FLA KEYS   S06    1   EPI MORI      [40,60) 0.20000000
      46 2022 FLA KEYS   S06    1   EPI MORI         <NA> 0.20000000
      47 2022 FLA KEYS   S08    0   EPI MORI      [20,40) 1.00000000
      48 2022 FLA KEYS   S09    0   EPI MORI      [20,40) 0.33333333
      49 2022 FLA KEYS   S09    0   EPI MORI      [40,60) 0.66666667
      50 2022 FLA KEYS   S09    1   EPI MORI      [20,40) 0.50000000
      51 2022 FLA KEYS   S09    1   EPI MORI      [40,60) 0.50000000
      52 2022 FLA KEYS   S11    0   EPI MORI      [20,40) 0.75000000
      53 2022 FLA KEYS   S11    0   EPI MORI      [40,60) 0.25000000

