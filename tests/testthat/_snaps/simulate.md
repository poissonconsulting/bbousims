# bbs_simulate_caribou works

    Code
      print(x)
    Output
      [[1]]
      [[1]]$survival
      # A tibble: 60 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  0                    0
       2     1     2 A                      30                  0                    0
       3     1     3 A                      30                  0                    0
       4     1     4 A                      30                  1                    0
       5     1     5 A                      29                  1                    0
       6     1     6 A                      28                  0                    0
       7     1     7 A                      28                  0                    0
       8     1     8 A                      28                  0                    0
       9     1     9 A                      28                  2                    0
      10     1    10 A                      26                  0                    0
      # i 50 more rows
      
      [[1]]$recruitment
      # A tibble: 577 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     2     4         0      0             0
       2     1     9 A                  1     2     2         0      1             0
       3     1     9 A                  1     2     0         1      3             0
       4     1     9 A                  1     4     0         0      1             0
       5     1     9 A                  1     1     0         0      2             0
       6     1     9 A                  1     2     1         1      0             0
       7     1     9 A                  1     1     2         1      1             0
       8     1     9 A                  1     2     2         1      1             0
       9     1     9 A                  1     1     0         1      1             0
      10     1     9 A                  1     3     0         1      0             0
      # i 567 more rows
      
      [[1]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       354
       2     1     1      1 A              Female Calf       334
       3     1     2      2 A              Female Calf       313
       4     1     3      3 A              Female Calf       298
       5     1     4      4 A              Female Calf       282
       6     1     5      5 A              Female Calf       271
       7     1     6      6 A              Female Calf       257
       8     1     7      7 A              Female Calf       244
       9     1     8      8 A              Female Calf       229
      10     1     9      9 A              Female Calf       217
      # i 356 more rows
      
      
      [[2]]
      [[2]]$survival
      # A tibble: 60 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  1                    0
       2     1     2 A                      29                  1                    0
       3     1     3 A                      28                  1                    0
       4     1     4 A                      27                  1                    0
       5     1     5 A                      26                  1                    0
       6     1     6 A                      25                  1                    0
       7     1     7 A                      24                  0                    0
       8     1     8 A                      24                  0                    0
       9     1     9 A                      24                  1                    0
      10     1    10 A                      23                  0                    0
      # i 50 more rows
      
      [[2]]$recruitment
      # A tibble: 553 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     3     1         2      0             0
       2     1     9 A                  1     2     1         0      0             0
       3     1     9 A                  1     4     1         0      2             0
       4     1     9 A                  1     2     0         2      0             0
       5     1     9 A                  1     2     2         1      0             0
       6     1     9 A                  1     1     2         0      1             0
       7     1     9 A                  1     2     3         2      0             0
       8     1     9 A                  1     2     1         1      0             0
       9     1     9 A                  1     1     2         0      1             0
      10     1     9 A                  1     3     2         0      1             0
      # i 543 more rows
      
      [[2]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       354
       2     1     1      1 A              Female Calf       323
       3     1     2      2 A              Female Calf       314
       4     1     3      3 A              Female Calf       296
       5     1     4      4 A              Female Calf       286
       6     1     5      5 A              Female Calf       263
       7     1     6      6 A              Female Calf       253
       8     1     7      7 A              Female Calf       244
       9     1     8      8 A              Female Calf       227
      10     1     9      9 A              Female Calf       215
      # i 356 more rows
      
      
      [[3]]
      [[3]]$survival
      # A tibble: 60 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  0                    0
       2     1     2 A                      30                  0                    0
       3     1     3 A                      30                  1                    0
       4     1     4 A                      29                  1                    0
       5     1     5 A                      28                  0                    0
       6     1     6 A                      28                  0                    0
       7     1     7 A                      28                  0                    0
       8     1     8 A                      28                  0                    0
       9     1     9 A                      28                  0                    0
      10     1    10 A                      28                  0                    0
      # i 50 more rows
      
      [[3]]$recruitment
      # A tibble: 569 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     3     1         1      1             0
       2     1     9 A                  1     0     3         1      0             0
       3     1     9 A                  1     0     1         1      2             0
       4     1     9 A                  1     1     0         1      4             0
       5     1     9 A                  1     3     0         1      0             0
       6     1     9 A                  1     2     1         1      2             0
       7     1     9 A                  1     3     0         0      4             0
       8     1     9 A                  1     5     1         1      1             0
       9     1     9 A                  1     4     0         1      0             0
      10     1     9 A                  1     3     1         0      1             0
      # i 559 more rows
      
      [[3]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       354
       2     1     1      1 A              Female Calf       336
       3     1     2      2 A              Female Calf       320
       4     1     3      3 A              Female Calf       299
       5     1     4      4 A              Female Calf       287
       6     1     5      5 A              Female Calf       270
       7     1     6      6 A              Female Calf       257
       8     1     7      7 A              Female Calf       245
       9     1     8      8 A              Female Calf       229
      10     1     9      9 A              Female Calf       213
      # i 356 more rows
      
      

