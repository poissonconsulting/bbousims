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
       3     1     3 A                      30                  1                    0
       4     1     4 A                      29                  0                    0
       5     1     5 A                      29                  0                    0
       6     1     6 A                      29                  1                    0
       7     1     7 A                      28                  0                    0
       8     1     8 A                      28                  0                    0
       9     1     9 A                      28                  2                    0
      10     1    10 A                      26                  0                    0
      # i 50 more rows
      
      [[1]]$recruitment
      # A tibble: 285 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     1     1         3      0             0
       2     1     9 A                  1     1     0         2      4             0
       3     1     9 A                  1     2     1         1      0             0
       4     1     9 A                  1     3     0         0      0             0
       5     1     9 A                  1     3     0         2      3             0
       6     1     9 A                  1     2     0         0      2             0
       7     1     9 A                  1     3     4         2      0             0
       8     1     9 A                  1     2     0         2      0             0
       9     1     9 A                  1     4     1         0      2             0
      10     1     9 A                  1     1     2         2      2             0
      # i 275 more rows
      
      [[1]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       177
       2     1     1      1 A              Female Calf       167
       3     1     2      2 A              Female Calf       156
       4     1     3      3 A              Female Calf       149
       5     1     4      4 A              Female Calf       141
       6     1     5      5 A              Female Calf       136
       7     1     6      6 A              Female Calf       129
       8     1     7      7 A              Female Calf       123
       9     1     8      8 A              Female Calf       115
      10     1     9      9 A              Female Calf       109
      # i 356 more rows
      
      
      [[2]]
      [[2]]$survival
      # A tibble: 60 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  0                    0
       2     1     2 A                      30                  0                    0
       3     1     3 A                      30                  0                    0
       4     1     4 A                      30                  0                    0
       5     1     5 A                      30                  0                    0
       6     1     6 A                      30                  1                    0
       7     1     7 A                      29                  0                    0
       8     1     8 A                      29                  1                    0
       9     1     9 A                      28                  1                    0
      10     1    10 A                      27                  1                    0
      # i 50 more rows
      
      [[2]]$recruitment
      # A tibble: 288 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     1     1         1      0             0
       2     1     9 A                  1     2     1         0      2             0
       3     1     9 A                  1     3     2         0      2             0
       4     1     9 A                  1     1     1         2      1             0
       5     1     9 A                  1     2     1         0      0             0
       6     1     9 A                  1     2     2         2      1             0
       7     1     9 A                  1     2     2         0      2             0
       8     1     9 A                  1     4     1         0      1             0
       9     1     9 A                  1     3     3         0      1             0
      10     1     9 A                  1     1     2         3      2             0
      # i 278 more rows
      
      [[2]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       177
       2     1     1      1 A              Female Calf       171
       3     1     2      2 A              Female Calf       160
       4     1     3      3 A              Female Calf       156
       5     1     4      4 A              Female Calf       152
       6     1     5      5 A              Female Calf       147
       7     1     6      6 A              Female Calf       135
       8     1     7      7 A              Female Calf       130
       9     1     8      8 A              Female Calf       125
      10     1     9      9 A              Female Calf       119
      # i 356 more rows
      
      
      [[3]]
      [[3]]$survival
      # A tibble: 60 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  2                    0
       2     1     2 A                      28                  1                    0
       3     1     3 A                      27                  2                    0
       4     1     4 A                      25                  0                    0
       5     1     5 A                      25                  0                    0
       6     1     6 A                      25                  0                    0
       7     1     7 A                      25                  0                    0
       8     1     8 A                      25                  0                    0
       9     1     9 A                      25                  0                    0
      10     1    10 A                      25                  0                    0
      # i 50 more rows
      
      [[3]]$recruitment
      # A tibble: 276 x 9
          Year Month PopulationName   Day  Cows Bulls Yearlings Calves UnknownAdults
         <int> <int> <chr>          <dbl> <int> <int>     <int>  <int>         <int>
       1     1     9 A                  1     1     2         0      1             0
       2     1     9 A                  1     1     0         3      3             0
       3     1     9 A                  1     1     2         1      1             0
       4     1     9 A                  1     2     1         0      0             0
       5     1     9 A                  1     2     3         1      0             0
       6     1     9 A                  1     2     3         1      2             0
       7     1     9 A                  1     2     0         1      3             0
       8     1     9 A                  1     1     1         0      2             0
       9     1     9 A                  1     3     0         0      1             0
      10     1     9 A                  1     0     2         0      2             0
      # i 266 more rows
      
      [[3]]$abundance
      # A tibble: 366 x 6
          Year Month Period PopulationName Stage       Abundance
         <dbl> <dbl>  <dbl> <chr>          <fct>           <dbl>
       1     0    12      0 A              Female Calf       177
       2     1     1      1 A              Female Calf       170
       3     1     2      2 A              Female Calf       156
       4     1     3      3 A              Female Calf       145
       5     1     4      4 A              Female Calf       138
       6     1     5      5 A              Female Calf       131
       7     1     6      6 A              Female Calf       122
       8     1     7      7 A              Female Calf       114
       9     1     8      8 A              Female Calf       109
      10     1     9      9 A              Female Calf       106
      # i 356 more rows
      
      

