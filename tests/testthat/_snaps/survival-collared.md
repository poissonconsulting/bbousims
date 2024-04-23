# survival collared works

    Code
      print(x)
    Output
      # A tibble: 120 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  0                    0
       2     1     2 A                      30                  0                    0
       3     1     3 A                      30                  0                    0
       4     1     4 A                      30                  1                    0
       5     1     5 A                      29                  0                    0
       6     1     6 A                      29                  0                    0
       7     1     7 A                      29                  0                    0
       8     1     8 A                      29                  0                    0
       9     1     9 A                      29                  0                    0
      10     1    10 A                      29                  0                    0
      # i 110 more rows

# survival collared works with different collar month

    Code
      print(x)
    Output
      # A tibble: 118 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     3 A                      30                  0                    0
       2     1     4 A                      30                  0                    0
       3     1     5 A                      30                  0                    0
       4     1     6 A                      30                  1                    0
       5     1     7 A                      29                  0                    0
       6     1     8 A                      29                  0                    0
       7     1     9 A                      29                  0                    0
       8     1    10 A                      29                  0                    0
       9     1    11 A                      29                  0                    0
      10     1    12 A                      29                  0                    0
      # i 108 more rows

# survival collared works with uncertain mort

    Code
      print(x)
    Output
      # A tibble: 120 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  0                    0
       2     1     2 A                      30                  0                    0
       3     1     3 A                      30                  0                    0
       4     1     4 A                      30                  1                    0
       5     1     5 A                      29                  0                    0
       6     1     6 A                      29                  0                    0
       7     1     7 A                      29                  0                    0
       8     1     8 A                      29                  0                    0
       9     1     9 A                      29                  0                    0
      10     1    10 A                      29                  0                    0
      # i 110 more rows

# survival collared low survival works

    Code
      print(x)
    Output
      # A tibble: 120 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                      30                  5                    0
       2     1     2 A                      25                  4                    0
       3     1     3 A                      21                  1                    0
       4     1     4 A                      20                  2                    0
       5     1     5 A                      18                  4                    0
       6     1     6 A                      14                  3                    0
       7     1     7 A                      11                  0                    0
       8     1     8 A                      11                  2                    0
       9     1     9 A                       9                  1                    0
      10     1    10 A                       8                  1                    0
      # i 110 more rows

# survival collared low collars works

    Code
      print(x)
    Output
      # A tibble: 120 x 6
          Year Month PopulationName StartTotal MortalitiesCertain MortalitiesUncertain
         <int> <int> <chr>               <dbl>              <int>                <int>
       1     1     1 A                       5                  1                    0
       2     1     2 A                       4                  0                    0
       3     1     3 A                       4                  1                    0
       4     1     4 A                       3                  0                    0
       5     1     5 A                       3                  0                    0
       6     1     6 A                       3                  0                    0
       7     1     7 A                       3                  1                    0
       8     1     8 A                       2                  0                    0
       9     1     9 A                       2                  0                    0
      10     1    10 A                       2                  0                    0
      # i 110 more rows

