# birth process matrix works

    Code
      print(x)
    Output
           [,1] [,2] [,3]
      [1,]    1    0  0.1
      [2,]    0    1  0.0
      [3,]    0    0  1.0

# birth process matrix can be matrix multiplied

    Code
      print(y)
    Output
           [,1]
      [1,]  110
      [2,]  100
      [3,]  100

# birth process matrix with male recruit works

    Code
      print(x)
    Output
           [,1] [,2] [,3]
      [1,]    1    0  0.2
      [2,]    0    1  0.2
      [3,]    0    0  1.0

# can change female proportion

    Code
      print(x)
    Output
           [,1] [,2] [,3]
      [1,]    1    0  0.3
      [2,]    0    1  0.0
      [3,]    0    0  1.0

# can change calf stage indices

    Code
      print(x)
    Output
           [,1] [,2] [,3]
      [1,]  1.0    0    0
      [2,]  0.3    1    0
      [3,]  0.1    0    1

# birth process matrices work

    Code
      print(x)
    Output
      , , 1
      
           [,1] [,2] [,3]
      [1,]    1    0  0.1
      [2,]    0    1  0.0
      [3,]    0    0  1.0
      
      , , 2
      
           [,1] [,2] [,3]
      [1,]    1    0 0.15
      [2,]    0    1 0.00
      [3,]    0    0 1.00
      

# works with bbs_fecundity

    Code
      print(x)
    Output
      , , 1
      
           [,1] [,2]
      [1,]    1  0.2
      [2,]    0  1.0
      
      , , 2
      
           [,1] [,2]
      [1,]    1  0.2
      [2,]    0  1.0
      
      , , 3
      
           [,1] [,2]
      [1,]    1  0.2
      [2,]    0  1.0
      
      , , 4
      
           [,1] [,2]
      [1,]    1  0.2
      [2,]    0  1.0
      
      , , 5
      
           [,1] [,2]
      [1,]    1  0.2
      [2,]    0  1.0
      

# works with bbs_fecundity_caribou

    Code
      print(x)
    Output
      , , 1
      
           [,1] [,2] [,3]
      [1,]    1    0  0.4
      [2,]    0    1  0.0
      [3,]    0    0  1.0
      
      , , 2
      
           [,1] [,2]      [,3]
      [1,]    1    0 0.4150471
      [2,]    0    1 0.0000000
      [3,]    0    0 1.0000000
      

