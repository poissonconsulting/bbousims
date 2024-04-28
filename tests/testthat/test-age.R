test_that("age process matrix works", {
  age <- c(2, 3, 3)
  x <- bbs_matrix_age(age)
  expect_snapshot({
    print(x)
  })
})

test_that("age process matrix can be multiplied", {
  age <- c(2, 3, 3)
  x <- bbs_matrix_age(age)
  y <- x %*% c(100, 100, 100)
  expect_snapshot({
    print(y)
  })
})
