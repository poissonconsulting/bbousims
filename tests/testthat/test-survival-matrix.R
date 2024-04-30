# Copyright 2024 Province of Alberta
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

test_that("survival process matrix", {
  rates <- c(0.84, 0.85)
  x <- bbs_matrix_survival(rates)
  expect_identical(dim(x), c(2L, 2L))
  expect_snapshot({
    print(x)
  })
})

test_that("survival process matrices", {
  # 4 seasons, 2 years, 2 stages
  rates <- array(
    c(
      0.85,
      0.84,
      0.86,
      0.87,
      0.89,
      0.83,
      0.81,
      0.85,
      0.85,
      0.84,
      0.86,
      0.87,
      0.89,
      0.83,
      0.81,
      0.85
    ),
    dim = c(4, 2, 2)
  )
  x <- bbs_matrix_survival_period(rates)
  expect_identical(dim(x), c(2L, 2L, 2L, 4L))
  expect_identical(dim(x[, , 1, 1]), c(2L, 2L))
  expect_snapshot({
    print(x)
  })
})
