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
