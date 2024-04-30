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

#' Create an age process matrix.
#'
#' @param age A vector indicating the stage to age into. If ageing does not occur, the stage should reference itself.
#'
#' @return A matrix of the age subprocess.
#' @export
#'
#' @examples
#' bbs_matrix_age(c(2, 3, 3)) %*% c(80, 50, 150)
bbs_matrix_age <- function(age = c(2, 3, 3)) {
  chk_whole_numeric(age)
  chk_range(age, range(0, length(age)))

  x <- empty_matrix(length(age))
  for (i in seq_along(age)) {
    x[age[i], i] <- 1
  }
  x
}
