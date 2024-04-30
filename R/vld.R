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

.vld_survival <- function(x) {
  all(
    vld_true(inherits(x, "list")),
    vld_identical(names(x), c("eSurvival", "b0", "bYear", "bAnnual", "bPeriod", "bAnnualPeriod")),
    vld_equal(length(dim(x$eSurvival)), 3)
  )
}

.vld_fecundity <- function(x) {
  all(
    vld_true(inherits(x, "list")),
    vld_identical(names(x), c("eFecundity", "b0", "bYear", "bAnnual")),
    vld_equal(length(dim(x$eFecundity)), 2)
  )
}

.vld_nyears <- function(survival, fecundity) {
  all(
    vld_equal(dim(survival$eSurvival)[2], dim(fecundity$eFecundity)[1])
  )
}
