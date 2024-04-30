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

.chk_survival <- function(x) {
  if (.vld_survival(x)) {
    return(invisible(x))
  }
  x_name <- deparse_backtick_chk(substitute(x))
  abort_chk(x_name, "must be a valid array of fecundity rates.
            See `bbs_survival_caribou()` and `bbs_survival()` for details.")
}

.chk_fecundity <- function(x) {
  if (.vld_fecundity(x)) {
    return(invisible(x))
  }
  x_name <- deparse_backtick_chk(substitute(x))
  abort_chk(x_name, "must be a valid array of fecundity rates.
            See `bbs_fecundity_caribou()` and `bbs_fecundity()` for details.")
}

.chk_nyears <- function(survival, fecundity) {
  if (.vld_nyears(survival, fecundity)) {
    return(invisible(survival))
  }
  x_name <- deparse_backtick_chk(substitute(survival))
  abort_chk(x_name, " must have the same number of years as `fecundity` object.")
}