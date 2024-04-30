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

#' @export
print.bbou_population <- function(x, ...) {
  attr(x, "fecundity") <- NULL
  attr(x, "survival") <- NULL
  attr(x, "class") <- NULL
  print.default(x)
}

#' @export
print.bbou_population_caribou <- function(x, ...) {
  attr(x, "fecundity") <- NULL
  attr(x, "survival") <- NULL
  attr(x, "class") <- NULL
  print.default(x)
}

#' @export
print.bbou_simulation <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x)
}
