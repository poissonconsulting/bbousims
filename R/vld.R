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
