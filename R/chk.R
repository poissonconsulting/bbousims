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
