# https://www.r-bloggers.com/2016/08/rcpp-and-roxygen2/
# https://stackoverflow.com/questions/41051003/rcpp-c-functions-dont-work-in-r-package
# This silly zzz.R file basically just handles the things needed for Rcpp, and is
# titled this way so it is sourced in alphabetical order. I did this years
# ago for another R project but I'm not sure if it's still necessary.
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib ensembleR
#' @name ensembleR
NULL
