#' \code{airportr} package
#'
#' Package to work with airport data
#'
#' See the README on \href{https://github.com/dshkol/airportr/blob/master/README.md}{GitHub}
#'
#' @docType package
#' @name airportr
#' @importFrom dplyr %>%
#' @importFrom utils data
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
