#' @title print.cipher
#'
#' @description internal function with print definitions for custom cipher class
#'
#' @param x previous object
#' @param ... other arguments
#' 
#' @noRd
#' 


print.cipher <- function(x,...) {
  print.default(x$encrypted, ...)
}