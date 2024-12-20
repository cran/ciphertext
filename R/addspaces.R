#' @title addspaces
#'
#' @description Internal function to add spaces between n letters in a string.
#' This might be helpful when printing long sequences of ciphered text, 
#'
#' @param string phrase 
#' @param L number of letters before a new space
#' 
#' @noRd
#'
#' @examples
#' addspaces("abcdefghijklm", 5)
#'


addspaces<- function(string, L) {
slice <- c()
j<- 0
for (i in seq(1, nchar(string), L)) {
  j <- j+1
  slice[j] <- substr(string, start= i, stop = i+L-1)
}

paste(slice, collapse = " ")
}
