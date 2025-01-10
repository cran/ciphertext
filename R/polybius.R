#' @title polybius
#'
#' @description The polybius square is a device which associates 
#' each letter to a pair of coordinates. The letter J is excluded and replaced
#' with I in order to get 25 letters and create a 5x5 matrix.
#'
#' @param input Word or phrase to be encrypted,
#'  or character vector with the sequence of coordinate numbers if we need to decrypt
#'  
#' @inheritParams affine
#' 
#' @return a string
#' @export
#'
#' @examples
#' polybius("hello world")
#' polybius("23 15 31 31 34 52 34 42 31 14", encrypt = TRUE)
#' 
#' @references https://en.wikipedia.org/wiki/Polybius_square
#'

polybius <- function(input, encrypt = TRUE) {
  
  # prepare polybius square without "j"
  pol_letters <- subset(letters, letters != "j")
  matpol <- matrix(pol_letters, nrow = 5, byrow = TRUE)
  out <- c()
  
  if (encrypt == TRUE) {
  w0a <- PrepCyp.w(input)
  w0 <- ifelse(w0a == "j", "i", w0a) #transform j in i in input word
  
  
  for (i in 1:length(w0)) {
  out[i] <- paste(which(w0[i] == matpol, arr.ind = T), collapse = "")
   }
  }
  
  if (encrypt == FALSE) {
    coords <- unlist(strsplit(input," "))
    row <- as.numeric(substr(coords, start= 1, stop = 1))
    col <- as.numeric(substr(coords, start= 2, stop = 2))
   
    for (i in 1:length(coords)) {
      out[i] <- matpol[row[i], col[i]]
    } 
  }
  
  paste(out, collapse = " ")
  
}