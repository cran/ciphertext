#' @title railfence
#'
#' @description The rail fence is a transposition cipher where the text is 
#' written upwards and downwards diagonally (zigzag) on the rails of the fence
#'
#' @param word Word or phrase to be encrypted
#' @param key numeric key (number of rails)
#' 
#' @return a string
#' @export
#'
#' @examples
#' railfence('we are discovered flee at once',3)
#'
#' @references https://en.wikipedia.org/wiki/Rail_fence_cipher
#'

railfence <- function(word, key) {
  w0 <- PrepCyp.w(word)
  
  length(w0)
  
  mat <- matrix("",nrow = length(w0), ncol = key)
  mat <- t(mat)
  
  j<-1
  aim <- key
  for (i in (1: length(w0))) {
    mat[j,i] = w0[i]
    aim <- ifelse(j == key, 1,
                  ifelse(j==aim, key, aim))
    j<- ifelse(aim == key, j+1, j-1)
  }
  
  final<- paste(t(mat), collapse = "")
  return(final)
}