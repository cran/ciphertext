#' @title scytale
#'
#' @description The Scytale is a transposition cipher
#'  The diameter of the Scytale (the number of turns) can be regarded as the key of the cipher.
#'
#' @param word Word or phrase to be encrypted or decrypted
#' @param key Number of turns of the band
#' 
#' @inheritParams affine
#' 
#' @return a string
#' @export
#'
#' @examples
#' scytale('we are discovered flee at once',3)
#'
#' @references https://en.wikipedia.org/wiki/Scytale
#'

scytale <- function(word, key = 3, encrypt = TRUE) {
  w0 <- PrepCyp.w(word)
  
  length(w0)
  
  mat <- matrix("",nrow = length(w0), ncol = key)
  mat <- t(mat)
  
  if (encrypt == TRUE) {
    
    j<-1
    for (i in (1: length(w0))) {
      mat[j,i] = w0[i]
      j<- ifelse(j == key, 1, j+1)
    }
    
    final<- paste(t(mat), collapse = "")
  }
  
  if (encrypt == FALSE) {
  j<-1
  for (i in (1: length(w0))) {
    mat[j,i] = "X"#w0[i]
    j<- ifelse(j == key, 1, j+1)
  }
  
  i <- 1
  for (k in (1:nrow(mat))) {
    m <- 1
    while (m <= ncol(mat)) {
    if(mat[k,m] == "X") {
      mat[k,m] = w0[i]
      i <- i+1
    }
    m <- m+1
    }
  }
  
  final<- paste(mat, collapse = "")
  }
  
  return(final)
}
