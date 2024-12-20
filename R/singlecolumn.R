#' @title singlecolumn
#'
#' @description In a columnar transposition cipher, the message is written out in rows of a fixed length,
#'  and then read out again column by column. The order of the column follows the alphabetcial order of the letters present in the key
#'
#' @param word Word or phrase to be encrypted
#' @param key word key: for example, the key "bcea" suggests that the column order is "2-3-4-1"
#' @param rm.blanks Should spaces between words be removed? By default set to `TRUE`
#' 
#' @return a string
#' @export
#'
#' @examples
#' singlecolumn("This is wikipedia", "cipher")
#'
#' @references https://www.geeksforgeeks.org/columnar-transposition-cipher/
#'

singlecolumn <- function(word, key, rm.blanks = TRUE) {
  
  w0 <- PrepCyp.w(word, rm.blanks = rm.blanks)
  
  k2 <- unique(unlist(strsplit(key,"")))
  k3 <- numeric(length(k2))
  
  for  (i in (1:length(k2))) {
    k3[i] <- which(k2[i] == sort(k2))
  }
  
  mat <- matrix("",nrow = length(k2), ncol = ceiling(length(w0)/length(k2)))
  mat[1:length(w0)] <- w0
  mat <- t(mat)
  
  out <- c()
  for (i in (1:length(k3))) {
    j <- which(i == k3)
    out <- c(out, paste(mat[,j]))
  }
  
  final <- paste(out, collapse = "")
  return(final)
}
