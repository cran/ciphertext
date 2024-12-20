#' @title vigenere
#'
#' @description Vigenère cipher is a method of encrypting alphabetic text where each letter of
#'  the plaintext is encoded with a different Caesar cipher, whose increment is determined by the corresponding letter the key
#'
#' @param word Word or phrase to be encrypted
#' @param key character key
#' @param decrypt  If `FALSE` (default), the program ciphers the input word, If `TRUE`, the program decrypts it.
#' 
#' @return a string
#' @export
#'
#' @examples
#' vigenere("hello world", "opla")
#'
#' @references https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
#'

vigenere <- function(word, key, decrypt = FALSE) {
  
  w0 <- PrepCyp.w(word)
  k0 <- PrepCyp.w(key)
  k1 <- c()
  
  # create key of equal length of the phrase
  while (length(w0) > length(k1)) {
    k1 <- c(k1,k0)
  }
  k1 <- k1[1:length(w0)]
  
  
  out <- character(length(w0))
  for (i in (1: length(w0))) {
    pos <- which(w0[i] == letters)
    knum <- which(k1[i] == letters) - 1
    newpos<- pos + ifelse(decrypt == FALSE , knum, -knum)
    
    finalpos0 <- ifelse(newpos>length(letters), newpos - length(letters), newpos) # correcting values >26
    finalpos <- ifelse(finalpos0 < 1, finalpos0+length(letters), finalpos0)       # correcting values <0
    out[i] = letters[finalpos]
  }
  
  final <- paste(out, collapse = "")
  return(final)
}
