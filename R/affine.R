#' @title affine
#'
#' @description The affine cipher is a monoalphabetic substitutoin cipher,
#' where each letter is enciphered with the function (ax+b) mod 26 
#' (26 is the number of letters in the alphabet)
#'
#' @param word Word or phrase to be encrypted
#' @param a First parameter. This value and 26 must be coprime
#' @param b Second parameter. Magnitude of the shift
#' @param encrypt If `TRUE` (default), the program ciphers the input word, If `FALSE`, the program decrypts it.
#' 
#' @return a string
#' @export
#'
#' @examples
#' affine("Hello", 1, -1)
#' 
#' @references https://en.wikipedia.org/wiki/Affine_cipher
#'

affine <- function(word, a, b, encrypt = TRUE) {
  
  # Check if a,x are coprime
  
  divisors_a <- seq_len(a)[a %% seq_len(a) == 0]
  divisors_26 <- seq_len(26)[26 %% seq_len(26) == 0]
  divisors <- divisors_a[divisors_a %in% divisors_26]
  
  if (length(divisors) > 1) {
    warning("a and 26 are not coprime, the substitution is not unique")
  } 
  
  # start procedure
  
  w0 <- PrepCyp.w(word)
  
  out <- character(length(w0))
  for (i in (1: length(w0))) {
    pos <- which(w0[i] == letters)-1
    newpos<- ifelse(encrypt == TRUE, (a*pos + b)%%26, ((pos-b)/a)%%26)

    out[i] = letters[newpos+1]
  }
  
  final <- paste(out, collapse = "")
  return(final)
}
