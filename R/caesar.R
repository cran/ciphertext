#' @title caesar
#'
#' @description caesar encryption
#'
#' @param word Word or phrase to be encrypted
#' @param key numeric key
#' @param decrypt  If `FALSE` (default), the program ciphers the input word, If `TRUE`, the program decrypts it.
#' 
#' @return a string
#' @export
#'
#' @examples
#' caesar("Hello", 1)
#'

caesar <- function(word, key, decrypt = FALSE) {
  
  w0 <- PrepCyp.w(word)
  
  out <- character(length(w0))
  for (i in (1: length(w0))) {
    pos <- which(w0[i] == letters)
    newpos<- pos+ ifelse(decrypt == FALSE , key, -key)
    finalpos0 <- ifelse(newpos>length(letters), newpos%%26, newpos) # correcting values >26
    finalpos <- ifelse(finalpos0 < 1, finalpos0+length(letters), finalpos0)       # correcting values <0
    out[i] = letters[finalpos]
  }
  
  final <- paste(out, collapse = "")
  return(final)
}

  