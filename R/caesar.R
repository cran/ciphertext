#' @title caesar
#'
#' @description caesar encryption
#'
#' @param word Word or phrase to be encrypted
#' @param key numeric key
#' 
#' @inheritParams affine
#' 
#' @return a string
#' @export
#'
#' @examples
#' caesar("Hello", 1)
#'

caesar <- function(word, key = 1, encrypt = TRUE) {
  
  w0 <- PrepCyp.w(word)
  
  out <- character(length(w0))
  for (i in (1: length(w0))) {
    pos <- which(w0[i] == letters)
    newpos<- pos+ ifelse(encrypt == TRUE , key, -key)
    finalpos0 <- ifelse(newpos>length(letters), newpos%%26, newpos) # correcting values >26
    finalpos <- ifelse(finalpos0 < 1, finalpos0+length(letters), finalpos0)       # correcting values <0
    out[i] = letters[finalpos]
  }
  
  final <- paste(out, collapse = "")
  return(final)
}

  