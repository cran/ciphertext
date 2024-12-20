#' @title PrepCyp.w
#'
#' @description Internal function for preparing the input word (enforcing lowercase, ASCII standard, and no blanks)
#'
#' @param word Word or phrase to be preprocessed
#' @param rm.blanks Should spaces between words be removed? By default set to `TRUE`
#' 
#' @return a string
#' @noRd
#'
#' @examples
#' PrepCyp.w("HèllO Wòrld")
#' 

PrepCyp.w <- function(word, rm.blanks = TRUE) {
  w0a <-  ifelse(rm.blanks == TRUE, gsub(" ", "", word), word) # Remove blanks 
  w0b <-  tolower(w0a)                                # Everything lowercase
  w0c <- iconv(w0b,to="ASCII//TRANSLIT")             # Remove strange characters and accents
  w0 <- unlist(strsplit(w0c, ""))                    # split in letters
  return(w0)
}
