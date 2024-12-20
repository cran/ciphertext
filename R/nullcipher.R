#' @title nullcipher
#'
#' @description A null cipher is an encryption method where the plaintext is mixed with a
#'  large amount of non-cipher material (decoy). 
#'
#' @param phrase Word or phrase to be decrypted
#' @param index letter of interest for each word in the phrase. Also a pattern vector can be entered.
#' @param decrypt Only Decryption is possible for now, but will be updated in the future
#' 
#' @return a string
#' @export
#'
#' @examples
#' nullcipher("handy set false posts", c(1,2,3))
#'
#' @references https://en.wikipedia.org/wiki/Null_cipher
#'

nullcipher <- function(phrase, index,  decrypt = TRUE) {
  
  terms <- unlist(strsplit(phrase,"[[:space:]]"))
  
  # indexpattern assigns to each word the character number to be taken
  # if the index is only 1 (for example, take always the first character)
  # then this section is redundant, but if a pattern is required (e.g., take
  # the first character for the firs, the second for the second, ...) then this is useful
  nrep <- ceiling(length(terms) / length(index))
  indexpattern <- rep(index, nrep)
  length(indexpattern) <- length(terms)
  
  out<- character()
  
  for (i in 1:length(terms)) {
    out[i] <- substr(terms[i],
                     start= indexpattern[i],
                     stop = indexpattern[i])
  }
  
  paste(out, collapse = "")
}
