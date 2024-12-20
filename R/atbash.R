#' @title atbash
#'
#' @description The Atbash cipher is a type of monoalphabetic cipher which takes
#'  the alphabet and maps it to its reverse. 
#'  It is a particular case of the affine cipher, with `a`=`b`= (`m`-1). As `m` is the number
#'  of letters and is equal to 26, it means that `a` = `b` = 25.
#'  Encrypting and decrypting are not separate for this cipher.
#'
#' @param word Word or phrase to be encrypted
#' 
#' @return a string
#' @export
#'
#' @examples
#' atbash("abcxyz")
#' 
#' @references https://en.wikipedia.org/wiki/Atbash
#'

atbash <- function(word) {
  affine(word, a=25, b = 25)
}
