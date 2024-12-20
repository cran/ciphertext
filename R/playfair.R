#' @title playfair
#'
#' @description The Playfair cipher is a symmetric method which encrypts pairs of letters
#'  using a modified Polybius square
#'
#' @param word Word or phrase to be encrypted or decrypted
#' @param key Word for creating the modified Polybius square
#' @param added_letter Letter to be added in case two letters of a pair are identical; usually "x" is used
#' @param decrypt  If `FALSE` (default), the program ciphers the input word, If `TRUE`, the program decrypts it.
#' 
#' @return a string
#' @export
#'
#' @examples
#' playfair( "instruments", "monarchy", added_letter = "z")
#' playfair("gatlmzclrqtx", "monarchy", added_letter = "z", decrypt = TRUE)
#' 
#' @references https://en.wikipedia.org/wiki/Playfair_cipher
#'

playfair <- function(word, key, added_letter = "x", decrypt = FALSE) {
  # Exclude j from both letter and alphabet
  key0j <- PrepCyp.w(key)
  
  key0 <- ifelse(key0j == "j", "i", key0j)
  letters0 <- subset(letters, letters != "j")
  
  shuffledletters <- unique(c(key0, letters0))
  
  matpol <- matrix(shuffledletters, nrow = 5, byrow = TRUE)
  
  
  # consider the word to be encrypted, and consider pairs
  w0j <- PrepCyp.w(word)
  w0<- ifelse(w0j == "j", "i", w0j)
  
  
  ## Look at the pairs, and insert X if needed to separate identical letters in a pair
  j<- 1
  while(j < length(w0)) {
    if (w0[j] == w0[j+1]) {w0 <- append(w0, added_letter, after = j)}
    j <- j+2
  }  
  # Add one X at the end if number of letters is uneven
  if (length(w0) %%2 != 0) {
    w0<- c(w0, added_letter)
  }
  
  out<- c()
  i<- 1
  while(i < length(w0)) {
  # extract initial coordinates of the pair
  l1 <- as.data.frame(which(w0[i] == matpol, arr.ind = T)) 
  l2 <- as.data.frame(which(w0[i+1] == matpol, arr.ind = T))
  
  new1 <- c()
  new2<- c()
  # Machinery for new coordinates new1 and new2
  if (l1$row != l2$row & l1$col != l2$col) {
    new1$row <- l1$row
    new1$col <- l2$col
    new2$row <- l2$row
    new2$col <- l1$col
  } else  if (l1$row == l2$row) {
    new1$row <- new2$row <- l1$row
    new1$col <- ifelse(decrypt == FALSE, (l1$col%%5) +1, (l1$col-1) + 5*(l1$col==1))
    new2$col <- ifelse(decrypt == FALSE, (l2$col%%5) +1, (l2$col-1) + 5*(l2$col==1))
  } else if (l1$col == l2$col) {
    new1$col <- new2$col <- l1$col
    new1$row <- ifelse(decrypt == FALSE, (l1$row%%5) +1, (l1$row-1) + 5*(l1$row==1))
    new2$row <- ifelse(decrypt == FALSE, (l2$row%%5) +1, (l2$row-1) + 5*(l2$row==1))
  }
  
  out[i]   <- matpol[new1$row, new1$col]
  out[i+1] <- matpol[new2$row, new2$col] 
  
  i<- i+2
  }
  
  f0<-paste(out, collapse = "")
  addspaces(f0, 5)
}
