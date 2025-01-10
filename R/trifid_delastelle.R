#' @title trifid_delastelle
#'
#' @description The trifid cipher is an encryption method that uses a 3-dimensional grid 
#' It was invented by Felix Delastelle in 1902. As a 3x3x3 grid is used, 27 character are needed.
#' Thus, we use all the 26 alphabet letter and add the "+" sign at the bottom.
#' 
#' @param input Word or phrase to be encrypted,
#'  or character vector with the sequence of coordinate numbers if we need to decrypt
#' @param key key Word for creating the modified Polybius square
#' @param period period length for splitting the input phrase. If greater or equal to the length of the input then the split is not executed
#' 
#' @inheritParams affine
#' 
#' @return a string
#' @export
#' 
#' @examples
#' trifid_delastelle("secret", key = "", period = 5, encrypt = TRUE)
#' trifid_delastelle("sjlkzt", key = "", period = 5, encrypt = FALSE)
#' 
#' @references https://en.wikipedia.org/wiki/Trifid_cipher
#'

trifid_delastelle <- function(input, key = "", period = 100, encrypt = TRUE) {

w0 <- PrepCyp.w(input)

# allow for block periods (set period >= length(w0) to avoid creating blocks)
splitw0 <- c()
for (k in seq(1, length(w0), by = period)) {
  split<- w0[k:(k+period-1)]
  splitw0 <- rbind(splitw0, split)
}

# Allow j and add a plus sign as 27th character
key0 <- PrepCyp.w(key)

shuffledletters <- unique(c(key0, letters, "+" ))
matpol <- aperm(array(shuffledletters, dim = c(3,3,3)), perm = c(2,1,3))

allblocks <- c()

for (w in c(1:nrow(splitw0))) { #process one block at a time
  
  w0 <- splitw0[w,]  # focus on the current block
  
  coordinit <- c()
  
  for (i in 1:length(w0)) {
    coordinit_temp <- which(w0[i] == matpol, arr.ind = T)
    coordinit <- rbind(coordinit, coordinit_temp) # create dataframe with initial coordinates
  }
  
  colnames(coordinit) <- c("row", "column", "grid")
  coordinit <- coordinit[,c("grid", "row", "column")]
  
  coordorder<- if(encrypt == TRUE) {as.numeric(coordinit)} else {as.numeric(t(coordinit))} #encryption/decryption defined here
  
  coordfin <- as.data.frame(matrix(coordorder, ncol = 3, byrow = isTRUE(encrypt))) # final coordinates, byrow is TRUE if encrypt, FALSE if decrypt
  colnames(coordfin) <- c("grid", "row", "column")
  out<- c()
  
  for (i in 1:nrow(coordfin)) {
    out_temp<- matpol[coordfin$row[i],coordfin$column[i], coordfin$grid[i]] # extract letters from coordinates
    out <- c(out, out_temp)
  }
  
  allblocks[w] <- paste(out, collapse = "") #save each transformed block
  
}

paste(allblocks, collapse = "") #paste together all blocks

}
