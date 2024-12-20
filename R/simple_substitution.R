#' @title simple_substitution
#'
#' @description simple substitution cipher. Each letter is monoalphabetically associated with a different one used for the encryption.
#'
#' @param word Word or phrase to be encrypted
#' @param key Word to be used as key for the encryption. If not provided, a random shuffle is performed
#' @param seed Seed for reproducibility of the encryption if key is not provided
#' 
#' @return a list with custom class "cipher", which modifies the printing defaults.
#'         The list contains the initial phrase (initial), the ciphered output (encrypted),
#'          and the alphabet order (keyalphabet)
#' @export
#' 
#' @examples
#' simple_substitution("hello world", seed = 1234)
#' simple_substitution("hello world", key = "zebras")
#'

simple_substitution <- function(word, key = "", seed = sample(1:1000,1)) {
  
  w0 <- tolower(unlist(strsplit(word, "")))
  w1 <- c()
  
  if (key != "") {
    key0 <- PrepCyp.w(key)
    shuffledletters <- unique(c(key0, letters))
  } else {
    set.seed(seed)
    shuffledletters <- sample(letters)
  }
  
  
  for (i in (1:length(w0))) {
    w1[i] <- ifelse(w0[i] %in% letters,
                    shuffledletters[which(w0[i]==letters)], " ")
  }
  
  final <- paste(w1, collapse = "")
  out <- list(initial=word, 
              encrypted=final, 
              keyalphabet = shuffledletters)
  class(out) <- "cipher"
  return(out)
}
