PrepCyp.w("çìAò ai NON asci agli spazi e alle MAIUSCOLE")

caesar("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG", 23, encrypt = TRUE)

vigenere("geeks for geeks", "ayush")
vigenere("gcyczfmlyleim", "ayush", encrypt = FALSE)

ssub<- simple_substitution("flee at once we are discovered", "zebras")
class(ssub) # class cipher
ssub$initial
ssub$encrypted
ssub # by default, only the encrypted is print

railfence('we are discovered flee at once',3)

singlecolumn("Geeks for Geeks", "hack", rm.blanks = FALSE)
singlecolumn("THIS IS WIKIPEDIA", "cipher", rm.blanks = TRUE)

