% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polybius.R
\name{polybius}
\alias{polybius}
\title{polybius}
\usage{
polybius(input, encrypt = TRUE)
}
\arguments{
\item{input}{Word or phrase to be encrypted,
or character vector with the sequence of coordinate numbers if we need to decrypt}

\item{encrypt}{If `TRUE` (default), the program ciphers the input word, If `FALSE`, the program decrypts it.}
}
\value{
a string
}
\description{
The polybius square is a device which associates 
each letter to a pair of coordinates. The letter J is excluded and replaced
with I in order to get 25 letters and create a 5x5 matrix.
}
\examples{
polybius("hello world")
polybius("23 15 31 31 34 52 34 42 31 14", encrypt = TRUE)

}
\references{
https://en.wikipedia.org/wiki/Polybius_square
}
