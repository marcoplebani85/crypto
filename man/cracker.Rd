% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/cracker_fun.R
\name{cracker}
\alias{cracker}
\title{cracks a secret code}
\usage{
cracker(message, keys)
}
\arguments{
\item{message}{A text message}

\item{keys}{A vector of integer numbers}
}
\description{
This function applies a brute-force approach for dechyphering an encrypted message. It tries a user-defined series of possible keys and it shows those decrypted messages in which at least one of the 21 most common english words are found.
}
\examples{
encrypted.msg <- ".92.37.109.26.97.37.109.19.26.105.109.67.37.26.67.37.26.38.26.37.118.52.5.118.105.53"
out <- cracker(message = encrypted.msg, keys = 1:200)
out
}
\keyword{code,}
\keyword{crypto,}
\keyword{cypher}
\keyword{decrypt,}
\keyword{encrypt,}

