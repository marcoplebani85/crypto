#' cracks a secret code
#'
#' This function applies a brute-force approach for dechyphering an encrypted message. It tries a user-defined series of possible keys and it shows those decrypted messages in which at least one of the 21 most common english words are found.
#' @param message A text message
#' @param keys A vector of integer numbers
#' @keywords crypto, encrypt, decrypt, code, cypher
#' @export
#' @examples 
#' encrypted.msg <- ".92.37.109.26.97.37.109.19.26.105.109.67.37.26.67.37.26.38.26.37.118.52.5.118.105.53"
#' out <- cracker(message = encrypted.msg, keys = 1:200)
#' out

cracker <- function(message, keys){
library(rlist)
common.words <- c("The", "A", "In", "Have", "I", "It", "He", "As", "You", "Do", "the", "be", "to", "of", "and", "a", "in", "that", "have", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at", "This", "this", "is")
List <- list()
for(i in c(keys)){
	key1 <- i
	out <- decrypt(message = message, key = key1)
	# separate words by spaces
	possible.decryption = as.vector(strsplit(out, " ")[[1]])
	# see if there is any correspondence with common.words
	correspondence <- Reduce(intersect, list(possible.decryption, common.words))
	l_out <- list(key= key1, 
		decryption = out, 
		words.found = correspondence,
		number.of.words.found = length(correspondence))
	List[[length(List)+1]] <- l_out
} # closes loop
List1 <- list.filter(List, number.of.words.found >= 1)
} # closes function