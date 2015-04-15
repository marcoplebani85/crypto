#' decrypts a message
#'
#' This function decrypts a text message based on a certain reference number, called "key". The "key" needs to be shared by the sender with the recipient for him/her to be able to decrypt the message.
#' @param message An encrypted message
#' @param key An integer number
#' @keywords encryption decryption key
#' @export
#' @examples
#' decrypted.msg <- decrypt(message = ".92.37.109.26.97.37.109.53", key = 123)
#' decrypted.msg

decrypt <- function(message, key){
# FIRST, GENERATE CODE:
	code <- create.code(key)
# THEN, DECRYPT MESSAGE:
	xxx <- data.frame(encryption = as.vector(strsplit(message, "[.]")[[1]]))
	xxx$encryption <- paste(".", xxx$encryption, sep="")
	xxx$meaning <- rep(NA, length(xxx$encryption))
	xxx$meaning <- code$sign[match(xxx$encryption, code$encryption)]
	xxx <- xxx[-1,]
	decr.message <- paste(xxx$meaning, sep="", collapse="")
}