#' encrypts a message
#'
#' This function encrypts a text message based on a certain reference number, called "key". The "key" needs to be shared with the recipient for him/her to be able to decrypt the message.
#' @param message A text message
#' @param key An integer number
#' @keywords crypto, encrypt
#' @export
#' @examples 
#' encrypted.msg <- encrypt(message = "Ush ush.", key=123)
#' encrypted.msg

encrypt <- function(message, key){
# FIRST, CREATE CODE:
	code <- create.code(key)
# THEN, ENCRYPT MESSAGE:
	xxx <- data.frame(sign = as.vector(strsplit(message, NULL)[[1]]))
	xxx$encryption <- rep(NA, length(xxx$sign))
	xxx$encryption <- code$encryption[match(xxx$sign, code$sign)]
	encr.message <- paste(xxx$encryption, sep="", collapse="")
}