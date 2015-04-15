#' creates a secret code
#'
#' This function creates the code for decyphering the encryption system, consisting in a dataset with alpha-numerical values and the corresponding encryption. Different keys produce different codes.
#' @param key An integer number
#' @keywords encryption decryption key code
#' @export
#' @examples
#' code1 <- create.code(key = 123)
#' code1

create.code <- function(key){
	set.seed(key)
	sign <- c(letters, LETTERS, 0:9, 
	" ", ",", ".", ";", ":", "/", "%","=",
	"*","+","-","_","€","£","$","&","(",")","'",
	"ì","ò","à","ù","è","é","@","°", "!","?","#",
	"|", "<", ">","«","»", "¥", "~", "≠", "•", "√", "ç", "Ç", 
	"À", "È", "Ì", "Ò", "Æ", "Ù", "Œ", "{", "}","[","]",
	"Å","Á", "É", "Í", "Ó", "Ú","æ","œ","·", "ø", "å", "Ø", "¿", "¡")
	encr <- c(sample(1:length(sign), length(sign), replace=F))
	encryption <- paste(".", encr, sep="")
	code <- data.frame(sign=sign, encryption=encryption)
	code <- code[with(code, order(sign)), ]
}