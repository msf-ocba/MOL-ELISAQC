#' Extract the end of a string by n characters
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function extracts the last n letters of a character string. 
#' 
#' @param x character string
#' @param n number of letters to extract from the end (numeric value)
#' 
#' @returns the last n letters as a character (sub) string 
#' 
#' @examples 
#'  # Extract the last 3 letters of the word marmalade:
#'  endstring(x = "marmalade", n = 3)
#' @export
endstring = function(x,n){
  substring(x, nchar(x)-n+1)
}