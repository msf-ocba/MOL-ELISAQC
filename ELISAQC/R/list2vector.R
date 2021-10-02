#' Convert lists in a vector to their unlisted values
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function takes a vector containing lists with single values in each 
#' slot as input, unlists them and returns the vector with the individual 
#' values instead. 
#' 
#' @note 
#' 1. Do not attempt to use this function with lapply as it will crash R!
#' 2. Do not use on lists with more than one value; this will generate an error.
#'  
#' @param dtcol vector containing lists to unlist
#' 
#' @returns the vector with sub-lists converted to the values they contain 
#' 
#' @examples 
#'  \dontrun{
#'  # Convert a vector containing sub-lists to the unlisted values:
#'  cases[, qanswers.sampledate := list2vector(qanswers.sampledate)]
#'  }
#' @export
list2vector <- function(dtcol){
  dtcol = unname(unlist(lapply(dtcol, "[", "value")))
  return(dtcol)
}
