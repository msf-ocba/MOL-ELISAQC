#' Calculate coefficient of variation as a percentage
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function takes a vector of values and returns the coefficient of 
#' variation as a percentage. 
#' 
#' @param values vector of values to calculate the cv for
#' 
#' @returns value (coefficient of variation as a percentage) 
#' 
#' @examples 
#'  # Create vector of values:
#'  x <- c(2.19, 2.32)
#'  y <- c(0.50, 1.50)
#'  
#'  # Calculate cvs:
#'  xcv <- calculate_cv(x)
#'  ycv <- calculate_cv(y)
#'  
#' @export
calculate_cv <- function(values){
  
  # Calculate standard deviation:
  stdev = sd(values)
  
  # Calculate mean:
  avg = mean(values)
  
  # Calculate % coefficient of variation:
  cvres  = round((stdev / avg) * 100, 2)
  
  # Return result:
  return(cvres)
  
}