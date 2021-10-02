#' Extract the date-time stamp from a raw ELISA results file
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function searches for and extracts the date-time stamp at which an 
#' ELISA assay was read from a raw ELISA results file.
#' It uses a regular expression to find the character string containing the 
#' date-time stamp; this string can include other text before the date.
#' The lubridate package is used to parse the dates - currently only European 
#' and ISO date formats are included. 
#' 
#' @note 
#' This function has not been tested extensively and may fail to identify some
#' date-time formats.   
#' 
#' Pre-requisites:
#'  * ELISA reader raw results file for a 96-well plate format
#'  * Accepted file formats are .txt or .csv 
#'  * The date is in European or ISO format (i.e. dmY or Ymd)
#' @md
#'  
#' @param elisa_file_path full or relative path or name of ELISA results file
#' 
#' @returns POSIXct formatted date time stamp object 
#' 
#' @import data.table lubridate stringr
#' 
#' @examples 
#'  \dontrun{
#'  
#'  # Get date time stamp from an ELISA raw results file:
#'  elisa_dtstamp <- extract_dtstamp("Biotek_ex_20210922.csv")
#'  
#'  }
#' 
#' @export
extract_dtstamp <- function(elisa_file_path){
  
  # Set regular expression to find date time pattern in the data:
  regdt = "(?=\\d)(?:(?:31(?!.(?:0?[2469]|11))|(?:30|29)(?!.0?2)|29(?=.0?2.(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00)))(?:\\x20|$))|(?:2[0-8]|1\\d|0?[1-9]))([-./])(?:1[012]|0?[1-9])\\1(?:1[6-9]|[2-9]\\d)?\\d\\d(?:(?=\\x20\\d)\\x20|$))(|([01]\\d|2[0-3])(:[0-5]\\d){1,2})?$"
  
  # Read in the raw ELISA results file as a data.table:
  dt = fread(file = elisa_file_path, stringsAsFactors = FALSE, na.strings = c(""))
  
  # Reduce values from data set to single vector:
  chars2search = as.vector(unlist(dt))
  
  # Extract the date time character string from the data:
  dtchar = subset(chars2search, !is.na(stringr::str_extract(chars2search, regdt)))
  
  # Check if string contains non date time info and if so, remove it:
  # Date time info will be either 19 (including seconds) or 16 characters
  if(nchar(dtchar) > 19){
    
    dtchar = substr(dtchar, start = nchar(dtchar) - 18, stop = nchar(dtchar))
    
  }
  
  # Convert the date time stamp to posixct format:
  dtformatted = lubridate::parse_date_time(dtchar, orders = c("dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS"))
  
  # Return the formatted date time object:
  return(dtformatted)
  
}
