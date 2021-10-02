#' Check if quality control parameters have been met
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function takes the results of the [runqc_...()] functions and checks 
#' that all the main parameters have been met.  The binary result is used to 
#' trigger other actions to complete the QC report (tick or cross mark if 
#' passed or failed, respectively and advice on troubleshooting if failed.)  
#' 
#' @param qctable Flextable object containing quality control summary.
#' 
#' @returns Quality control global result (pass or fail) in a character string
#' 
#' @import data.table flextable officer
#' 
#' @examples 
#'  \dontrun{
#'  # Create final QC result:
#'  qc <- qc_check(qctable = qctab)
#'  }
#' @export
qc_check <- function(qctable){
  
  # Name dataset:
  qctable = qctable
  
  # Inspect the results from qctab to see if any of them were out of range:
  if(all(qctable$body$dataset$`CQ reussi` == "Vrai") == TRUE){
    
    qc = "reussi"
    
  } else {
    
    qc = "echoue" 
    
  }
  
}
