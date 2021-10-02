#' Prepare laboratory data for export
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function prepares the processed laboratory results for export:
#'  a. To a Microsoft Excel file for validation by the lab supervisor
#'  b. To the Go.Data database for storage and merging with patient details
#' @md  
#' 
#' @param elisadata Data.table of ELISA data to prepare for export.
#' @param kit The name of the ELISA kit being used (inherited from params$kit)
#' @param timestamp The date-time stamp at which the ELISA plate was read.
#' @param qcstatus The quality control result for the ELISA run.
#' @param operator The name of the person who performed the test (from params)
#' 
#' @returns Data.table ready for export and MS Excel workbook with the results.
#' 
#' @import data.table openxlsx
#' 
#' @examples 
#'  \dontrun{
#'  # Export lab results:
#'  elisaout <- export_labresults(elisadata = elisadt, 
#'                                kit = params$kit, 
#'                                timestamp = elisa_dtstamp, 
#'                                qcstatus = qc, 
#'                                operator = params$operateur)
#'  }
#' @export
export_labresults <- function(elisadata, 
                              kit, 
                              timestamp,
                              qcstatus,
                              operator){
  
  # Name dataset:
  elisadata = elisadata
  
  # List controls to exclude:
  elisa_controls = c("Cal", 
                     "Cal1",
                     "Cal2",
                     "Cal3",
                     "Cal4",
                     "INC",
                     "IPC",
                     "ENC",
                     "EPC", 
                     "Empty")
  
  # Remove controls and blank wells before export:
  elisaout = subset(elisadata, !PID %in% elisa_controls, 
                     select = c("PID",
                                "Puit",
                                "Delta_DO", 
                                "Value",
                                "Interpretation"))
  
  # Update column names for export:
  setnames(elisaout, old = c("Value", "Delta_DO"), 
           new = c("Valeur", "Resultats_brutes"))
  
  # Set test prefixes for sample ID:
  if(kit == "Rougeole (IgM - NP)"){
    
    test_prefix = "RG"
    
  } else if(kit == "Rubéole (IgM - GP)"){
    
    test_prefix = "RB"
    
  } else {
    
    test_prefix = "PT"
    
  }
  
  # Generate sample ID:
  elisaout[, SID := paste0(PID, "_", test_prefix, "01")]
  
  # Add ELISA test kit details:
  kitdet = unlist(strsplit(kit, split = " \\("))
  
  kitdet[2] = gsub(")", "", kitdet[2])
  
  elisaout[, Pathogene := kitdet[1]]
  
  elisaout[, Molecule := kitdet[2]]
  
  # Add timestamp for ELISA results:
  elisaout[, Date_resultat := timestamp]
  
  # Add operator name:
  elisaout[, Operateur := operator]
  
  # Add QC status:
  elisaout[, CQ := qc]

  # Update column order:
  setcolorder(elisaout, c("PID",
                          "SID",
                          "Date_resultat",
                          "Pathogene",
                          "Molecule",
                          "Resultats_brutes",
                          "Valeur", 
                          "Interpretation",
                          "Puit",
                          "CQ", 
                          "Operateur"))
  
  # Sort the data by patient ID:
  setorder(elisaout, PID)
  
  # Set date:
  today = Sys.time()
  today = as.character(today)
  today = gsub(":", "", today)
  
  # Export the sub-setted data (must be .xlsx with dates in YYYY-MM-DD format):
  write.xlsx(x = elisaout, 
             file = here("Output", 
                         paste0("Resultats_", kit, "_", today, ".xlsx")),
             asTable = FALSE, 
             overwrite = TRUE)
  
  # Return cleaned results table:
  return(elisaout)
}