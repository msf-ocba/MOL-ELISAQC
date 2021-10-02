#' Label raw ELISA plate results with sample IDs
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function reshapes imported ELISA data and sample IDs from wide (plate 
#' format) to long and merges the two data.tables together so that ELISA 
#' results are associated with their sample IDs.
#' 
#' @note 
#' The function assumes column and row names are inherited from the import 
#' function [extract_ods()] for 96-well ELISA plates.
#' Rows in the input tables should be labeled A:H
#' Columns in the input tables should be labeled C01:C12
#' 
#' @param sampleids Data.table of sample IDs in wide (plate) format
#' @param elisaresults Data.table of raw ELISA results in wide (plate) format
#' 
#' @returns Data.table of merged sample IDs and raw ELISA results 
#' 
#' @import data.table tidyr
#' 
#' @examples 
#'  \dontrun{
#'  # Label ELISA results with their sample IDs:
#'  elisadt <- label_elisaresults(sampleids = sampleids, 
#'                                elisaresults = elisa_raw)
#'  }
#' 
#' @export
label_elisaresults <- function(sampleids, elisaresults){
  
  #######################################
  ### RESHAPE SAMPLE ID TABLE ###########
  
  # Reshape sample IDs from wide to long format:
  labelong = data.table(tidyr::pivot_longer(data = sampleids, 
                                            cols = starts_with("C"),
                                            names_to = "Cols",
                                            values_to = "PID"))
  
  # Add a column with the well IDs:
  labelong[, Puit := paste0(Rows, gsub("C", "", Cols))]
  
  # Sort columns:
  setcolorder(labelong, neworder = c("Rows", "Cols", "Puit", "PID"))
  
  # Set key:
  setkeyv(labelong, c("Rows", "Cols", "Puit"))
  
  #######################################
  ### RESHAPE ELISA RESULTS #############
  
  # Reshape labels from wide to long format:
  elisalong = data.table(tidyr::pivot_longer(data = elisaresults, 
                                              cols = starts_with("C"),
                                              names_to = "Cols",
                                              values_to = "Delta_DO"))
  
  # Add a column with the well IDs:
  elisalong[, Puit := paste0(Rows, gsub("C", "", Cols))]
  
  # Sort columns:
  setcolorder(elisalong, neworder = c("Rows", "Cols", "Puit", "Delta_DO"))
  
  # Set key:
  setkeyv(elisalong, c("Rows", "Cols", "Puit"))
  
  ########################################
  ### MERGE SAMPLE IDS & ELISA RESULTS ###
  
  # Merge sample labels and elisa results:
  elisadt = merge.data.table(labelong, elisalong)
  
  # Replace empty sample IDs with blanks:
  elisadt[ PID %in% c(NA, ""), PID := "Empty"]
  
  # Replace ODs in empty wells with NA:
  elisadt[ PID == "Empty", Delta_DO := NA]
  
  ########################################
  ### RETURN RESHAPED DATA.TABLE #########
  
  return(elisadt)
  
}