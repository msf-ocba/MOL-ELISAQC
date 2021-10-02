#' Calculate normalized ratios and interpret ELISA results
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function normalises ELISA results from EUROIMMUN kits by calculating 
#' the ratio of the delta OD (optical density) for each sample to the delta OD
#' for the kit calibrator. Quantitative results are also interpreted as 
#' negative, indeterminate or positive according to reference ranges provided
#' by the user (these can be found on the kit lot quality assurance 
#' certificate and are unique to each lot number).
#' 
#' @note 
#' 1. The function assumes that ELISA results have already been converted to long
#' format by [label_elisaresults()].
#' 
#' 2. The raw results for the kit calibrator in the input data must be labeled
#' as "Cal" - if they are labeled as something else, this function will fail.
#' 
#' 3. If there are two calibrator values in the input data, this function will
#' calculate the average (according to the World Health Organisation protocol) 
#' and use the average delta OD of the calibrators to calculate the normalised
#' ratio for sample results.
#' 
#' @param elisadata Data.table of labeled raw ELISA results (delta OD)
#' 
#' @returns Data.table with two new columns; Value and Interpretation 
#' 
#' @import data.table
#' 
#' @examples 
#'  \dontrun{
#'  # Normalise and interpret ELISA results:
#'  elisadt <- calculate_ratios(elisadata = elisadt)
#'  }
#' @export
calculate_ratios <- function(elisadata){
  
  # Assign data.table name:
  elisadata = elisadata
  
  # Calculate average delta OD for calibrator duplicates:
  cal_mean = mean(elisadata$Delta_DO[elisadata$PID == "Cal"])
  
  # Calculate normalized results:
  elisadata[, Value := (Delta_DO / cal_mean)]
  
  # Put Calibrator results back to their original values for QC:
  elisadata[PID == "Cal", Value := Delta_DO]
  
  # Interpret normalized results:
  elisadata[ Value < 0.8, Interpretation := "negatif"]
  elisadata[ Value >= 0.8 & Value < 1.1, Interpretation := "indetermine"]
  elisadata[ Value >= 1.1, Interpretation := "positif"]
  
  # Set "interpretation" for calibrator results:
  elisadata[PID == "Cal", Interpretation := "calibreur"]
  
  # Round up Value to 3 decimal places for display purposes:
  elisadata[, Value := round(Value, digits = 3)]
  
  # Return updated data.table:
  return(elisadata)
  
}