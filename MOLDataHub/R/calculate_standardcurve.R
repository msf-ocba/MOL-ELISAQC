#' Convert ELISA results to IU/mL from standard curve
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function converts raw ELISA results to International Units (IU) per mL
#' using the four kit calibrator values in a standard curve as reference points.
#' The standard curve fit is calculated with the drc package.
#' 
#' @note 
#' 1. The function assumes that ELISA results have already been converted to 
#' long format by [label_elisaresults()].
#' 
#' 2. The raw results for the four kit calibrators in the input data must be 
#' labeled as "Cal1", "Cal2", "Cal3" and "Cal4", respectively.  If they are 
#' labeled as something else, this function will fail.
#' 
#' 3. If there are duplicates for each calibrator value in the input data, this 
#' function will calculate the average (according to the WHO protocol) and use 
#' the average delta OD of the calibrators to calculate the normalized ratio 
#' for sample results.
#' 
#' @param elisadata Data.table of labeled raw ELISA results (delta OD)
#' 
#' @returns Data.table with two new columns; Value and Interpretation 
#' 
#' @import data.table drc
#' 
#' @examples 
#'  \dontrun{
#'  # Normalise and interpret ELISA results:
#'  elisadt <- calculate_standardcurve(elisadata = elisadt)
#'  }
#' @export
calculate_standardcurve <- function(elisadata){
  
  # Assign data.table name:
  elisadata = elisadata
  
  # Add concentration values (in IU/mL) for calibrators:
  elisadata[PID == "Cal1", Value := 200]
  elisadata[PID == "Cal2", Value := 100]
  elisadata[PID == "Cal3", Value := 25]
  elisadata[PID == "Cal4", Value := 5]
  
  # Create the model of the standard curve with this data:
  sc = drc::drm(data = subset(elisadata, 
                              PID %in% c("Cal1", "Cal2", "Cal3", "Cal4")), 
                formula = Delta_DO ~ Value, 
                fct = LL.3())
  
  
  # Calculate concentration values for patient samples:
  elisadata[!PID %in% c("Cal1", "Cal2", "Cal3", "Cal4"), 
            Value := drc::ED(object = sc, 
                             respLev = Delta_DO, 
                             type = "absolute", 
                             display = FALSE)[,1]]
  
  # Interpret results:
  elisadata[ Value < 40, Interpretation := "negatif"]
  elisadata[ Value >= 40 & Value < 100, Interpretation := "indetermine"]
  elisadata[ (Value >= 100) | (Delta_DO > Delta_DO[PID == "Cal1"]), 
             Interpretation := "positif"]
  
  # Round up Value to 3 decimal places for display purposes:
  elisadata[, Value := round(Value, digits = 3)]
  
  # Convert value to character so that it can include proxy values:
  elisadata[, Value := as.character(Value)]
  
  # Assign proxy value " > 200 IU/mL" to samples with DOs higher than Cal1:
  elisadata[ Delta_DO > Delta_DO[PID == "Cal1"], Value := "> 200"]
  
  # Return updated data.table with concentrations in the Values column:
  return(elisadata)
  
}