#' Extract optical densities from a raw ELISA results file
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function extracts optical densities (ODs) from any raw ELISA file.
#' It is platform independent and works by finding the block of data preceded
#' by the 8 row names (A - H) usually applied to a 96-well ELISA plate.
#' Currently the function does not support plates of a different size.
#' All header information and un-necessary columns are removed.
#' 
#' Pre-requisites:
#'  * ELISA reader raw results file for a 96-well plate format
#'  * Accepted file formats are .txt or .csv 
#'  * The block of optical density results must be preceded by row labels A - H
#' @md
#'  
#' @param elisa_file_path full or relative path or name of ELISA results file
#' 
#' @returns data.table of ODs (8 rows A - H by 12 columns C01 - C12) 
#' 
#' @import data.table
#' 
#' @examples 
#'  \dontrun{
#'  
#'  # Import a .csv ELISA raw results file from a Biotek 800 TS reader:
#'  elisa_raw <- extract_ods("Biotek_ex_20210922.csv")
#'  
#'  }
#' 
#' @export
extract_ods <- function(elisa_file_path) {
  
  # Read complete raw file into R as a data.table:
  dt = fread(file = elisa_file_path, stringsAsFactors = FALSE, na.strings = c(""))
  
  # ELISA plate row names to look for:
  ern = c("A", "B", "C", "D", "E", "F", "G", "H")
  
  # Check which column contains all these strings:
  elisarows = unlist(lapply(dt, function(dtcol) all(ern %in% dtcol) == TRUE), use.names = TRUE)
  
  # Get the name of only the column that has the ELISA row names:
  col2sub = names(elisarows[elisarows == TRUE])
  
  # Get the index of col2sub:
  col2subind <- which(names(dt) == col2sub)
  
  # Subset the input data from col2sub on where rows match the labels:
  dtout = subset(dt, get(col2sub) %in% ern, select = c(col2subind:ncol(dt)))
  
  # Create new column names:
  newnames = c("Rows", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12")
  
  # Update the column names in the extracted data:
  setnames(dtout, old = names(dtout), new = newnames)
  
  # Create index for the number of replicates of each row label:
  dtout[, nreps := 1:.N, by = Rows]
  
  # Get the maximum index number:
  maxind = max(dtout$nreps, na.rm = TRUE)
  
  # In case ref values are given, select only the last set (delta):
  dtfinal = subset(dtout, nreps == maxind, names(dtout)[names(dtout) != "nreps"])
  
  # Ensure that all results columns are numeric and convert them if not:
  dtfinal[, c(2:13) := lapply(.SD, as.numeric), .SDcols = c(2:13)]
  
  # Return the final data set:
  return(dtfinal)
  
}
