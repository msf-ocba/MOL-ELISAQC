#' Merge case and lab data on case ID to create final line list
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function merges case and laboratory data imported from Go.Data, based 
#' on the case (visual) ID.  The line list can be used for downstream analyses.
#' 
#' @note 
#' 1. The line list is presented at the sample test level, i.e. there is one row
#' for each test result.  Where there are multiple test results for a single 
#' case, case-level data is repeated on each row.
#' 
#' 2. Results are organized by case ID
#' 
#' 3. Column names for merging are inherited from [godata_getcaselist()] and
#' [godata_getlablist()] functions, respectively. 
#'  
#' @param caselist Data.table of case data imported from Go.Data
#' @param lablist Data.table of laboratory data imported from Go.Data
#' 
#' @returns Data.table of merged case and laboratory data 
#' 
#' @import data.table openxlsx
#' 
#' @examples 
#'  \dontrun{
#'  # Fetch lab results:
#'  linelist <- create_linelist(caselist = caseres, lablist = labres)
#'  }
#' 
#' @export
create_linelist <- function(caselist, lablist){
  
  # Merge case and laboratory data based on patient ID:
  linelist = data.table::merge.data.table(caselist, lablist, by = c("mol_pid", "godata_pid"))
  
  # Update case classification status according to lab results:
  linelist[, classification_cas := fifelse(interpretation == "positif", 
                                            paste0("confirmé - ", maladie), 
                                            fifelse(interpretation == "négatif", 
                                                    paste0("rejeté - ", maladie), 
                                                    classification_cas))]
  
  # Set date:
  today = as.character(Sys.time())
  today = gsub(":", "", today)
  
  # Export line list to Microsoft Excel workbook:
  write.xlsx(x = linelist, 
             file = here("Output", paste0("MOL_liste_lineaire_cas_", today, ".xlsx")),
             asTable = FALSE, 
             overwrite = TRUE)
  
  
  # Return merged and updated line list:
  return(linelist)
  
}
