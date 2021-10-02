#' Calculate quality control metrics for measles or rubella ELISA runs
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function determines whether kit and in-house controls for calculated 
#' ratios from measles and rubella kits fall within the accepted range of 
#' values defined by the manufacturer, thus validating an ELISA run.  
#' If controls are duplicated, coefficients of variation are also calculated.  
#' 
#' @note 
#' To be validated: 
#'  * in-house and kit negative control ratios must be < 0.8
#'  * in-house and kit positive control ratios must be >= 1.1
#'  * kit control ratios must fall within limits set by EUROIMMUN for each lot
#' @md
#' 
#' 
#' @param elisadata Data.table of calculated ELISA ratios
#' @param calref Reference value for the kit calibrator 
#' @param calinf Lower threshold for kit calibrator 
#' @param ipcinf Lower threshold for kit positive control
#' @param ipcsup Upper threshold for kit positive control
#' @param incinf Lower threshold for kit negative control
#' @param incsup Upper threshold for kit negative control
#' 
#' @returns Flextable of quality control indicators for the ELISA run 
#' 
#' @import data.table flextable officer
#' 
#' @examples 
#'  \dontrun{
#'  # Create QC table:
#' qctab <- runqc_ratios(elisadata = elisadt,
#'                       calinf = params$calinf,
#'                       calref = params$calref,
#'                       ipcinf = params$ipcinf,
#'                       ipcsup = params$ipcsup,
#'                       incinf = params$incinf,
#'                       incsup = params$incsup)
#'  }
#' @export
runqc_ratios <- function(elisadata, 
                         calinf,
                         calref,
                         ipcinf,
                         ipcsup,
                         incinf,
                         incsup){
  
  #########################################
  ### CREATE QC TABLE #####################
  
  # Set data.table name:
  elisadata = elisadata
  
  # Define duplicate controls:
  controls = c("Cal", "INC", "ENC", "IPC", "EPC")
  
  # Subset data for quality control:
  qcdata = subset(elisadata, PID %in% controls, 
                  select = c("PID", "Value"))
  
  # Add lower and upper limits to table:
  qcdata[PID == "Cal", lower := calinf]
  qcdata[PID == "Cal", upper := calref * 3]
  qcdata[PID == "INC", lower := incinf]
  qcdata[PID == "INC", upper := incsup]
  qcdata[PID == "IPC", lower := ipcinf]
  qcdata[PID == "IPC", upper := ipcsup]
  qcdata[PID == "ENC", lower := 0]
  qcdata[PID == "ENC", upper := 0.8]
  qcdata[PID == "EPC", lower := 1.1]
  qcdata[PID == "EPC", upper := 1.1*10]
  
  # Create index for the number of replicates:
  qcdata[, nreps := 1:.N, by = PID]
  
  # Get the maximum index number:
  maxind = max(qcdata$nreps, na.rm = TRUE)
  
  # Check if controls are duplicated:
  controldups = ifelse(!is.na(maxind) & maxind > 1, TRUE, FALSE)
  
  # Calculate mean and coefficients of variation for duplicate controls:
  qcsum = data.table::dcast(data = qcdata,
                            formula = PID + lower + upper ~ ., 
                            fun = list(mean, calculate_cv), 
                            value.var = "Value")
  
  # If controls are not duplicated, comment out CV:
  if(controldups == FALSE) {
    
    # Convert cv column to character to allow string input:
    qcsum[, Value_calculate_cv := as.character(Value_calculate_cv)]
    
    # Replace NAs with unique value statement:
    qcsum[is.na(Value_calculate_cv), Value_calculate_cv := "valeur unique"]
    
  }
  
  # Determine if values are in range:
  qcsum[, QCpass := fifelse(Value_mean >= lower & Value_mean < upper, 
                            "Vrai", "Faux")]
  
  # Add interpretation of QC result:
  qcsum[, Interpretation := fifelse(QCpass == "Vrai", "Acceptable", 
                                     fifelse(QCpass == "Faux" &
                                               Value_mean < lower, 
                                             "Trop faible", "Trop eleve"))]
  
  # Add source column:
  qcsum[, Source := fifelse(PID %in% c("Cal", "INC", "IPC"), "kit", "maison")]
  
  # Add control column:
  qcsum[, PID := dplyr::recode(PID, Cal = "Calibreur", 
                               INC = "Negatif", 
                               IPC = "Positif", 
                               ENC = "Negatif",
                               EPC = "Positif")]
  
  # Set column order:
  setcolorder(qcsum, neworder = c("Source", 
                                  "PID",
                                  "lower", 
                                  "upper", 
                                  "Value_mean", 
                                  "Value_calculate_cv",
                                  "QCpass", 
                                  "Interpretation"))
  
  # Set data order:
  setorder(qcsum, Source, PID, na.last = TRUE)
  
  # Rename columns in French for output:
  setnames(qcsum, old = names(qcsum), 
           new = c("Source", 
                   "Controle", 
                   "Inferieure", 
                   "Superieure", 
                   "Ratio", 
                   "Coefficient de variation", 
                   "CQ reussi",
                   "Interpretation"))
  
  # Create the printed table:
  qcprint <- flextable(qcsum) %>% 
    add_header_row(values = c("Source", 
                              "Controle", 
                              "Limite ratios", 
                              "Limite ratios", 
                              "Ratio", 
                              "Coefficient de variation", 
                              "CQ reussi", 
                              "Interpretation")) %>%
    theme_booktabs() %>%
    autofit(part = "all") %>%
    align(j = c(5:8), align = "right", part = "all") %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    align(j = c(3:4), align = "center", part = "all") %>%
    hline_top(border = fp_border(color = "black", width = 2), part = "all") %>%
    bold(part = "header") %>% 
    bg(part = "body", i = ~ (`CQ reussi` == "Faux" |
                               (is.numeric(`Coefficient de variation`) &
                                  `Coefficient de variation` > 10)), bg = "yellow")
  
  # Return the printable table:
  return(qcprint)

}
