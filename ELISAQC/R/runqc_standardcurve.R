#' Calculate quality control metrics for pertussis ELISA runs
#' 
#' @author Amy Mikhail, \email{amy.mikhail@@gmail.com}
#' 
#' @description 
#' This function determines whether kit controls and calibrators for standard
#' curve-derived values from pertussis ELISA kits fall within the accepted 
#' range of values defined by the manufacturer, thus validating an ELISA run.  
#' If controls are duplicated, coefficients of variation are also calculated.  
#' 
#' @note 
#' To be validated: 
#'  * kit negative controls must be < 40 IU/mL
#'  * kit positive controls must be >= 40 IU/mL
#'  * kit control values must fall within limits set by EUROIMMUN for each lot
#' @md
#' 
#' 
#' @param elisadata Data.table of calculated ELISA values
#' @param cal12inf Lower limit (OD) for calibrators 1 - 2 
#' @param cal34inf Lower limit (OD) for calibrators 3 - 4 
#' @param cipcinf Lower threshold for kit positive control (IU/mL)
#' @param cipcsup Upper threshold for kit positive control (IU/mL)
#' @param cincinf Lower threshold for kit negative control (IU/mL)
#' @param cincsup Upper threshold for kit negative control (IU/mL)
#' 
#' @returns Flextable of quality control indicators for the ELISA run 
#' 
#' @import data.table flextable officer
#' 
#' @examples 
#'  \dontrun{
#'  # Create QC table:
#' qctab <- runqc_standardcurve(elisadata = elisadt,
#'                              cal12inf = params$cal12inf,
#'                              cal34inf = params$cal34inf,
#'                              cipcinf = params$cipcinf,
#'                              cipcsup = params$cipcsup,
#'                              cincinf = params$cincinf,
#'                              cincsup = params$cincsup)
#'  }
#' @export
runqc_standardcurve <- function(elisadata, 
                         cal12inf,
                         cal34inf,
                         cipcinf,
                         cipcsup,
                         cincinf,
                         cincsup){
  
  #########################################
  ### CREATE QC TABLE #####################
  
  # Set data.table name:
  elisadata = elisadata
  
  # Define duplicate controls:
  controls = c("Cal1", "Cal2", "Cal3", "Cal4", "INC", "IPC")
  
  # Subset data for quality control:
  qcdata = subset(elisadata, PID %in% controls, 
                  select = c("PID", "Delta_DO", "Value"))
  
  # Convert non-numeric elements of Value column back to numeric:
  qcdata[Value == "> 200", Value := "200"]
  
  # Convert column back to numeric:
  qcdata[, Value := as.numeric(Value)]
  
  # Add lower and upper limits to table:
  qcdata[PID %in% c("Cal1", "Cal2"), lower := cal12inf]
  qcdata[PID %in% c("Cal3", "Cal4"), lower := cal34inf]
  qcdata[PID == "INC", lower := cincinf]
  qcdata[PID == "INC", upper := cincsup]
  qcdata[PID == "IPC", lower := cipcinf]
  qcdata[PID == "IPC", upper := cipcsup]

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
                            value.var = c("Delta_DO", "Value"))
  
  # If controls are not duplicated, comment out CV:
  if(controldups == FALSE) {
    
    # Convert cv columns to character to allow string input:
    qcsum[, Value_calculate_cv := as.character(Value_calculate_cv)]
    qcsum[, Delta_DO_calculate_cv := as.character(Delta_DO_calculate_cv)]
    
    # Replace NAs with unique value statement:
    qcsum[is.na(Value_calculate_cv), Value_calculate_cv := "valeur unique"]
    qcsum[is.na(Delta_DO_calculate_cv), Delta_DO_calculate_cv := "valeur unique"]
    
  }
  
  # Determine if calibrators are in range:
  qcsum[PID %in% c("Cal1", "Cal2", "Cal3", "Cal4"), 
        QCpass := fifelse(Delta_DO_mean > lower, 
                            "Vrai", "Faux")]
  
  # Determine if controls are in range:
  qcsum[PID %in% c("INC", "IPC"), 
        QCpass := fifelse(Value_mean >= lower & Value_mean < upper, 
                          "Vrai", "Faux")]
  
  # Add interpretation of QC result for calibrators:
  qcsum[PID %in% c("Cal1", "Cal2", "Cal3", "Cal4"), 
        Interpretation := fifelse(QCpass == "Vrai", "Acceptable", 
                                    fifelse(QCpass == "Faux" &
                                              Delta_DO_mean < lower, 
                                            "Trop faible", "Trop eleve"))]
  
  # Add interpretation of QC result for controls:
  qcsum[PID %in% c("INC", "IPC"), 
        Interpretation := fifelse(QCpass == "Vrai", "Acceptable", 
                                    fifelse(QCpass == "Faux" &
                                              Value_mean < lower, 
                                            "Trop faible", "Trop eleve"))]
  
  
  # Add source column:
  qcsum[, Source := fifelse(PID %in% c("Cal1",
                                       "Cal2", 
                                       "Cal3",
                                       "Cal4",
                                       "INC",
                                       "IPC"), "kit", "maison")]
  
  # Add control column:
  qcsum[, PID := dplyr::recode(PID, Cal = "Calibreur", 
                               INC = "Negatif", 
                               IPC = "Positif")]
  
  # Set column order:
  setcolorder(qcsum, neworder = c("Source", 
                                  "PID",
                                  "lower", 
                                  "upper",
                                  "Delta_DO_mean",
                                  "Value_mean", 
                                  "Delta_DO_calculate_cv",
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
                   "Densite optique",
                   "IU/mL", 
                   "CV DO",
                   "CV IU/mL", 
                   "CQ reussi",
                   "Interpretation"))
  
  # Create the printed table:
  qcprint <- flextable(qcsum) %>% 
    add_header_row(values = c("Source", 
                              "Controle", 
                              "Limite IU/mL & DO", 
                              "Limite IU/mL & DO", 
                              "Densite optique",
                              "IU/mL", 
                              "Coefficient de variation", 
                              "Coefficient de variation", 
                              "CQ reussi", 
                              "Interpretation")) %>%
    theme_booktabs() %>%
    autofit(part = "all") %>%
    align(j = c(5:10), align = "right", part = "all") %>% 
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    align(j = c(3:4, 7:8), align = "center", part = "all") %>%
    hline_top(border = fp_border(color = "black", width = 2), part = "all") %>%
    bold(part = "header") %>% 
    bg(part = "body", i = ~ (`CQ reussi` == "Faux" |
                               (is.numeric(`CV DO`) & `CV DO` > 10) |
                               (is.numeric(`CV IU/mL`) & `CV IU/mL` > 10)), bg = "yellow")
  
  # Return the printable table:
  return(qcprint)
  
}
