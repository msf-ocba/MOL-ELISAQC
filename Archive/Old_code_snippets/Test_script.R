###################################
### ELISA QC CODE FOR SHINY APP ###
###################################

# Notes for code reviewers:
# - This script performs a complete QC check, analysis and interpretation on raw ELISA data from EUROIMMUN kits
# - The script is accompanied by two data sets, one which will pass the QC process and one which will fail.
# - There are also two matching tables of sample IDs - choose the one which matches the dataset you want to test.
# - After you have set which example data set and which set of sample IDs to use, you can run the rest of the code.
# - If you want to test this script with other datasets:
# ----- put them in the "data" folder of this repository
# ----- read in or create a table of matching sample IDs (see 96-well plate format below for example)
# ----- Change the name of the table assigned to "labels2use"
# ----- Change the name of the dataset to import and other parameters (e.g. rows to skip) in the import section

# Future developments:
# - This code is currently in a regular R script for initial testing but will soon be incorporated into a Shiny app.
# - Once the Shiny app has been developed, this script will still be available in the archive folder.

# Organisation of this code:
# - This code is divided into four main sections:
# - a. User input data (sample ids, quality control reference values and raw ELISA results)
# - b. Data preprocessing (reshaping and merging)
# - c. Analysis (calculation of quantitative results, interpretation and application of QC flags)
# - d. Production of outputs (graphs, heatmap, QC table, summary statement and processed data file)

# Lastly, please abide by the contribution guidelines and log any issues you encounter on the GitHub repository.


###################################
### R SETTINGS AND PACKAGES #######
###################################

### List of required packages:
pkgs2install <- c("devtools", 
                  "shiny",
                  "knitr", 
                  "rmarkdown",
                  "here", 
                  "data.table",
                  "dplyr",
                  "tidyr", 
                  "ggplot2",
                  "openxlsx", 
                  "flextable", 
                  "officer")

# Install packages (if required):
if (!requireNamespace(c(pkgs2install), quietly = TRUE)) install.packages(c(pkgs2install))

### Load packages:
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(flextable)
library(officer)

### Set R options:
options(scipen = 999)

### Create output directory:
if (!dir.exists(here("Output"))) {dir.create(here("Output"))}

###################################
### IMPORT DATA AND USER INPUTS ###
###################################

### A. ENTER SAMPLE ID LABELS #####

# Source the script that will create example tables of sample IDs:
labels2use <- source(here("data", "SampleIDs_QCpass.R"))

# Define which labels to use:
labels2use <- labels_qcpass


####################################
### B. IMPORT RAW ELISA RESULTS ####

# Example data that will fail QC:
data_qcfail <- fread(file = here("data", "ExampleData_QCfail.csv"), 
                     stringsAsFactors = FALSE, 
                     skip = 4, 
                     header = TRUE, 
                     dec = ".", 
                     colClasses = list(character = 1, numeric = 2:13))

# Example data that will pass QC:
data_qcpass <- fread(file = here("data", "ExampleData_QCpass.csv"), 
                     stringsAsFactors = FALSE, # read in strings as characters
                     skip = 27, # number of rows to skip
                     select = c(2:14), # which columns to select
                     header = TRUE, # use the first row of the selected data as column names
                     dec = ".", # specify the decimal separator (comma or period)
                     colClasses = list(character = 2, numeric = 3:14)) # predefine column types by numeric reference

### Define time stamp for ELISA results from file:
elisa_timestamp <- fread(file =  here("data", "ExampleData_QCpass.csv"), 
                         stringsAsFactors = FALSE, 
                         skip = 1, 
                         select = 1, 
                         nrows = 1, 
                         header = FALSE, 
                         colClasses = "character")

elisa_timestamp <- unlist(elisa_timestamp[1:1])
elisa_timestamp <- as.POSIXlt.character(elisa_timestamp, format = "%d/%m/%Y %H:%M")

# Define name of dataset to use:
elisa_raw <- data_qcfail


###################################
### C. IMPORT QC REFERNCE DATA ####

# Import IPB QC data:
ipbcontrols <- data.table(openxlsx::read.xlsx(xlsxFile = here("data", "20 10 21 IHC Rougeole Euroimmun.xlsx"), 
                                              sheet = "saisie des données", 
                                              startRow = 6, 
                                              colNames = TRUE, 
                                              cols = 1:7, 
                                              sep.names = "_"))

####################################
### D. ENTER KIT REFERENCE VALUES ##

# Define kit calibrator and control ranges:

cal_ref <- 0.316

cal_lower <- 0.140

ipc_ref <- 2.6

ipc_lower <- 1.4

ipc_upper <- 3.8

inc_ref <- 0.1

inc_lower <- 0

inc_upper <- 0.7


###################################
### DATA PREPROCESSING ############
###################################

### RESHAPE SAMPLE ID TABLE #######

# Reshape labels from wide to long format:
labelong <- data.table(tidyr::pivot_longer(data = labels2use, 
                                           cols = starts_with("C"),
                                           names_to = "Cols",
                                           values_to = "PID"))

# Add a col with complete well ID:
labelong[, Well := paste0(Rows, gsub("C", "", Cols))]

# Sort columns:
setcolorder(labelong, neworder = c("Rows", "Cols", "Well", "PID"))

# Set key:
setkeyv(labelong, c("Rows", "Cols", "Well"))


###################################
### SET COLUMN NAMES FOR ELISA ####

# Define column names to change:
cols2change_o <- names(elisa_raw[,2:13])

# Reformat column names as for labels:
cols2change_n <- fifelse(nchar(cols2change_o) == 1, paste0("C0", cols2change_o), paste0("C", cols2change_o))

# Rename the columns:
setnames(elisa_raw, cols2change_o, cols2change_n)


###################################
### RESHAPE ELISA RESULTS #########

# Reshape labels from wide to long format:
elisalong <- data.table(tidyr::pivot_longer(data = elisa_raw, 
                                           cols = starts_with("C"),
                                           names_to = "Cols",
                                           values_to = "Delta_DO"))

# Rename Plate rows:
setnames(elisalong, 1, "Rows")

# Add a col with complete well ID:
elisalong[, Well := paste0(Rows, gsub("C", "", Cols))]

# Sort columns:
setcolorder(elisalong, neworder = c("Rows", "Cols", "Well", "Delta_DO"))

# Set key:
setkeyv(elisalong, c("Rows", "Cols", "Well"))


###########################################
### MERGE ELISA RESULTS WITH SAMPLE IDS ###

# Merge sample labels and elisa results:
elisadt <- merge.data.table(labelong, elisalong)

# Replace empty sample IDs with blanks:
elisadt[ PID == "", PID := "Empty"]

# Replace ODs in empty wells with NA:
elisadt[ PID == "Empty", Delta_DO := NA]


###################################
### ANALYSIS AND CALCULATIONS #####
###################################

### MORMALISE RESULTS #############

# Calculate average extinction value for calibrator duplicates:
cal_mean <- mean(elisadt$Delta_DO[elisadt$PID == "Cal"])

# Calculate normalised results:
elisadt[, Rapport := (Delta_DO / cal_mean)]

# Put Calibrator results back to their original values for QC:
elisadt[PID == "Cal", Rapport := Delta_DO]

# Interpret normalised results:
elisadt[ Rapport < 0.8, Interpretation := "Négatif"]
elisadt[ Rapport >= 0.8 & Rapport < 1.1, Interpretation := "Limite"]
elisadt[ Rapport >= 1.1, Interpretation := "Positif"]

# Correct interpretation for calibrator results:
elisadt[PID == "Cal", Interpretation := "Calibrateur"]


###################################
### ADD IPB QC CONTROL LIMITS #####  

# Ensure columns are numeric:
ipbcontrols[, Do_Ctl_Neg := as.numeric(Do_Ctl_Neg)]
ipbcontrols[, Do_Ctl_Pos_M := as.numeric(Do_Ctl_Pos_M)]

# Mean IPB positive in-house control:
EPCmean <- mean(ipbcontrols$Do_Ctl_Pos_M, na.rm = TRUE)

# Standard deviation IPB positive in-house control:
EPCsd <- sd(ipbcontrols$Do_Ctl_Pos_M, na.rm = TRUE)

# Mean IPB negative in-house control:
ENCmean <- mean(ipbcontrols$Do_Ctl_Neg, na.rm = TRUE)

# Standard deviation IPB negative in-house control:
ENCsd <- sd(ipbcontrols$Do_Ctl_Neg, na.rm = TRUE)

# Table of QC values:
ipbqc <- data.table(PID = c("EPC", "ENC"), 
                    ControlMean = c(EPCmean, ENCmean), 
                    ControlSD = c(EPCsd, ENCsd))

# Add Standard deviations to get acceptable limits:
ipbqc[, SDplus1 := ControlMean + (ControlSD * 1)]
ipbqc[, SDplus2 := ControlMean + (ControlSD * 2)]
ipbqc[, SDplus3 := ControlMean + (ControlSD * 3)]
ipbqc[, SDmoin1 := ControlMean - (ControlSD * 1)]
ipbqc[, SDmoin2 := ControlMean - (ControlSD * 2)]
ipbqc[, SDmoin3 := ControlMean - (ControlSD * 3)]


########################################
### ADD CALCULATED QC CONTROL LIMITS ###  

cal_upper <- cal_ref * 3

epc_lower <- ipbqc$SDmoin2[ipbqc$PID == "EPC"]

epc_upper <- ipbqc$SDplus2[ipbqc$PID == "EPC"]

enc_lower <- ipbqc$SDmoin2[ipbqc$PID == "ENC"]

enc_upper <- ipbqc$SDplus2[ipbqc$PID == "ENC"]


###################################
### ADD QC FLAGS ##################

# Function to calculate coefficient of variation:
cv <- function(values, groups){
  
  # Calculate standard deviation:
  stdev = sd(values)
  
  # Calculate mean:
  avg = mean(values)
  
  # Calculate % coefficient of variation:
  cvres  = round((stdev / avg) * 100, 2)
  
  # Return result:
  return(cvres)
  
}

# Define duplicate controls:
controls <- c("Cal", "INC", "ENC", "IPC", "EPC")

# Subset data for quality control:
qcdata <- subset(elisadt, PID %in% controls, select = c("Well", "PID", "Delta_DO", "Rapport"))

# Calculate the mean Delta_DO for each duplicate:
qcdata[, Mean_DOD := mean(Delta_DO), by = PID]

# Calculate the SD for Delta_DO for each duplicate:
qcdata[, SD_DOD := sd(Delta_DO), by = PID]

# Calculate coefficient of variation for each duplicate:
qcdata[, CV := cv(Delta_DO), by = PID]

# Calculate maximum CV between the duplicates:
qcdata[, maxcv := max(CV, na.rm = TRUE)]

# Add a flag based on the results:
qcdata[, QC_cvpass := fifelse(maxcv < 10, "Vrai", "Faux")]

# Calculate mean of OD duplicates for calibrator:
qcdata[PID == "Cal", Mean_RQ := mean(Delta_DO)]

# Calculate mean calibrator-normalised value for kit negative control:
qcdata[PID == "INC", Mean_RQ := mean(Rapport)]

# Calculate mean calibrator-normalised value for IPB negative control:
qcdata[PID == "ENC", Mean_RQ := mean(Rapport)]

# Calculate mean calibrator-normalised value for kit positive control:
qcdata[PID == "IPC", Mean_RQ := mean(Rapport)]

# Calculate mean calibrator-normalised value for IPB positive control:
qcdata[PID == "EPC", Mean_RQ := mean(Rapport)]


# Add lower bounds column:
qcdata[PID == "Cal", Inferieure := rep(cal_lower, 2)]
qcdata[PID == "INC", Inferieure := rep(inc_lower, 2)]
qcdata[PID == "ENC", Inferieure := rep(enc_lower, 2)]
qcdata[PID == "IPC", Inferieure := rep(ipc_lower, 2)]
qcdata[PID == "EPC", Inferieure := rep(epc_lower, 2)]


# Add upper bounds column:
qcdata[PID == "Cal", Superieure := rep(cal_upper, 2)]
qcdata[PID == "INC", Superieure := rep(inc_upper, 2)]
qcdata[PID == "ENC", Superieure := rep(enc_upper, 2)]
qcdata[PID == "IPC", Superieure := rep(ipc_upper, 2)]
qcdata[PID == "EPC", Superieure := rep(epc_upper, 2)]


# Determine if controls are within accepted range:
qcdata[, QCpass := fifelse(data.table::between(x = Mean_RQ, 
                                   lower = Inferieure, 
                                   upper = Superieure, 
                                   incbounds = TRUE), "Vrai", "Faux")]

# Interpret QC results:
qcdata[, Interpretation := fifelse(QCpass == "Vrai", "Acceptable", 
                                   fifelse(QCpass == "Faux" & Mean_RQ < Inferieure, 
                                           "Trop faible", "Trop élevé"))]



###################################
### OUTPUTS AND DOWNLOADS #########
###################################

# Create QC output table:
qcout <- dcast(qcdata, PID + 
                 Mean_DOD + 
                 SD_DOD + 
                 CV + 
                 QC_cvpass + 
                 Mean_RQ + 
                 Inferieure + 
                 Superieure + 
                 QCpass + 
                 Interpretation ~ ., value.var = "PID", fun.aggregate = length)

# Add source of controls in a separate column:
qcout[grepl("^I", PID) == TRUE | PID == "Cal", Source := "kit"]
qcout[grepl("^E", PID) == TRUE, Source := "maison"]

# Rename controls:
qcout[, PID := .(dplyr::recode(PID, "Cal" = "Calibrateur", 
                        "INC" = "Négatif", 
                        "ENC" = "Négatif", 
                        "IPC" = "Positif", 
                        "EPC" = "Positif"))] 


# Sort controls:
setorder(qcout, PID, na.last = TRUE)

# Remove extra column:
qcout[, `.` := NULL]

# Set column order:
setcolorder(qcout, neworder = c("PID", "Source", "Mean_DOD", "Mean_RQ", "SD_DOD", "CV", "QC_cvpass", 
                                "Inferieure", "Superieure", "QCpass", "Interpretation"))

# Rename columns in French for output:
setnames(qcout, old = c("PID", "Mean_DOD", "Mean_RQ", "SD_DOD", "QC_cvpass", "QCpass"), 
         new = c("Controle", "DO", "Rapport", "DS", "CV reussi", "CQ reussi"))

# Reduce decimal places:
numcols <- names(which(sapply(qcout, is.numeric)))
qcout[, (numcols) := round(.SD, 3), .SDcols = numcols]


# Create the printed table:
qcprint <- flextable(qcout) %>% 
  add_header_row(values = c("Controle", 
                            "Source", 
                            "DO", 
                            "Rapport", 
                            "DS", 
                            "CV", 
                            "CV reussi", 
                            "Limite", 
                            "Limite", 
                            "CQ reussi", 
                            "Interpretation")) %>%
  theme_booktabs() %>%
  autofit(part = "all") %>%
  align(j = c(7, 10, 11), align = "right", part = "all") %>% 
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  align(j = c(8:9), i = 1, align = "center", part = "header") %>%
  hline_top(border = fp_border(color = "black", width = 2), part = "all") %>%
  bold(part = "header")  

# Print the table:
qcprint

###################################
### GRAPH FOR IPB CONTROLS ###

# Determine run number for current plate relative to IPB control plates:
plate_no <- max(ipbcontrols$`N°_de_Serie`, na.rm = TRUE) + 1


# Add reult from this dataset to IPB data:
EQCdata <- data.table(Serie = c(ipbcontrols$`N°_de_Serie`, plate_no), 
                      ENC = c(ipbcontrols$Do_Ctl_Neg, unique(qcdata$Mean_RQ[qcdata$PID == "ENC"])), 
                      EPC = c(ipbcontrols$Do_Ctl_Pos_M, unique(qcdata$Mean_RQ[qcdata$PID == "EPC"])))

# Define current plate results:
EQCdata[, Plaque := fifelse(Serie == plate_no, "Actuel", "Réference")]

# Create subset for current plate external control results:
EQCcurrent <- subset(EQCdata, Plaque == "Actuel")


# Define the upper and lower bounds for the negative control plot:
encbounds <- data.table(yintercept = c(ipbqc$SDmoin2[ipbqc$PID == "ENC"],
                                       ipbqc$ControlMean[ipbqc$PID == "ENC"],
                                       ipbqc$SDplus2[ipbqc$PID == "ENC"]), 
                        Limites = c(paste0(expression("\u0394"), " 2 DS"), 
                                    "Moyen", 
                                    paste0(expression("\u0394"), " 2 DS")))

# Plot the line graph for the IPB negative control with points:
ENCplot <- ggplot(EQCdata, aes(x = Serie, y = ENC, color = Plaque)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = yintercept, linetype = Limites), encbounds) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("Réference" = "blue", "Actuel" = "red")) +
  annotate("text", x = EQCcurrent$Serie, y = (EQCcurrent$ENC + (EQCcurrent$ENC * 0.15)), 
           label = round(EQCcurrent$ENC, 3)) +
  ggtitle("Graphique d'assurance de qualité: contrôles négatifs") +
  ylab("Valeurs de contrôles négatifs (unités arbitraires)") + 
  theme(axis.title.x = element_text(family = "sans", size = 14, margin = margin(20,0,0,0)), 
        axis.title.y = element_text(family = "sans", size = 14, margin = margin(0,20,0,0)), 
        axis.text = element_text(family = "sans", size = 12),
        legend.title = element_text(family = "sans", size = 12, face = "bold", margin = margin(0,0,2.5,0)), 
        legend.text = element_text(family = "sans", size = 10),
        plot.title = element_text(family = "sans", size = 16, face = "bold", margin = margin(0,0,25,0)))

# Print the negative control plot:
ENCplot


# Define the upper and lower bounds for the positive control plot:
epcbounds <- data.table(yintercept = c(ipbqc$SDmoin2[ipbqc$PID == "EPC"],
                                       ipbqc$ControlMean[ipbqc$PID == "EPC"],
                                       ipbqc$SDplus2[ipbqc$PID == "EPC"]), 
                        Limites = c(paste0(expression("\u0394"), " 2 DS"), 
                                    "Moyen", 
                                    paste0(expression("\u0394"), " 2 DS")))

# Plot the line graph for the IPB positive control with points:
EPCplot <- ggplot(EQCdata, aes(x = Serie, y = EPC, color = Plaque)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = yintercept, linetype = Limites), epcbounds) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("Réference" = "blue", "Actuel" = "red")) +
  annotate("text", x = EQCcurrent$Serie, y = (EQCcurrent$EPC + (EQCcurrent$EPC * 0.05)), 
           label = round(EQCcurrent$EPC, 3)) +
  ggtitle("Graphique d'assurance de qualité: contrôles positifs") +
  ylab("Valeurs de contrôles positifs (unités arbitraires)") + 
  theme(axis.title.x = element_text(family = "sans", size = 14, margin = margin(20,0,0,0)), 
        axis.title.y = element_text(family = "sans", size = 14, margin = margin(0,20,0,0)), 
        axis.text = element_text(family = "sans", size = 12),
        legend.title = element_text(family = "sans", size = 12, face = "bold", margin = margin(0,0,2.5,0)), 
        legend.text = element_text(family = "sans", size = 10),
        plot.title = element_text(family = "sans", size = 16, face = "bold", margin = margin(0,0,25,0)))

# Print the negative control plot:
EPCplot



###################################
### HEAT MAP OF ELISA RESULTS ###

elisadt[, Rowlabs := factor(Rows)]
elisadt[, Colabs := factor(Cols)]


platemap <- ggplot(elisadt, aes(x = Colabs, y = ordered(Rowlabs, levels = rev(levels(Rowlabs))))) + 
  geom_tile(fill = "white", na.rm = TRUE) +
  geom_point(aes(colour = Rapport, size = 200)) +
  scale_size_continuous(range = c(10, 12)) +
  guides(size = FALSE) +
  scale_color_gradient(low = "lightblue", high = "darkblue", na.value = "lightgrey") + 
  #scale_color_manual(values = "lightgrey", labels = "Puits vides") +
  ylab("") +
  xlab("") +
  ggtitle("Visualisation du plaque ELISA: résultats normalisées") +
  scale_x_discrete(position = "top") + 
  theme(axis.title.x = element_text(family = "sans", size = 14, margin = margin(0,0,20,0)), 
        axis.title.y = element_text(family = "sans", size = 14, margin = margin(0,20,0,0)), 
        axis.text = element_text(family = "sans", size = 12),
        legend.title = element_text(family = "sans", size = 12, face = "bold", margin = margin(0,0,2.5,0)), 
        legend.text = element_text(family = "sans", size = 10),
        plot.title = element_text(family = "sans", size = 16, face = "bold", margin = margin(0,0,5,0)))

platemap


###################################
### RESULTS STATEMENT FOR REPORT ##


# Final text advice based on quality control results:
if(all(qcdata$QC_cvpass) == TRUE & all(qcdata$QCpass) == TRUE) {
  
  conseil <- cat("Ce test ELISA a été soumis à des procédures internes de contrôle de la qualité.\nLes résultats sont prêts à être communiqués.")
  
} else {
  
  conseil <- cat("Ce test ELISA a échoué aux procédures internes de contrôle de la qualité.\nEnvisagez de refaire le test de la plaque.\nLes problèmes suivants doivent être vérifiés et résolus lors du dépannage:\n
A. Si les résultats sont trop faibles:\n- Le temps d'incubation était trop court\n- La température d'incubation était trop basse\n- Le tampon de lavage n'a pas été complètement retiré des puits\n- Il y a eu une rupture de la chaîne du froid pendant le stockage des réactifs\n
B. Si les résultats sont trop élevés:\n- Le temps d'incubation était trop long\n- La température d'incubation (ambiante) était trop élevée\n- Le lavage des assiettes était insuffisant (temps trop court ou pas assez de tampon de lavage utilisé)\n- Une contamination croisée s'est produite (si les valeurs du contrôle négatif sont trop élevées)\n 
C. Si les Coefficients de variation (CV) sont trop élevés:\n- Les broches du lecteur ELISA peuvent etre mal alignées\n- Les composants optiques du lecteur ELISA peuvent être contaminés par de la poussière")
  
}


########################################
### EXPORT OF PROCESSED ELISA RESULTS ##

# Remove controls and blank wells before export:
elisaout <- subset(elisadt, !PID %in% c("Cal", "INC", "IPC", "ENC", "EPC", "Empty"), 
                   select = c("PID", "Well", "Delta_DO", "Rapport", "Interpretation"))

# Sort the data by patient ID:
setorder(elisaout, PID)

# Set date:
today <- Sys.Date()

# Export the sub-setted data:
write.csv(x = elisaout, 
          file = here("Output", paste0("Résultats ELISA à reporter_", today, ".csv")), 
          na = "", 
          row.names = FALSE)


############################################################################
## To do:

# Export Rmd report with all this stuff in it but also show on dashboard
