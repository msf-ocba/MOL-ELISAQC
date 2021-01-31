###################################
### ELISA QC CODE FOR SHINY APP ###
###################################

### LOAD PACKAGES ###

library(data.table)
library(tidyr)
library(ggplot2)
library(openxlsx)

### Set R options:
options(scipen = 999)

###################################
### EXAMPLE LABELS ###

# Create example table of Sample IDs in 96-well plate configuration:
labels_qcfail <- data.table(Rows = c("A", "B", "C", "D", "E", "F", "G", "H"), 
                      C01 = c("Cal", "IPC", "INC", "EPC", "ENC", "S01", "S02", "S03"), 
                      C02 = c("Cal", "IPC", "INC", "EPC", "ENC", "S04", "S05", "S06"),
                      C03 = c("S07", "S08", "S09", "S10", "S11", "S12", "S13", "S14"),
                      C04 = c("S15", "S16", "S17", "S18", "S19", "S20", "S21", "S22"), 
                      C05 = c("S23", "S24", "S25", "S26", "S27", "S28", "S29", "S30"), 
                      C06 = c("S31", "S32", "S33", "S34", "S35", "S36", "S37", "S38"), 
                      C07 = c("S39", "S40", "S41", "S42", "S43", "S44", "S45", "S46"), 
                      C08 = c("S47", 48, 49, 50, "", "", "", ""),  
                      C09 = c("", "", "", "", "", "", "", ""), 
                      C10 = c("", "", "", "", "", "", "", ""), 
                      C11 = c("", "", "", "", "", "", "", ""), 
                      C12 = c("", "", "", "", "", "", "", ""))

# Example labels for drytest data:
labels_qcpass <- data.table(Rows = c("A", "B", "C", "D", "E", "F", "G", "H"), 
                            C01 = c("Cal", "IPC", "INC", "EPC", "ENC", "S01", "S02", "S03"), 
                            C02 = c("Cal", "IPC", "INC", "EPC", "ENC", "S04", "S05", "S06"),
                            C03 = c("", "", "", "", "", "", "", ""),
                            C04 = c("", "", "", "", "", "", "", ""), 
                            C05 = c("", "", "", "", "", "", "", ""), 
                            C06 = c("", "", "", "", "", "", "", ""), 
                            C07 = c("", "", "", "", "", "", "", ""), 
                            C08 = c("", "", "", "", "", "", "", ""),  
                            C09 = c("", "", "", "", "", "", "", ""), 
                            C10 = c("", "", "", "", "", "", "", ""), 
                            C11 = c("", "", "", "", "", "", "", ""), 
                            C12 = c("", "", "", "", "", "", "", ""))

# Define labels to use:
labels2use <- labels_qcfail


###################################
### RESHAPE TO LONG ###

# Reshape labels from wide to long format:
labelong <- data.table(tidyr::pivot_longer(data = labels2use, 
                                           cols = starts_with("C"),
                                           names_to = "Cols",
                                           values_to = "PID"))

# Add a col with complete well ID:
labelong[, Well := paste0(Rows, gsub("C", "", Cols))]

# Sort columns:
setcolorder(labelong, neworder = c("Rows", "Cols", "Well", "PID"))

# Remove Row and Col columns:
#labelong <- labelong[, c("Well", "PID")] # don't remove as need them for heatmap

# Set key:
setkeyv(labelong, c("Rows", "Cols", "Well"))

###################################
### IMPORT ELISA RESULTS ###

# Test import EU format:
eu <- fread(file = "ELISA_res_EU.csv",
            stringsAsFactors = FALSE, 
            skip = 4, 
            header = TRUE, 
            dec = ",", 
            colClasses = list(character = 1, numeric = 2:13))

# Test import UK format:
data_qcfail <- fread(file = "ExampleData_QCfail.csv", 
            stringsAsFactors = FALSE, 
            skip = 4, 
            header = TRUE, 
            dec = ".", 
            colClasses = list(character = 1, numeric = 2:13))

# Test import of dry test data:
data_qcpass <- fread(file = "ExampleData_QCpass.csv", 
                 stringsAsFactors = FALSE, # read in strings as characters
                 skip = 27, # number of rows to skip
                 select = c(2:14), # which columns to select
                 header = TRUE, # use the first row of the selected data as column names
                 dec = ".", # specify the decimal separator (comma or period)
                 colClasses = list(character = 2, numeric = 3:14)) # predefine column types by numeric reference

# Define name of dataset to use:
elisa_raw <- data_qcfail

###################################
### SET COLUMN NAMES FOR ELISA ###

# Define column names to change:
cols2change_o <- names(elisa_raw[,2:13])

# Reformat column names as for labels:
cols2change_n <- fifelse(nchar(cols2change_o) == 1, paste0("C0", cols2change_o), paste0("C", cols2change_o))

# Rename the columns:
setnames(elisa_raw, cols2change_o, cols2change_n)


###################################
### RESHAPE ELISA RESULTS ###


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

###################################
### MERGE ELISA RESULTS WITH SAMPLE IDS ###

# Merge sample labels and elisa results:
elisadt <- merge.data.table(labelong, elisalong)

# Replace empty sample IDs with blanks:
elisadt[ PID == "", PID := "Empty"]

# Replace ODs in empty wells with NA:
elisadt[ PID == "Empty", Delta_DO := NA]


###################################
### MORMALISE RESULTS ###

# Calculate average extinction value for calibrator duplicates:
cal_mean <- mean(elisadt$Delta_DO[elisadt$PID == "Cal"])

# Calculate normalised results:
elisadt[, Resultats := (Delta_DO / cal_mean)]

# Put Calibrator results back to their original values for QC:
elisadt[PID == "Cal", Resultats := Delta_DO]

# Interpret normalised results:
elisadt[ Resultats < 0.8, Interpretation := "Négatif"]
elisadt[ Resultats >= 0.8 & Resultats < 1.1, Interpretation := "Limite"]
elisadt[ Resultats >= 1.1, Interpretation := "Positif"]

# Correct interpretation for calibrator results:
elisadt[PID == "Cal", Interpretation := "Calibrateur"]

###################################
### ADD IPB QC CONTROL LIMITS ###  

# Import IPB QC data:
ipbcontrols <- data.table(openxlsx::read.xlsx(xlsxFile = "20 10 21 IHC Rougeole Euroimmun.xlsx", 
                             sheet = "saisie des données", 
                             startRow = 6, 
                             colNames = TRUE, 
                             cols = 1:7, 
                             sep.names = "_"))

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


###################################
### ADD Kit QC CONTROL LIMITS ###  

# Define kit calibrator and control ranges:

cal_ref <- 0.316

cal_lower <- 0.140

cal_upper <- cal_ref * 3

ipc_ref <- 2.6

ipc_lower <- 1.4

ipc_upper <- 3.8

inc_ref <- 0.1

inc_lower <- 0

inc_upper <- 0.7

epc_lower <- ipbqc$SDmoin2[ipbqc$PID == "EPC"]

epc_upper <- ipbqc$SDplus2[ipbqc$PID == "EPC"]

enc_lower <- ipbqc$SDmoin2[ipbqc$PID == "ENC"]

enc_upper <- ipbqc$SDplus2[ipbqc$PID == "ENC"]


###################################
### ADD QC FLAGS ###

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
qcdata <- subset(elisadt, PID %in% controls, select = c("Well", "PID", "Delta_DO", "Resultats"))

# Calculate the mean Delta_DO for each duplicate:
qcdata[, Mean_DOD := mean(Delta_DO), by = PID]

# Calculate the SD for Delta_DO for each duplicate:
qcdata[, SD_DOD := sd(Delta_DO), by = PID]

# Calculate coefficient of variation for each duplicate:
qcdata[, CV := cv(Delta_DO), by = PID]

# Calculate maximum CV between the duplicates:
qcdata[, maxcv := max(CV, na.rm = TRUE)]

# Add a flag based on the results:
qcdata[, QC_cvpass := fifelse(maxcv < 10, TRUE, FALSE)]

# Calculate mean of OD duplicates for calibrator:
qcdata[PID == "Cal", Mean_RQ := mean(Delta_DO)]

# Calculate mean calibrator-normalised value for kit negative control:
qcdata[PID == "INC", Mean_RQ := mean(Resultats)]

# Calculate mean calibrator-normalised value for IPB negative control:
qcdata[PID == "ENC", Mean_RQ := mean(Resultats)]

# Calculate mean calibrator-normalised value for kit positive control:
qcdata[PID == "IPC", Mean_RQ := mean(Resultats)]

# Calculate mean calibrator-normalised value for IPB positive control:
qcdata[PID == "EPC", Mean_RQ := mean(Resultats)]


# Add lower bounds column:
qcdata[PID == "Cal", L_inferieure := rep(cal_lower, 2)]
qcdata[PID == "INC", L_inferieure := rep(inc_lower, 2)]
qcdata[PID == "ENC", L_inferieure := rep(enc_lower, 2)]
qcdata[PID == "IPC", L_inferieure := rep(ipc_lower, 2)]
qcdata[PID == "EPC", L_inferieure := rep(epc_lower, 2)]


# Add upper bounds column:
qcdata[PID == "Cal", L_superieure := rep(cal_upper, 2)]
qcdata[PID == "INC", L_superieure := rep(inc_upper, 2)]
qcdata[PID == "ENC", L_superieure := rep(enc_upper, 2)]
qcdata[PID == "IPC", L_superieure := rep(ipc_upper, 2)]
qcdata[PID == "EPC", L_superieure := rep(epc_upper, 2)]


# Determine if controls are within accepted range:
qcdata[, QCpass := fifelse(between(x = Mean_RQ, 
                                   lower = L_inferieure, 
                                   upper = L_superieure, 
                                   incbounds = TRUE), TRUE, FALSE)]

# Interpret QC results:
qcdata[, Interpretation := fifelse(QCpass == TRUE, "Acceptable", 
                                   fifelse(QCpass == FALSE & Mean_RQ < L_inferieure, 
                                           "Trop faible", "Trop élevé"))]

# Final text advice based on quality control results:
if(all(qcdata$QC_cvpass) == TRUE & all(qcdata$QCpass) == TRUE) {
  
  conseil <- cat("Ce test ELISA a été soumis à des procédures internes de contrôle de la qualité.\nLes résultats sont prêts à être communiqués.")
  
} else {
  
  conseil <- cat("Ce test ELISA a échoué aux procédures internes de contrôle de la qualité.\nEnvisagez de refaire le test de la plaque.\nLes problèmes suivants doivent être vérifiés et résolus lors du dépannage:\n
A. Si les résultats sont trop faibles:\n- Le temps d'incubation était trop court\n- La température d'incubation était trop basse\n- Le tampon de lavage n'a pas été complètement retiré des puits\n- Il y a eu une rupture de la chaîne du froid pendant le stockage des réactifs\n
B. Si les résultats sont trop élevés:\n- Le temps d'incubation était trop long\n- La température d'incubation (ambiante) était trop élevée\n- Le lavage des assiettes était insuffisant (temps trop court ou pas assez de tampon de lavage utilisé)\n- Une contamination croisée s'est produite (si les valeurs du contrôle négatif sont trop élevées)\n 
C. Si les Coefficients de variation (CV) sont trop élevés:\n- Les broches du lecteur ELISA peuvent etre mal alignées\n-Les composants optiques du lecteur ELISA peuvent être contaminés par de la poussière")
  
}

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
                        Limites = c(paste0(expression("\u0394"), " 2 SD"), 
                                    "Moyen", 
                                    paste0(expression("\u0394"), " 2 SD")))

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
                        Limites = c(paste0(expression("\u0394"), " 2 SD"), 
                                    "Moyen", 
                                    paste0(expression("\u0394"), " 2 SD")))

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

#ggplotly(EPCplot)


###################################
### HEAT MAP OF ELISA RESULTS ###

elisadt[, Rowlabs := factor(Rows)]
elisadt[, Colabs := factor(Cols)]


platemap <- ggplot(elisadt, aes(x = Colabs, y = ordered(Rowlabs, levels = rev(levels(Rowlabs))))) + 
  geom_tile(fill = "white", na.rm = TRUE) +
  geom_point(aes(colour = Resultats, size = 200)) +
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




## To do:

# Add QC table
# Export Rmd report with all this stuff in it but also show on dashboard
# Export ELISA data ready for import to another system













