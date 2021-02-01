### GENERATE EXAMPLE SAMPLE ID LABELS FOR ELISAQC APP TESTING ###

# Create example table of Sample IDs in 96-well plate configuration for QC fail example data:
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