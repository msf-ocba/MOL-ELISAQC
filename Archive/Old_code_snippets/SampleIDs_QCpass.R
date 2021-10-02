### GENERATE EXAMPLE SAMPLE ID LABELS FOR ELISAQC APP TESTING ###

# Create second set of example labels for the example data that will pass QC:
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
