#' Update lab records in Go.Data with ELISA results
#' 
#' @authors c(Amy Mikhail, \email{amy.mikhail@@gmail.com};
#'            James Fuller, \email{fullerj@@who.int})
#' 
#' @description 
#' This function takes Go.Data user credentials and ELISA results (from ELISAQC)
#' as input and passes these results to Go.Data.  Both the main lab records and 
#' relevant fields in the lab questionnaire are updated.  
#' 
#' @note 
#' 1. Data in the Go.Data MongoDB database is held in a complex structure; 
#' specifically questionnaire questions are nested.  This function updates 
#' these fields in a two-step process; first by un-nesting the relevant fields
#' to update, then once updated, the updated values are passed back to the 
#' json format and PUT back into Go.Data.  The code for the second step 
#' (updating the json) was developed by James Fuller (WHO Go.Data team). 
#' 
#' 2. Note that column names are hard-coded (derived from ELISAQC and Go.Data 
#' core data, case and lab questionnaires, respectively).
#'  
#' Pre-requisites:
#'  * The user must already have an account in the Go.Data database
#'  * The user must know their Go.Data user credentials 
#'  * The user must know the url for their Go.Data instance
#' @md
#'  
#' @param godataids List object containing Go.Data url, outbreak ID and token
#' @param elisaresults data.table of ELISA results from ELISAQC
#' 
#' @returns updates lab records in Go.Data (only shows warnings and errors in R) 
#' 
#' @import httr jsonlite data.table dplyr
#' 
#' @examples 
#'  \dontrun{
#'  # Update lab results:
#'  godata_updatelab(godataids = godata_getids(), elisaresults = elisaout)
#'  }
#' 
#' @export
godata_updatelab <- function(godataids, elisaresults){
  
  ###########################################################
  # Extract Go.Data credentials from ID object:
  url = godataids$url
  outbreak = godataids$outbreak
  token = godataids$token
  
  ###########################################################
  # Extract case data to fetch sample details from case form:
  cases = httr::GET(paste0(url,
                           "api/outbreaks/",
                           outbreak,
                           "/cases?"), 
                    add_headers(Authorization = paste("Bearer",
                                                      token, 
                                                      sep = " "))) %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    as.data.table()
  
  # Convert NULLs to NA in the cases dataset:
  cases[, names(cases) := lapply(.SD, null2na)]
  
  # Convert questionnaire date columns from list to a vector:
  cases[, questionnaireAnswers.date_prelevement := list2vector(questionnaireAnswers.date_prelevement)]
  cases[, questionnaireAnswers.date_livraison_mol := list2vector(questionnaireAnswers.date_livraison_mol)]
  
  ###########################################################
  # Extract lab data and update with ELISA results:
  lab_results = httr::GET(paste0(url,
                                 "api/outbreaks/",
                                 outbreak,
                                 "/lab-results/aggregate"), 
                          add_headers(Authorization = paste("Bearer",
                                                            godata_getids()$token,
                                                            sep = " "))) %>%
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    as.data.table()
  
  # Set language tokens for columns to change:
  tokengen <- "LNG_REFERENCE_DATA_CATEGORY_"
  tokenlab <- "LAB_NAME_"
  tokenresult <- "LAB_TEST_RESULT_"
  tokenstatus <- "LAB_TEST_RESULT_STATUS_"
  tokentest <- "TYPE_OF_LAB_TEST_"
  tokensample <- "TYPE_OF_SAMPLE_"
  
  # Sort data.tables by case ID:
  data.table::setkey(elisaresults, SID)
  data.table::setkey(cases, visualId)
  data.table::setkeyv(lab_results, c("person.visualId", "sampleIdentifier"))
  
  # Subset only new lab results to update:
  newdata = subset(lab_results, sampleIdentifier %in% elisaresults$SID)
  
  # Fill in NULL values with NA:
  newdata[, names(newdata) := lapply(.SD, null2na)]
  
  # Populate the core lab fields for sample description:
  newdata[, labName := paste0(tokengen, tokenlab, "MOL")]
  newdata[, sampleType := paste0(tokengen, tokensample, "SERUM")]
  newdata[, testType := paste0(tokengen, tokentest, "ELISA_ANTICORPS")]
  newdata[, testedFor := unique(elisaresults$Pathogene)]
  
  # Populate the core lab date fields:
  newdata[cases, dateSampleTaken := questionnaireAnswers.date_prelevement, 
          on = c("person.visualId" = "visualId")]
  newdata[cases, dateSampleDelivered := questionnaireAnswers.date_livraison_mol, 
          on = c("person.visualId" = "visualId")]
  
  # Reformat ELISA time stamp to match Go.Data:
  dtstamp = unique(elisaresults$Date_resultat)
  dtstamp = format(dtstamp, "%Y-%m-%dT%H:%M:%S.000Z")
  
  # Update lab data with timestamp for date of testing and results:
  newdata[, dateTesting := dtstamp]
  newdata[, dateOfResult := dtstamp]
  
  # Populate the result fields:
  newdata[elisaresults, quantitativeResult := as.character(Valeur), 
          on = c("sampleIdentifier" = "SID")]
  newdata[elisaresults, result := paste0(tokengen, tokenresult, 
                                         fifelse(Interpretation == "negatif", "NEGATIVE", 
                                                 fifelse(Interpretation == "positif", "POSITIVE", "INCONCLUSIVE"))), 
          on = c("sampleIdentifier" = "SID")]
  newdata[elisaresults, status := fifelse(CQ == "reussi", 
                                          paste0(tokengen, tokenstatus, "COMPLETED"), 
                                          paste0(tokengen, tokenstatus, "IN_PROGRESS")), 
          on = c("sampleIdentifier" = "SID")]
  
  # Convert lab questionnaire fields from lists to a vector:
  newdata[, questionnaireAnswers.nom_laborantin := as.character(list2vector(questionnaireAnswers.nom_laborantin))]
  newdata[, questionnaireAnswers.molecule := as.character(list2vector(questionnaireAnswers.molecule))]
  newdata[, questionnaireAnswers.unites := as.character(list2vector(questionnaireAnswers.unites))]
  newdata[, questionnaireAnswers.puit := as.character(list2vector(questionnaireAnswers.puit))]
  newdata[, questionnaireAnswers.resultats_brutes := as.character(list2vector(questionnaireAnswers.resultats_brutes))]
  newdata[, questionnaireAnswers.CQ := as.character(list2vector(questionnaireAnswers.CQ))]
  
  
  # Populate the lab questionnaire fields:
  newdata[, questionnaireAnswers.nom_laborantin := unique(elisaresults$Operateur)]
  newdata[, questionnaireAnswers.molecule := unique(elisaresults$Molecule)]
  newdata[, questionnaireAnswers.unites := rep("ratio DO", nrow(newdata))]
  newdata[elisaresults, questionnaireAnswers.puit := Puit, on = c("sampleIdentifier" = "SID")]
  newdata[elisaresults, questionnaireAnswers.resultats_brutes := as.character(Resultats_brutes), 
          on = c("sampleIdentifier" = "SID")]
  newdata[, questionnaireAnswers.CQ := unique(elisaresults$CQ)]
  
  # Reformat logical columns to character before export:
  coltypes_logical = sapply(newdata, is.logical)
  logicols = names(coltypes_logical[coltypes_logical == TRUE])
  newdata[, (logicols) := lapply(.SD, as.character), .SDcols = logicols]
  
  # Reformat numeric columns to character before export:
  coltypes_numeric = sapply(newdata, is.numeric)
  numcols = names(coltypes_numeric[coltypes_numeric == TRUE])
  newdata[, (numcols) := lapply(.SD, as.character), .SDcols = numcols]
  
  # Select only id columns and updated columns to push to Go.Data:
  lab_updates <- subset(newdata, select = c("personId", 
                                            "id",
                                            "outbreakId",
                                            "sampleIdentifier",
                                            "labName",
                                            "sampleType",
                                            "testType",
                                            "testedFor",
                                            "dateSampleTaken",
                                            "dateSampleDelivered",
                                            "dateTesting",
                                            "dateOfResult",
                                            "quantitativeResult",
                                            "result",
                                            "status",
                                            "questionnaireAnswers.nom_laborantin",
                                            "questionnaireAnswers.molecule",
                                            "questionnaireAnswers.unites",
                                            "questionnaireAnswers.puit",
                                            "questionnaireAnswers.resultats_brutes",
                                            "questionnaireAnswers.CQ"))
  
  # Identify questionnaire variable names:
  questionnaireFields = subset(names(lab_updates), 
                               grepl("^questionnaireAnswers.", names(lab_updates)) == TRUE)
  
  # Identify core variable names:
  coreFields = subset(names(lab_updates), 
                      !grepl("^questionnaireAnswers.", names(lab_updates)) == TRUE)
  
  # Strip questionnaire variable names of the prefix:
  setnames(lab_updates, 
           old = questionnaireFields, 
           new = gsub("^questionnaireAnswers.", "", questionnaireFields))
  
  # Strip list of questionnaire variable names of prefix:
  questionnaireFields = gsub("^questionnaireAnswers.", "", questionnaireFields)
  
  # Convert lab updates to a data.frame for pushing back to Go.Data:
  lab_updates = as.data.frame(lab_updates)
  
  ###########################################################
  # Extract lab data in json format to pass results to:
  lab_content = httr::GET(paste0(url,
                                 "api/outbreaks/",
                                 outbreak,
                                 "/lab-results/aggregate"), 
                          add_headers(Authorization = paste("Bearer",
                                                            godata_getids()$token, sep = " "))) %>%
    content()
  
  # Update lab results one by one and push back to Go.Data:
  for (i in 1:nrow(lab_updates)) {
    
    # Get case IDs to update:
    person_id_i = lab_updates$personId[i]
    
    # Get lab record IDs to update:
    lab_id_i = lab_updates$id[i]
    
    
    # Get current snapshot of Go.Data for each lab record:
    for (x in 1:length(lab_content)) {
      
      if (lab_content[[x]]$id == lab_id_i) {
        
        # Get current lab data
        lab_content_i = lab_content[[x]]
        
        # Create new data packet for the lab records
        new_content_i = lab_content_i
        
        # Add new values to the new data packet and do this column by column:
        for (fq in questionnaireFields) {
          new_content_i$questionnaireAnswers[fq][[1]][[1]]$value = lab_updates[fq][[1]][i]
        }
        for (fc in coreFields) {
          new_content_i[fc][[1]] = lab_updates[fc][[1]][i]
        }
        
        # PUT data into Go.Data:
        put_request = PUT(paste0(url,
                                 "api/outbreaks/",
                                 outbreak,
                                 "/cases/",
                                 person_id_i,
                                 "/lab-results/", 
                                 lab_id_i),
                          add_headers(Authorization = paste("Bearer",
                                                            godata_getids()$token, 
                                                            sep = " ")),
                          body = new_content_i,
                          encode = "json")
        
        warn_for_status(put_request)
        
        rm(lab_content_i, new_content_i, put_request)
      }
    }
    
  }
  
}
