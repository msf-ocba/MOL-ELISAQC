#' Fetch lab results from Go.Data in a flat table
#' 
#' @authors c(Amy Mikhail, \email{amy.mikhail@@gmail.com};
#'            James Fuller, \email{fullerj@@who.int})
#' 
#' @description 
#' This function imports lab results from Go.Data into R as a flat table (no 
#' nesting, no sub-lists) to facilitate down-stream analysis.  Full credit to 
#' James Fuller (WHO Go.Data) who wrote the import section of this function. 
#' Variable sub-setting for line lists is specific to the MOL Go.Data instance.  
#' 
#' @note 
#' Pre-requisites:
#'  * The user must already have an account in the Go.Data database
#'  * The user must know their Go.Data user credentials 
#'  * The user must know the url for their Go.Data instance
#' @md
#'  
#' @param godataids List object containing Go.Data url, outbreak ID and token
#' 
#' @returns data.table of flat lab results imported from Go.Data into R 
#' 
#' @import httr jsonlite data.table dplyr
#' 
#' @examples 
#'  \dontrun{
#'  # Fetch lab results:
#'  labres <- godata_getlablist(godataids = godata_getids())
#'  }
#' 
#' @export
godata_getlablist <- function(godataids) {
  
  ###########################################################
  # Extract Go.Data credentials from ID object:
  url = godataids$url
  outbreak = godataids$outbreak
  token = godataids$token
  
  ###########################################################
  # Import the data from Go.Data in a flat table:
  
  # Submit an export request to the system:
  export_request = GET(paste0(url,
                              "api/outbreaks/",
                              outbreak,
                              "/lab-results/export",
                              "?filter=%7B%22where%22%3A%7B%22useDbColumns%22%3A%22true%22%2C%20%22dontTranslateValues%22%3A%22true%22%2C%20%22jsonReplaceUndefinedWithNull%22%3A%22true%22%20%7D%7D",
                              "&type=csv",
                              "&access_token=",
                              token))
  
  # Check the status code of the request:
  if (export_request$status_code != 200) {
    stop(paste0('Error code: ', export_request$status_code))
  } else if (export_request$status_code == 200) {
    
    # Get the Request ID
    export_request_id = export_request %>%
      content("text") %>% 
      fromJSON(flatten = TRUE) %>% 
      unlist() %>% 
      unname()
    
    message(paste0("Export Request ID: ", export_request_id))
    
    # Check status of request:
    Sys.sleep(1) # wait 1 second before checking status
    export_request_status = GET(paste0(url,
                                       "api/export-logs/",
                                       export_request_id,
                                       "?access_token=",
                                       token)) %>%
      content("text") %>% fromJSON(flatten = TRUE)
    
    message(paste0("Export Request Status: ", export_request_status$statusStep))
    
    # Keep checking status every 3 seconds until it is finished
    if (export_request_status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
      Sys.sleep(3)
      export_request_status = GET(paste0(url,
                                         "api/export-logs/",
                                         export_request_id,
                                         "?access_token=",
                                         token)) %>%
        content("text") %>% 
        fromJSON(flatten = TRUE)
      message(paste0("Export Request Status: ", export_request_status$statusStep))
    }
    
    # If the status is finished, then download the export
    if (export_request_status$statusStep == "LNG_STATUS_STEP_EXPORT_FINISHED") {
      df.content = GET(paste0(url,
                              "api/export-logs/",
                              export_request_id,
                              "/download?access_token=",
                              token)) %>%
        content("text")
      df = read.csv(textConnection(df.content), na.strings = c(""))
    }
    
  }
  
  # Fix one strange variable name:
  names(df)[names(df) %in% "X_id"] = "id"
  
  ###########################################################
  # Clean up lab line list:
  
  # Convert to a data.table:
  dt = data.table(df)
  
  # Lab variables to retain for line list:
  labvars2keep = c("personId", 
                    "id",
                    "person.person.visualId",
                    "sampleIdentifier", 
                    "dateSampleTaken",                                          
                    "dateSampleDelivered",                                      
                    "dateTesting",                                              
                    "dateOfResult",                                             
                    "labName",
                    "sampleType",                                               
                    "testType",                                                 
                    "testedFor", 
                    "molecule..MV.1.",
                    "unites..MV.1.",
                    "quantitativeResult", 
                    "result",                                                   
                    "status")
  
  # Subset data set to the relevant variables:
  dt = subset(dt, select = labvars2keep)
  
  # Rename variables for export to MSP:
  data.table::setnames(dt, old = names(dt), new = c("godata_pid", 
                                                    "godata_lid",
                                                    "mol_pid", 
                                                    "mol_lid",
                                                    "date_prelevement",
                                                    "date_livrelabo",
                                                    "date_test",
                                                    "date_resultat",
                                                    "laboratoire",
                                                    "echantillon",
                                                    "test",
                                                    "maladie",
                                                    "molecule",
                                                    "unites",
                                                    "resultat",
                                                    "interpretation",
                                                    "statut"))
  
  # Identify dates to format:
  dates2format = subset(names(dt), grepl("^date_", names(dt)))
  
  # Format these columns:
  dt[, (dates2format) := lapply(.SD, as.POSIXct, format = "%Y-%m-%dT%H:%M:%S.000Z"), .SDcols = dates2format]
  
  # Remove prefix text from option columns:
  dt[, laboratoire := gsub("^LNG_REFERENCE_DATA_CATEGORY_LAB_NAME_", "", laboratoire)]
  dt[, echantillon := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_TYPE_OF_SAMPLE_", "", echantillon))]
  dt[, test := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_TYPE_OF_LAB_TEST_", "", test))]
  dt[, interpretation := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_LAB_TEST_RESULT_", "", interpretation))]
  dt[, statut := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_LAB_TEST_RESULT_STATUS_", "", statut))]
  
  # Translate responses to French where necessary:
  dt[, echantillon := dplyr::recode(echantillon, serum = "serum")]
  dt[, test := dplyr::recode(test, elisa_anticorps = "ELISA anticorps")]
  dt[, interpretation := dplyr::recode(interpretation, positive = "positif", 
                                       negative = "negatif", 
                                       inconclusive = "indetermine")]
  dt[, statut := dplyr::recode(statut, in_progress = "en cours", completed = "termine")]
  
  # Set key for merging with patient data:
  data.table::setkey(dt, godata_pid)
  
  # Return cleaned lab line list:
  return(dt)
  
}

