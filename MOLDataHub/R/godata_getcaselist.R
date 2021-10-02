#' Fetch case data from Go.Data in a flat table
#' 
#' @authors c(Amy Mikhail, \email{amy.mikhail@@gmail.com};
#'            James Fuller, \email{fullerj@@who.int})
#' 
#' @description 
#' This function imports case data from Go.Data into R as a flat table (no 
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
#' @returns data.table of flat case data imported from Go.Data into R 
#' 
#' @import httr jsonlite data.table dplyr
#' 
#' @examples 
#'  \dontrun{
#'  # Fetch lab results:
#'  caseres <- godata_getcaselist(godataids = godata_getids())
#'  }
#' 
#' @export
godata_getcaselist <- function(godataids) {
  
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
                              "/cases/export",
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
                                       export_request_id,"?access_token=",
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
  # Clean up case line list:
  
  # Convert to a data.table:
  dt = data.table(df)
  
  # Lab variables to retain for line list:
  casevars2keep = c("id",
                   "visualId",
                   "firstName",
                   "middleName",
                   "lastName",
                   "gender",
                   "dob",
                   "age.age.years",
                   "age.age.months",
                   "pregnancyStatus",
                   "addresses.geoLocation.lat..1.",
                   "addresses.geoLocation.lng..1.",
                   "addresses.geoLocationAccurate..1.",
                   "addresses.addressLine1..1.",
                   "addresses.city..1.",
                   "addresses.locationId..1..Lieu.du.parent..4.",
                   "addresses.locationId..1..Lieu.du.parent..3.",
                   "addresses.locationId..1..Lieu.du.parent..2.",
                   "geozone..MV.1.",
                   "besoin_gardien..MV.1.",
                   "nom_gardien_parent..MV.1.",
                   "prenom_gardien_parent..MV.1.",
                   "lien_accompagnant..MV.1.",
                   "addresses.phoneNumber..1.",
                   "addresses.emailAddress..1.",
                   "dateRanges.typeId..1.",
                   "dateRanges.centerName..1.",
                   "dateRanges.startDate..1.",
                   "dateRanges.endDate..1.",
                   "dateOfOnset",
                   "isDateOfOnsetApproximate",
                   "dateOfReporting",
                   "presence_fievre..MV.1.",
                   "date_fievre1..MV.1.",
                   "id_fievre1..MV.1.",
                   "temperature_fievre..MV.1.",
                   "presence_exantheme..MV.1.",
                   "date_eruption..MV.1.",
                   "id_eruption..MV.1.",
                   "presence_toux..MV.1.",
                   "date_ds_toux..MV.1.",
                   "categorie_toux..MV.1.",
                   "autres_symptomes..MV.1.",
                   "presence_vac_rougeole..MV.1.",
                   "doses_vaccin_rougeole..MV.1.",
                   "date_vaccin_rougeole..MV.1.",
                   "presence_vac_rubeole..MV.1.",
                   "doses_vaccin_rubeole..MV.1.",
                   "date_vaccin_rubeole..MV.1.",
                   "presence_vac_coqueluche..MV.1.",
                   "doses_vaccin_coqueluche..MV.1.",
                   "date_vaccin_coqueluche..MV.1.",
                   "riskLevel",
                   "riskReason",
                   "poste_quotidienne..MV.1.",
                   "occupation",
                   "profession_specifier..MV.1.",
                   "nom_ecole..MV.1.",
                   "date_ecole..MV.1.", 
                   "nom_educ_niveau3..MV.1.",
                   "date_educ_niveau3..MV.1.",
                   "nom_travail..MV.1.",
                   "date_travail..MV.1.",                                           
                   "contexte_de_transmission..MV.1.",
                   "contexte_details..MV.1.",
                   "nom_enqueteur..MV.1.",
                   "prenom_enqueteur..MV.1.", 
                   "role_enqueteur..MV.1.", 
                   "tel_enqueteur..MV.1.", 
                   "email_enqueteur..MV.1.",
                   "classification",
                   "prelevement_sang..MV.1.",
                   "prelevement_ecouvillon..MV.1.",
                   "preleveent_pustules..MV.1.",
                   "temperature_indigo..MV.1.")
  
 
  # Subset data set to the relevant variables:
  dt = subset(dt, select = casevars2keep)
  
  # Rename variables for export to MSP:
  data.table::setnames(dt, old = names(dt), new = c("godata_pid", 
                                                    "mol_pid",
                                                    "prenom",
                                                    "nom_millieu",
                                                    "nom_famille",
                                                    "sexe",
                                                    "date_naissance",
                                                    "age_annees",
                                                    "age_mois",
                                                    "enceinte",
                                                    "latitude",
                                                    "longitude",
                                                    "geolocation_precise",
                                                    "cas_addresse",
                                                    "cas_ville",
                                                    "code_structure_sanitaire",
                                                    "code_district_sanitaire",
                                                    "code_region_sanitaire",
                                                    "geozone",
                                                    "accompagnant_besoin",
                                                    "accompagnant_nom",
                                                    "accompagnant_prenom",
                                                    "accompagnant_lien",
                                                    "cas_tel",
                                                    "cas_email",
                                                    "structure_sante_type",
                                                    "structure_sante_nom",
                                                    "structure_sante_date_arrivee",
                                                    "structure_sante_date_depart",
                                                    "date_debut_symptomes",
                                                    "debut_symptomes_precise",
                                                    "date_enquete",
                                                    "fievre_presence",
                                                    "fievre_date",
                                                    "fievre_methode",
                                                    "fievre_temperature",
                                                    "exantheme_presence",
                                                    "exantheme_date",
                                                    "exantheme_categorie",
                                                    "toux_presence",
                                                    "toux_date",
                                                    "toux_categorie",
                                                    "symptomes_nonliste",
                                                    "vaccin_rougeole_presence",
                                                    "vaccin_rougeole_doses",
                                                    "vaccin_rougeole_date",
                                                    "vaccin_rubeole_presence",
                                                    "vaccin_rubeole_doses",
                                                    "vaccin_rubeole_date",
                                                    "vaccin_coqueluche_presence",
                                                    "vaccin_coqueluche_doses",
                                                    "vaccin_coqueluche_date",
                                                    "risque_niveau",
                                                    "risque_justification",
                                                    "poste_quotidienne",
                                                    "occupation_categorie",
                                                    "occupation_specifier",
                                                    "ecole_nom",
                                                    "ecole_date",
                                                    "education_niveau3_nom",
                                                    "education_niveau3_date",
                                                    "lieu_travail_nom",
                                                    "lieu_travail_date",
                                                    "contexte_transmission",
                                                    "contexte_details",
                                                    "enqueteur_nom",
                                                    "enqueteur_prenom",
                                                    "enqueteur_role",
                                                    "enqueteur_tel",
                                                    "enqueteur_email",
                                                    "classification_cas",
                                                    "prelevement_sang",
                                                    "prelevement_ecouvillon",
                                                    "prelevement_pustules",
                                                    "prelevement_indigo_temp"))
  
  # Identify dates to format:
  dates2format = subset(names(dt), grepl("date", names(dt)))
  
  # Format these columns:
  dt[, (dates2format) := lapply(.SD, as.POSIXct, format = "%Y-%m-%dT%H:%M:%S.000Z"), .SDcols = dates2format]
  
  # Remove prefix text from option columns:
  dt[, sexe := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_GENDER_", "", sexe))]
  dt[, enceinte := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_PREGNANCY_STATUS_", "", enceinte))]
  dt[, structure_sante_type := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_PERSON_DATE_TYPE_", "", structure_sante_type))]
  dt[, structure_sante_nom := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_CENTRE_NAME_", "", structure_sante_nom))]
  dt[, classification_cas := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_CASE_CLASSIFICATION_", "", classification_cas))]
  dt[, risque_niveau := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_RISK_LEVEL_", "", risque_niveau))]
  dt[, occupation_categorie := tolower(gsub("^LNG_REFERENCE_DATA_CATEGORY_OCCUPATION_", "", occupation_categorie))]
  
  
  # Translate responses to French where necessary:
  dt[, sexe := dplyr::recode(sexe, female = "feminin", male = "masculin")]
  
  dt[, enceinte := dplyr::recode(enceinte, 
                                 none = "pas pertinent", 
                                 not_pregnant =  "pas enceinte", 
                                 yes_first_trimester = "enceinte T1", 
                                 yes_second_trimester = "enceinte T2", 
                                 yes_third_trimester = "enceinte T3", 
                                 yes_trimester_unknown = "enceinte T inconnu")]
  
  dt[, structure_sante_type := dplyr::recode(structure_sante_type, 
                                             primary_health_care_phc_gp_etc_visit = "soins de sante primaires", 
                                             hospitalisation = "hopital")]
  
  dt[, classification_cas := dplyr::recode(classification_cas, 
                                           suspect = "suspect", 
                                           probable = "probable", 
                                           confirmed = "confirme")]
  
  dt[, risque_niveau := dplyr::recode(risque_niveau, 
                                      `1_low` = "bas", 
                                      `2_medium` = "moyen", 
                                      `3_high`= "eleve")]
  
  dt[, occupation_categorie := dplyr::recode(occupation_categorie, 
                                             farmer = "agriculteur", 
                                             other = "autre", 
                                             butcher = "boucher", 
                                             hunter = "chasseur", 
                                             taxi_driver = "conducteur de taxi", 
                                             child = "enfant",
                                             civil_servant = "fonctionnaire d´etat",
                                             traditional_healer = "tradipracticien", 
                                             health_laboratory_worker = "laborantin", 
                                             unknown = "inconnu", 
                                             religious_leader = "leader religieux",
                                             teacher = "professeur", 
                                             working_with_animals = "travail avec des animaux", 
                                             health_care_worker = "agent de sante",
                                             student = "etudiant")]
  
  
  
  
  
  # Set key for merging with patient data:
  data.table::setkey(dt, godata_pid)
  
  # Return cleaned lab line list:
  return(dt)
  
}

