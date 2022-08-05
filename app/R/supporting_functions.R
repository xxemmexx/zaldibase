printToastMessage <- function(aModalTitle) {
  
  switch (aModalTitle,
  "Effacer profil" = "Le profil a été supprimé",
  "Modification du profil" = "Le profil a bien été modifié",
  "Registrer un nouveau patient" = "Les données ont bien été enregistrées",
  "Registrer un nouveau dossier" = "Les données ont bien été enregistrées",
  "C'est fait!")
  
}

printButtonLabel <- function(aModalTitle) {
  switch (aModalTitle,
          "Modification du profil" = "Modifier",
          "Modification du dossier" = "Modifier",
          "Registrer un nouveau patient" = "Ajouter patient",
          "Registrer un nouveau dossier" = "Ajouter dossier",
          "C'est bon!")
}

deliverAge <- function(aDateNaissance) {
  interval(ymd(aDateNaissance), today()) %/% years(1)
}

placePatientUID <- function(aUID) {
  
  if(is.null(aUID)) {
    thisPatientUID <- ' '
  } else {
    thisPatientUID <- aUID
  }
  
  thisPatientUID
}

deliverUID <- function(aValueOnHold) {
  
  thisUID <- NA
  
  if (!is.null(aValueOnHold)) {
    thisUID <- aValueOnHold$uid
  } 
  
  thisUID
}

deliverHopital <- function(aHospital, anOtherHospital) {
  
  if(aHospital == "Autre...") {
    return(anOtherHospital)
  }
  
  aHospital
}

deliverCreator <- function(aValueOnHold, aUsername) {
  
  thisCreator <- aUsername
  
  if (!is.null(aValueOnHold)) {
    thisCreator <- aValueOnHold$created_by
  } 
  
  thisCreator
}

deliverCreationTime <- function(aValueOnHold, aTimeNow) {
  
  thisTime <- aTimeNow
  
  if (!is.null(aValueOnHold)) {
    thisTime <- aValueOnHold$created_at
  } 
  
  thisTime
}

writeISODate <- function(aDateString) {
  
  aDateString %>%
    as.Date() %>%
    format('%Y-%m-%d')
}

computeDateGarde <- function(aTimestamp) {
  
  timestampNowParts <- str_split(aTimestamp, " ")
  timestampNow <- timestampNowParts[[1]][[2]]
  todayStr <- timestampNowParts[[1]][[1]]
  
  hoursMinutesSecondsList <- timestampNow %>%
    str_split(":")
  
  hourNow <- hoursMinutesSecondsList[[1]][[1]] %>%
    as.numeric()
  
  if(hourNow > 7) {
    dateGarde <- as.Date(todayStr)
  } else {
    dateGarde <- as.Date(todayStr) - 1
  }
  
  dateGarde
  
}

generateIdentifier <- function(aFirstName, aSurname) {
  
  letters <- paste0(str_to_upper(str_sub(aFirstName, 1, 1)),
                    str_to_upper(str_sub(aSurname, 1, 1)))
  
  Sys.time() %>%
    str_replace_all(":", "") %>%
    str_replace_all("-", "") %>%
    str_replace_all(" ", letters) %>%
    str_sub(3, -1)
  
}

writeQuery <- function(aUID, 
                       aNom, 
                       aPrenom, 
                       aDateNaissance, 
                       aPhoneNumber,
                       aPathologie,
                       aPreDecision, 
                       aContactPerson,
                       aContactPhone,
                       aContactEmail,
                       aHospital,
                       aCreatedAt, 
                       aCreatedBy, 
                       aModifiedAt, 
                       aModifiedBy, 
                       aStatement = c("insert", "update")) {
  
  thisQuery = switch(aStatement,
                     
                     "insert" = paste0("INSERT INTO patients (uid, nom, prenom, 
                     date_naissance, phone_number_patient, pathologie, pre_decision, 
                     contact_person, contact_phone, contact_email, hopital, created_at,
                     created_by, modified_at, modified_by) VALUES ('",
                     aUID, "', '", aNom, "', '", aPrenom, "', '", aDateNaissance, "', '",
                     aPhoneNumber, "', '", aPathologie, "', '", aPreDecision, "', '",
                     aContactPerson, "', '", aContactPhone, "', '", aContactEmail, "', '",
                     aHospital, "', '",
                     aCreatedAt, "', '", aCreatedBy, "', '", 
                     aModifiedAt, "', '", aModifiedBy, "');"),
                     
                     "update" = paste0("UPDATE patients SET nom='", aNom, 
                                       "', prenom='", aPrenom, 
                                       "', date_naissance='", aDateNaissance,
                                       "', phone_number_patient='", aPhoneNumber,
                                       "', pathologie='", aPathologie, 
                                       "', pre_decision='", aPreDecision,
                                       "', contact_person='", aContactPerson,
                                       "', contact_phone='", aContactPhone,
                                       "', contact_email='", aContactEmail,
                                       "', hopital='", aHospital,
                                       "', created_at='", aCreatedAt, 
                                       "', created_by='", aCreatedBy, 
                                       "', modified_at='", aModifiedAt, 
                                       "', modified_by='", aModifiedBy,
                                       "' WHERE uid='", aUID, "';")
                     )
  thisQuery
  
}


