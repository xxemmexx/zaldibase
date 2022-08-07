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

buildDecisionBanner <- function(aPreDecision, aDefDecision) {
  if(aDefDecision == '') {
    paste0('<div class="pull-right"><b>Décision préliminaire: ', aPreDecision, '</b></div>')
  } else {
    paste0('<div class="pull-right""><b>Décision définitive: ', aDefDecision, '</b></div><br>',
           '<div class="pull-right", style="color:#A9A9A9;"><b>Décision préliminaire: ', aPreDecision, '</b></div>'
           )
  }
}

sd <- stamp_date("26/09/2018")

buildContactCard <- function(aPatientPhone, aContactPerson, aContactPhone, 
                             aContactEmail, aHospital, aCreatedAt, 
                             aCreatedBy, aUserTibble) {
  
  thisDate <- sd(ymd_hms(aCreatedAt))
  
  thisUser <- aUserTibble %>%
    filter(user == aCreatedBy) %>%
    select(name)
  
  paste0('<div>
        Enregistré(e) le ', thisDate, ' par ', thisUser[[1]], '<br><br>
         <h4><b>Données de la personne de contact</b></h4><br>
         <b>Nom: </b> ', aContactPerson, '<br>
         <b>Centre hospitalier d`origine: </b> ', aHospital, '<br>
         <b>Numéro de téléphone: </b> ', aContactPhone, '<br>
         <b>Email: </b>', aContactEmail, '<br><br>
         <h4><b>Données du patient</b></h4><br>
         <b>Numéro de téléphone: </b> ', aPatientPhone, '<br>
         </div>')
}

buildUnorderedList <- function(aList, aTitle) {
  
  n <- length(aList)
  
  items <- paste0('<li>', aList[[1]], '</li>')
  
  for(i in 2:n) {
    if(!(aList[[i]] == '')) {
      items <- paste0(items, '<li>', aList[[i]], '</li>')
    }
  }
  
  paste0('<h5 style = "padding-right: 150px;"><b>', aTitle,'</b></h5><br>', '<ul>', items, '</ul>')
}

buildParagraph <- function(aParagraph, aTitle) {
  
  paste0('<h5 style = "padding-right: 150px;"><b>', aTitle,'</b><br><br>', aParagraph, '</h5>')
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

deliverStandardOrCustom <- function(aStandard, anOtherValue, include = TRUE) {
  
  if(include) {
    
    if(aStandard == "Autre...") {
      return(anOtherValue)
    }
    
    aStandard
    
  } else {
    return('')
  }
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
                       aPathologie1,
                       aPathologie2,
                       aPathologie3,
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
                     date_naissance, phone_number_patient, pathologie_1, 
                     pathologie_2, pathologie_3, pre_decision, def_decision, 
                     contact_person, contact_phone, contact_email, hopital, 
                     has_definitive_decision, is_reported, created_at,
                     created_by, modified_at, modified_by) VALUES ('",
                     aUID, "', '", aNom, "', '", aPrenom, "', '", aDateNaissance, "', '",
                     aPhoneNumber, "', '", aPathologie1, "', '", aPathologie2, "', '",
                     aPathologie3, "', '", aPreDecision, "', '', '",
                     aContactPerson, "', '", aContactPhone, "', '", aContactEmail, "', '",
                     aHospital, "', 0, 0, '",
                     aCreatedAt, "', '", aCreatedBy, "', '", 
                     aModifiedAt, "', '", aModifiedBy, "');"),
                     
                     "update" = paste0("UPDATE patients SET nom='", aNom, 
                                       "', prenom='", aPrenom, 
                                       "', date_naissance='", aDateNaissance,
                                       "', phone_number_patient='", aPhoneNumber,
                                       "', pathologie_1='", aPathologie1, 
                                       "', pathologie_2='", aPathologie2, 
                                       "', pathologie_3='", aPathologie3, 
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


