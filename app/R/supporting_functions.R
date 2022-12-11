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

deliverAge <- function(aDateNaissance, aCreatedAtDate) {
  interval(ymd(aDateNaissance), sdISO(ymd_hms(aCreatedAtDate))) %/% years(1)
}


buildDecisionBanner <- function(aPreDecision, aDefDecision) {
  if(str_trim(aPreDecision) == '' || is.null(aPreDecision)) {
    preDecision = '-'
  } else {
    preDecision = aPreDecision
  }
  
  if(str_trim(aDefDecision) == '' || is.null(aDefDecision)) {
    defDecision = '-'
  } else {
    defDecision = aDefDecision
  }
  
  if(aDefDecision == '') {
    paste0('<div class="pull-right"><b>Décision préliminaire: ', preDecision, '</b></div>')
  } else {
    paste0('<div class="pull-right""><b>Décision définitive: ', defDecision, '</b></div><br>',
           '<div class="pull-right", style="color:#A9A9A9;"><b>Décision préliminaire: ', preDecision, '</b></div>'
           )
  }
}

convertUsernameToDisplayname <- function(aUsername, aUserTibble) {
  
  aUserTibble %>%
    filter(user == aUsername) %>%
    select(name)
}

buildContactCard <- function(aPatientPhone, aContactPerson, aContactPhone, 
                             aContactEmail, aHospital, aCreatedAt, 
                             aCreatedBy, aUserTibble) {
  
  thisDate <- sd(ymd_hms(aCreatedAt))
  thisUser <- convertUsernameToDisplayname(aCreatedBy, aUserTibble)
  
  paste0('<div>
         <h4><b>Données de la personne de contact</b></h4><br>
         <b>Nom: </b> ', aContactPerson, '<br>
         <b>Numéro de téléphone: </b> ', aContactPhone, '<br>
         <b>Email: </b>', aContactEmail, '<br>
         <b>Centre hospitalier d`origine: </b> ', aHospital, '<br><br>
         <h4><b>Données du patient</b></h4><br>
         <b>Numéro de téléphone: </b> ', aPatientPhone, '<br>
         <b>Enregistré(e) depuis le: </b>', thisDate, ' <br>
         <b> Garde: </b>', thisUser[[1]], '
         </div>')
}

hasAnyValues <- function(aList) {
  
  trimmedList <- lapply(aList, str_trim)
  
  isEmptyList <- TRUE
  
  for(i in trimmedList) {
    if(!(i == '')) {
      isEmptyList <- FALSE
      break
    }
  }
  
  return(!isEmptyList)
}


hasAnyEmptyValues <- function(aList) {
  
  trimmedList <- lapply(aList, str_trim)
  
  hasEmptyValues <- FALSE
  
  for(i in trimmedList) {
    if(i == '') {
      hasEmptyValues <- TRUE
      break
    }
  }
  
  return(hasEmptyValues)
}

buildUnorderedList <- function(aList, aTitle) {
  
  if(is.null(aList) || !hasAnyValues(aList)) {
    
    htmlList <- '<div></div>'
    
  } else {
    
    #print(aList)
    
    n <- length(aList)
    
    items <- paste0('<li>', aList[[1]], '</li>')
    
    for(i in 2:n) {
      if(!(str_trim(aList[[i]]) == '')) {
        items <- paste0(items, '<li>', aList[[i]], '</li>')
      }
    }
    
    htmlList <- paste0('<h5 style = "padding-right: 150px;"><b>', 
                       aTitle,
                       '</b></h5><br>', 
                       '<ul>', 
                       items, 
                       '</ul>')
  }
  
  return(htmlList)
}

buildParagraph <- function(aParagraph, aTitle) {
  
  if(aTitle == 'Histoire') {
    openTag = '<h4 '
    endTag = '</h4>'
  } else {
    openTag = '<h5 '
    endTag = '</h5>'
  }
  
  paste0(openTag, 
         'style = "padding-right: 150px;"><b>', 
         aTitle,
         '</b><br><br>', 
         aParagraph, 
         endTag)
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


translateDate <- function(aDateString) {
  date <- aDateString %>% 
    writeISODate() 
  
  paste0(day(date), " de ", deliverMonthName(month(date)), ", ", year(date))
}

deliverMonthName <- function(anInteger) {
  
  switch(anInteger,
         'Janvier',
         'Février',
         'Mars',
         'Avril',
         'Mai',
         'Juin',
         'Juillet',
         'Août',
         'Septembre',
         'Octobre',
         'Novembre',
         'Décembre')
  
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

makeTimestampNow <- function() {
  
  Sys.time() %>%
    str_replace_all(":", "") %>%
    str_replace_all("-", "") %>%
    str_replace_all(" ", "")
}

writeGardeQuery <- function(aUsername, aTimestamp) {
  
  paste0("INSERT INTO garde (modified_at, modified_by) VALUES ('",
         aTimestamp, "', '", aUsername, "');")
  
}

writeQuery <- function(aUID, 
                       aNom, 
                       aPrenom, 
                       aDateNaissance, 
                       aPhoneNumber,
                       aPathologie1,
                       aPathologie2,
                       aPathologie3,
                       aHistory,
                       aPreDecision, 
                       aDefDecision,
                       anExplanation,
                       aContactPerson,
                       aContactPhone,
                       aContactEmail,
                       aHospital,
                       hasCoagulation,
                       aCoagulant1,
                       aDernierePrise1,
                       aCoagulant2,
                       aDernierePrise2,
                       aCoagulant3,
                       aDernierePrise3,
                       aCreatedAt, 
                       aCreatedBy, 
                       aModifiedAt, 
                       aModifiedBy,
                       needsRendezVous = 0,
                       hasRendezVous = 0,
                       isClosed = 0,
                       isViewed = 0,
                       status,
                       aStatement = c("insert", "update")) {
  
  thisQuery = switch(aStatement,
                     
                     "insert" = paste0("INSERT INTO patients (uid, nom, prenom, 
                     date_naissance, phone_number_patient, pathologie_1, 
                     pathologie_2, pathologie_3, description_histoire, pre_decision, 
                     def_decision, 
                     explication, contact_person, contact_phone, contact_email, hopital, 
                     has_coagulation,
                     treat_coagulant_1, date_derniere_prise_1, treat_coagulant_2, 
                     date_derniere_prise_2, treat_coagulant_3, date_derniere_prise_3,
                     created_at, created_by, modified_at, modified_by, needs_rendezvous,
                     has_rendezvous, is_closed, is_viewed, status) VALUES ('",
                     aUID, "', '", aNom, "', '", aPrenom, "', '", aDateNaissance, "', '",
                     aPhoneNumber, "', '", aPathologie1, "', '", aPathologie2, "', '",
                     aPathologie3, "', '", aHistory, "', '", aPreDecision, "', '", aDefDecision, "', '",
                     anExplanation, "', '",
                     aContactPerson, "', '", aContactPhone, "', '", aContactEmail, "', '",
                     aHospital, "', ", hasCoagulation, ", '",
                     aCoagulant1, "', '", aDernierePrise1, "', '", aCoagulant2, "', '", 
                     aDernierePrise2, "', '", aCoagulant3, "', '", aDernierePrise3, "', '",
                     aCreatedAt, "', '", aCreatedBy, "', '", 
                     aModifiedAt, "', '", aModifiedBy, "', ", needsRendezVous, ", ",
                     hasRendezVous, ", ", isClosed, ", ", isViewed, ", 0);"),
                     
                     "update" = paste0("UPDATE patients SET nom='", aNom, 
                                       "', prenom='", aPrenom, 
                                       "', date_naissance='", aDateNaissance,
                                       "', phone_number_patient='", aPhoneNumber,
                                       "', pathologie_1='", aPathologie1, 
                                       "', pathologie_2='", aPathologie2, 
                                       "', pathologie_3='", aPathologie3, 
                                       "', description_histoire='", aHistory,
                                       "', pre_decision='", aPreDecision,
                                       "', def_decision='", aDefDecision,
                                       "', explication='", anExplanation,
                                       "', contact_person='", aContactPerson,
                                       "', contact_phone='", aContactPhone,
                                       "', contact_email='", aContactEmail,
                                       "', hopital='", aHospital,
                                       "', has_coagulation=", hasCoagulation,
                                       ", treat_coagulant_1='", aCoagulant1,
                                       "', date_derniere_prise_1='", aDernierePrise1,
                                       "', treat_coagulant_2='", aCoagulant2,
                                       "', date_derniere_prise_2='", aDernierePrise2,
                                       "', treat_coagulant_3='", aCoagulant3,
                                       "', date_derniere_prise_3='", aDernierePrise3,
                                       "', created_at='", aCreatedAt, 
                                       "', created_by='", aCreatedBy, 
                                       "', modified_at='", aModifiedAt, 
                                       "', modified_by='", aModifiedBy,
                                       "', needs_rendezvous=", needsRendezVous,
                                       ", has_rendezvous=", hasRendezVous,
                                       ", is_closed=", isClosed,
                                       ", is_viewed=", isViewed,
                                       ", status=", status,
                                       " WHERE uid='", aUID, "';")
                     )
  thisQuery
  
}

writeRendezVousQuery <- function(aPatientUid, aMode) {
  if(aMode == 'needsRendezvous') {
    query <- paste0("UPDATE patients SET 
                    needs_rendezvous = 1, 
                    status = 3
                    WHERE uid = '", aPatientUid, "';")
  }
  
  query
  
}

writeStaffDecisionQuery <- function(aStaffDecision, aPatientUid) {
  
  updateStaffDecision <- paste0("UPDATE patients SET staff_decision = '",
                                aStaffDecision, "'")
  
  updateStatus <- ", is_closed = 1, is_viewed = 1, needs_rendezvous = 1"
  
  whereClause <- paste0(" WHERE uid = '", aPatientUid, "';")
  
  if(aStaffDecision == "Rendez-vous / Suivi") {
    query <- paste0(updateStaffDecision, updateStatus, whereClause)
  } else {
    query <- paste0(updateStaffDecision, whereClause)
  }
  
  query
  
}

displayStatusName <- function(aStatus) {
  case_when(
    aStatus == 0 ~ "En cours...",
    aStatus == 1 ~  "À opérer",
    aStatus == 2 ~  "Opéré(e)",
    aStatus == 3 ~ "Attend sécretariat pour un rendez-vous",
    aStatus == 4 ~ "Rendez-vous accordé",
    aStatus == 5 ~  "En attente d'info supplémentaire",
  )
}

displayStatusCode <- function(aStatus) {
  case_when(
    aStatus == "En cours..." ~ 0,
    aStatus == "À opérer" ~ 1,
    aStatus == "Opéré(e)" ~ 2,
    aStatus == "Attend sécretariat pour un rendez-vous" ~ 3,
    aStatus == "Rendez-vous accordé" ~ 4,
    aStatus == "En attente d'info supplémentaire" ~ 5,
  )
}

transferFile <- function(aPathToFile, 
                         aUID, 
                         aHostAddress, 
                         index, 
                         extension, 
                         aDevice,
                         anAccessCode,
                         test = FALSE) {
  
  
  if(test) {
    zaldir <- "zalditest"
  } else {
    zaldir <- "zaldibase"
  }
  
  aTimestamp <- makeTimestampNow()
  
  aFileName <- paste0("Fichier_", index, "_", aTimestamp, ".", extension)
  
  remoteLocation <- paste0("sftp://",
                           aHostAddress,
                           "//home/tospiti/prog/R-projects/zaldibase/imgs/",
                           zaldir,
                           "/",
                           aUID,
                           "/",
                           aFileName)
  
  RCurl::ftpUpload(what = aPathToFile,
                   asText = FALSE,
                   to = remoteLocation,
                   port =22,
                   userpwd = paste(aDevice, anAccessCode, sep = ":"),
                   connecttimeout = 30,
                   ssl.verifypeer = FALSE, 
                   ssl.verifyhost = FALSE,
                   verbose = TRUE,
                   ftp.create.missing.dirs = TRUE)
}

saveFile <- function(aPathToFile, 
                     aUID, 
                     index, 
                     extension) {
  
  aTimestamp <- makeTimestampNow()
  
  aFileName <- paste0("Fichier_", index, "_", aTimestamp, ".", extension)
  
  targetDir <- paste0(imgsDir, aUID)
  
  if(!file.exists(targetDir)) {
    dir.create(targetDir)
  }
  
  targetLocation <- paste0(targetDir, "/", aFileName)
  
  aPathToFile %>%
  image_read() %>%
  image_write(path = targetLocation, format = extension)
}

isFile <- function(aString) {
  ifelse(aString == ".." | aString == "." | aString == "", FALSE, TRUE)
}

fetchPhotos <- function(aZaldibaseDir, 
                        aHostAddress, 
                        aPort, 
                        aDevice,
                        anAccessCode,
                        aListOfFilenames,
                        aMode,
                        aLocalDB) {
  
  targetDir <- paste0(tiffDir, aZaldibaseDir)
  
  if(!file.exists(targetDir)) {
    dir.create(targetDir)
  }
  
  if(length(aListOfFilenames) > 0) {
    if(!aLocalDB) {
      zalDir <- ifelse(aMode == 'test', 'zalditest', 'zaldibase')
      
      clearTmp()
      
      for (filename in aListOfFilenames) {
        
        anOrigin <- paste0(url = "sftp://", 
                           aHostAddress,
                           ":", 
                           aPort,
                           "/home/tospiti/prog/R-projects/zaldibase/imgs/",
                           zalDir,
                           "/",
                           aZaldibaseDir, 
                           "/",
                           filename)
        
        aDestination <- paste0(tmpDir, filename)
        
        
        # Open connection to aDestination
        con = file(aDestination, "wb")
        
        RCurl::getBinaryURL(url = anOrigin,
                            userpwd=paste(aDevice, anAccessCode, sep = ":"),
                            verbose = FALSE,
                            ssl.verifyhost = FALSE) %>%
          writeBin(con)
        
        # Close connection to aDestination
        close(con)
      }
    }
      
  
    frames <- NULL
    
    for (filename in aListOfFilenames) {
      
      print(noquote(paste0("Reading ", filename)))
      img <- image_read(getImageLocation(aLocalDB, aZaldibaseDir, filename))
      frames <- c(frames, img)
    }
    
    
    frames %>%
      image_join() %>%
      image_write(path = generateTiffFilename(targetDir, aZaldibaseDir, length(aListOfFilenames)), 
                  format = "tiff")
  } else {
    
    pathToTiff <- generateTiffFilename(targetDir, aZaldibaseDir, length(aListOfFilenames))
    print(pathToTiff)
    image_read('www/grenoble.jpeg') %>%
      image_write(path = pathToTiff, 
                  format = "tiff")
  }
  
}

generateTiffFilename <- function(aTargetDir, aPatientUuid, anInteger) {
  paste0(aTargetDir, '/', 
         aPatientUuid, '_', 
         anInteger, 
         '_.tiff')
}

fetchFiles <- function(aZaldibaseDir, 
                       aHostAddress, 
                       aPort, 
                       aDevice,
                       anAccessCode,
                       aMode = 'test',
                       aLocalDB = FALSE) {
  
  if(aLocalDB) {
    
    fetchFilesFromLocalDirectory(aZaldibaseDir)
    
  } else {
    
    fetchFilesFromRemoteLocation(aZaldibaseDir, 
                                 aHostAddress, 
                                 aPort, 
                                 aDevice,
                                 anAccessCode,
                                 aMode)
  }
  
}

fetchFilesFromLocalDirectory <- function(aZaldibaseDir) {
  
  print("Fetching filenames from local dir...")
  
  aDestination <- paste0(imgsDir, aZaldibaseDir, "/")
  
  filenames <- list.files(aDestination)
  
  # if (identical(filenames, character(0))) {
  #   return(list)
  # } 
  
  filenames
                         
}


fetchFilesFromRemoteLocation <- function(aZaldibaseDir, 
                                         aHostAddress, 
                                         aPort, 
                                         aDevice,
                                         anAccessCode,
                                         aMode) {
  
  print("Fetching filenames from server...")
  
  filenames <- NA
  
  zalDir <- ifelse(aMode == 'test', 'zalditest', 'zaldibase')
  
  aDestination <- paste0(url = "sftp://", 
                         aHostAddress,
                         ":", 
                         aPort,
                         "/home/tospiti/prog/R-projects/zaldibase/imgs/",
                         zalDir,
                         "/",
                         aZaldibaseDir, 
                         "/")
  
  tryCatch(filenames <- RCurl::getURL(aDestination, 
                                      userpwd = paste(aDevice, anAccessCode, sep=":"),
                                      dirlistonly = TRUE,
                                      verbose = FALSE,
                                      ssl.verifyhost = FALSE),
           error = function(e) {
             message("The Zaldibase has no record of this patient")
           }
  )
  
  if(is.na(filenames)) {
    return(list())
  } else {
    filenames_split <- filenames %>% 
      str_split('\n')
    
    mask <- filenames_split[[1]] %>% 
      isFile()
    
    availableFiles <- filenames_split[[1]][mask]
    
    if(identical(availableFiles, character(0))) {
      message("No photos currently available in the Zaldibase")
    } else {
      return(availableFiles)
    }
  }
}

clearCache <- function(aPatientUuid) {
  
  targetDir <- paste0(tiffDir, aPatientUuid)
  
  if(file.exists(targetDir)) {
    unlink(targetDir, recursive = TRUE)
  }
}

clearTmp <- function() {
  
  do.call(file.remove, list(list.files(tmpDir, full.names = TRUE)))
  
}

getImageLocation <- function(aLocalDB, aPatientUuid, aFilename) {

  if(aLocalDB) {
    targetDir <- paste0(imgsDir, aPatientUuid, "/", aFilename)
  } else {
    targetDir <- paste0(tmpDir, aFilename)
  }
  
  targetDir

}

generateReportEmail <- function(aPatient,
                                aDateDeNaissance, 
                                aStaffDecision,
                                anExplanation) {
  
  img_string <- add_image(file = logoPath, width = 90, align = 'center')
  
  thisBody <- paste0("Bonjour,

Vous recevez cette notification automatique parce que l'équipe du Neurochirurgical du Centre Hospitalier de Grenoble a récemment
pris une décision par rapport à un dossier que vous avez soumis.<br><br>

<b>Patient: </b><br>", 
aPatient, ", né(e) le ", translateDate(aDateDeNaissance), "<br><br>
<b>Décision: </b><br>",
aStaffDecision, "<br><br>
<b>Note supplémentaire</b><br>
<em>", anExplanation,"</em><br><br>

Merci d'avance!<br><br><br>

Cordialement, <br><br>
L'équipe du Neurochirurgical")
  
  
  compose_email(
    body = md(glue::glue(thisBody)),
    footer = md(glue::glue("Ceci est mon corps donné pour vous, faites ceci en souvenir de moi"))
  )
}