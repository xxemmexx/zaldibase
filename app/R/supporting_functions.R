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



