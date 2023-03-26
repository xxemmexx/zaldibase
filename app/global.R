# Library in packages used in this application
require(shiny)
require(shinyjs)
require(DT)
require(DBI)
require(RSQLite)
require(RPostgres)
require(shinythemes)
require(shinycssloaders)
require(lubridate)
require(shinyFeedback)
require(stringr)
require(dbplyr)
require(bslib)
require(sodium)
require(RCurl)
require(magick)
require(blastula)
require(quarto)
require(purrr)
require(purrrlyr)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)

localDB = TRUE

tmpDir <- 'data/tmp/'
tiffDir <- 'data/tiff/'
imgsDir <- 'data/imgs/'
tmpImg <- 'www/tmpimg'
logoPath <- 'www/chu_logo.jpeg'
childPath <- 'www/child_icon.jpeg'
bugsPath <- 'www/bugs_icon.png'
localDBPath <- 'data/zaldibase.sqlite3'
configPath <- 'data/config.rds'
keyPath <- 'data/key.rds'
usersPath <- 'data/user_base_encryp.rds'
credentialsPath <- 'data/outlook_credentials'
sd <- stamp_date("26/09/2018")
sdISO <- stamp_date("2018-09-26")

user_base <- readRDS(usersPath)
db_config <- readRDS(configPath)
my_key <- readRDS(keyPath)

getDisplayName <- user_base$name
names(getDisplayName) <- user_base$user
getUsername <- user_base$user
names(getUsername) <- user_base$name

config_df <- db_config %>%
  data_decrypt(my_key) %>%
  unserialize() 

secretariat <- config_df[['mailSecretariat']] %>% as.character()
zaldibase <- config_df[['mailZaldibase']] %>% as.character()

if(localDB) {
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = localDBPath)
  
} else {
  
  dbInfo <- config_df[['db']] %>%
    str_split(":")
  
  deviceInfo <- config_df[['device']] %>%
    str_split(":")
  
  conn <- DBI::dbConnect(RPostgres::Postgres(), 
                         dbname = dbInfo[[1]][[1]], 
                         host = dbInfo[[1]][[2]], 
                         port = dbInfo[[1]][[3]], 
                         user = dbInfo[[1]][[4]], 
                         password = dbInfo[[1]][[5]])
}

decisions <- c(" ", "A hospitaliser / A rapatrier",
               "Rendez-vous / Suivi",
               "Prise en charge externe ou non-chirurgicale",
               "Complément d`examen à faire")

decisions_dev <- c(" ", 
                   "A hospitaliser / A rapatrier",
                   "Rendez-vous / Suivi", 
                   "Complément d`examen à faire",
                   "Clôturer dossier")

graphiques <- c("Nombre de cas par origine",
                "Disque par pathologie",
                "Disque par âge")

pathologies <- c(" ", "Trauma crânien",
                 "Trauma rachidien",
                 "Pathologie vasculaire (Intracranienne et rachidienne)",
                 "Dégénératif spinal",
                 "Tumeur encephalique et enveloppes",
                 "Tumeur rachidienne et médullaire",
                 "Troubles LCR / Hydrocephalie",
                 "Infections primaires",
                 "Congénitales / HIV",
                 "Complications / Infections post-op",
                 "Autre...")

medications <- c(" ", "Aspirine",
                 "Plavix",
                 "Eliquis",
                 "Xarelto",
                 "Coumadine",
                 "HBPM",
                 "Autres",
                 "Thrombocytopenie",
                 "Troubles Coagulations")

hopitaux <- c(" ", "CHU - Urgences et Services", 
              "CHU - Dechoc",
              "Voiron",
              "Albertville - Moûtiers",
              "Chambéry",
              "Saint-Jean-de-Maurienne",
              "Bourg-Saint-Maurice",
              "Romans-sur-Isère",
              "La Mure",
              "Gap",
              "Briançon",
              "Valence",
              "Savoie (Annecy-Thonons-CHAL-Sallanches-St. Julien)",
              "Aix-les-Bains", 
              "Bougoin-Jallieu",
              "Cliniques (Mutualiste - Autres)",
              "Médecin généraliste",
              "SSR",
              "Autre...")

# Stop database connection when application stops
shiny::onStop(function() {
  dbDisconnect(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 1)

# HTML

printStandardEmailTitle <- "Nouvelle notification CHUGA - équipue du neurochirurgical"

printAbout <- '<p><h5><b>Exploitation</b><br><br>
Ce formulaire a été créé dans le but d’améliorer la traçabilité des avis de Neurochirurgie de l’Hôpital de Grenoble ainsi que pour optimiser la communication de ces avis aux hôpitaux et cliniques partenaires.
La base de données le soutenant permet, également, de mener des statistiques internes sur ces avis afin d’ajuster le fonctionnement du service en fonction des nécessités de la population du bassin Grenoblois.<br><br>

En déplaçant la responsabilité de la documentation des avis aux médecins demandeurs, le formulaire garantit l’intégrité des informations transmise et minimise les risques de perte d’information liés à une communication téléphonique à distance. En contrepartie, les avis rendus sont transmis par écrit afin de permette une référence fiable de l’avis donné par la garde de Neurochirurgie.<br><br> 

Les centres partenaires disposent d’un accès sécurisé pour visualiser les avis précédents avec une possibilité de rétrospection de 1 mois.<br><br>

Les données contenues dans la base de données sont soumises au secret médical et leur visualisation nécessite l’autorisation préalable du Chef de Services de Neurochirurgie et des accès professionnels CHUGA.<br><br>
<br>
<b>Recherche</b><br><br>
L’utilisation des données à des fins de statistique, de recherche et de publications est possible si les démarches éthiques nécessaires pour leur exploitation auprès des instances hospitalières responsables sont effectuées.
Un accès spécial recherche est nécessaire et délivré après approbation pour accéder aux données dans un but de recherche.
Merci de contacter Accueil-Recherche@chu-grenoble.fr<br><br>

Toute publication contenant des données tirées de cette base de données devra contenir des contributions (authorship) soulignant le travail de conception et de mise en place de la base de données.
Le concepteur de la base de données devra figurer en dernier auteur si >50% du contenu de la publication est tirée de la base de données et en avant-dernier auteur si <50% des données proviennent de la base de données. Le reste des auteurs et leur ordre sera décidé selon les protocoles éthiques de publication habituelles.
Pour vérifier la compliance votre article avec ces conditions d’utilisation, merci de contacter julienzaldivar@gmail.com.</h5></p>'

printLogo <- '<p style="font-family: Garamond">Zal<b>di|b</b>ase</p>'

printTakeGarde <- '<p style="color:#3E3F3A;font-size:60px;">&#9817;</p> <br>'

printWarningIncompleteStaffMeeting <- '<p style="color:#FF4500;font-size:60px">&#9888;</p> <br> <h4 style = "line-height: 1.75;">Il est indispensable que tous les dossiers <br> aient une décision de staff et une explication <br> pour pouvoir clôturer le staff meeting</h4>'

printWarningHighPrivileges <- '<p style="color:#FF4500;font-size:60px">&#9888;</p> <br> <h4 style = "line-height: 1.75;">Vous ne pouvez prendre la garde que si vous <br> avez des privilèges de <em>resident</em> ou de <em>chef de garde</em></h4>'

printPatientez <- '<p style="color:#3E3F3A;font-size:60px;">&#9993;</p> <br> <h3 style = "line-height: 1.75;">Patientez...</h3>'

printReopenDossierExplanation <- '<p style="color:#3E3F3A;font-size:60px;">&#9997;</p> <br> <h3 style = "line-height: 1.75;">En réouvrant ce dossier, vous allez créer un nouveau <br> dossier qui reprendra les données de ce patient. <br> Êtes-vous sûr de vouloir réouvrir le dossier?</h3>'

patientezDialog <- modalDialog(
  div(style = "padding: 30px;", class = "text-center",
      HTML(printPatientez)),
    easyClose = TRUE,
    footer = modalButton("Fermer"))

jsHeader <- JS("function(settings, json) {",
"$(this.api().table().header()).css({'background-color': '#3E3F3A', 'color': '#FFF0F5'});",
"}")



