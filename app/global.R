# Library in packages used in this application
require(shiny)
require(DT)
require(DBI)
require(RSQLite)
require(RPostgres)
require(shinythemes)
require(shinyjs)
require(shinycssloaders)
require(lubridate)
require(shinyFeedback)
require(stringr)
require(dplyr)
require(dbplyr)
require(bslib)
require(sodium)
require(RCurl)
require(magick)
require(blastula)
require(quarto)

localDB = TRUE

tmpDir <- 'data/tmp/'
tiffDir <- 'data/tiff/'
imgsDir <- 'data/imgs/'
logoPath <- 'www/chu_logo.jpeg'
localDBPath <- 'data/zaldibase.sqlite3'
configPath <- 'data/config.rds'
keyPath <- 'data/key.rds'
usersPath <- 'data/user_base_encryp.rds'
credentialsPath <- 'data/outlook_credentials'

user_base <- readRDS(usersPath)
db_config <- readRDS(configPath)
my_key <- readRDS(keyPath)

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

decisions <- c(" ", "A opérer",
               "Abstention",
               "Avis à autre discipline",
               "Rendez-vous à la consultation du chef",
               "Rendez-vous à la consultation des internes",
               "Examen complémentaire",
               "Opérés",
               "Surveillance",
               "Traitement conservateur",
               "Autre"
               )

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

printAbout <- '<h2 style="font-family: Garamond">Zal<b>di|b</b>ase</h2> <br>
                                 <p>Un projet pour et par pendejos qui veulent une
                                 administration simple pour les chefs de garde du CHU.<br><br>
                                 Avez-vous des questions? Gardez-les pour vous.</p>'

printLogo <- '<p style="font-family: Garamond">Zal<b>di|b</b>ase</p>'


jsHeader <- JS("function(settings, json) {",
"$(this.api().table().header()).css({'background-color': '#3E3F3A', 'color': '#FFF0F5'});",
"}")

