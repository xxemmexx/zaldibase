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
#require(twilio)

user_base <- readRDS("data/user_base_encryp.rds")
db_config <- readRDS("data/config_encryp.rds")
my_key <- readRDS("data/key.rds")

config_df <- db_config %>%
  data_decrypt(my_key) %>%
  unserialize() 
  
dbInfo <- config_df[['secret']] %>%
  str_split(":")


conn <- DBI::dbConnect(RPostgres::Postgres(), 
                       dbname = dbInfo[[1]][[1]], 
                       host = dbInfo[[1]][[2]], 
                       port = dbInfo[[1]][[3]], 
                       user = dbInfo[[1]][[4]], 
                       password = dbInfo[[1]][[5]])


decisions <- c(" ", "A opérer",
               "Abstention",
               "Avis à autre discipline",
               "Contrôle à la consultation du chef",
               "Contrôle à la consultation des internes",
               "Examen complémentaire",
               "Opérés",
               "Surveillance",
               "Traitement conservateur",
               "Autre"
               )

pathologies <- c(" ", "Complications - Infections",
                 "Dégénératif spinal",
                 "HSDC", 
                 "Infections primaires",
                 "Pédiatrie",
                 "Trauma crânien",
                 "Trauma spinal",
                 "Troubles hydrauliques",
                 "Tumeur crânienne",
                 "Tumeur spinale",
                 "Vasculaire",
                 "Autre...")

hopitaux <- c(" ", "Aix-les-Bains", 
              "Albertville",
              "Annecy",
              "Bourg-Saint-Maurice",
              "Briançon",
              "CHU - Autres",
              "CHU - Urgences",
              "Chambéry",
              "Clinique Belledonne",
              "Clinique des Cèdres",
              "Clinique Mutualiste",
              "Médecin généraliste",
              "Romans-sur-Isère",
              "Saint-Jean-de-Maurienne",
              "SSR",
              "Voiron",
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
"$(this.api().table().header()).css({'background-color': '#CD5C5C', 'color': '#FFF0F5'});",
"}")

