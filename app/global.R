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


decisions <- c(" ", "Opérés",
               "A opérer",
               "Examen complementaire",
               "Surveillance",
               "Traitement Conservateur",
               "Contrôle à la consultation des internes",
               "Contrôle à la consultation du chef",
               "Avis à autre discipline",
               "Autre",
               "Rien / Abstention")

pathologies <- c(" ", "HSDC",               
                 "Trauma Cranien",
                 "Trauma Spinal",
                 "Degeneratif Spinal",
                 "Tumeur Cranienne",
                 "Tumeur Spinale",
                 "Vasculaire",
                 "Complications - Infections",
                 "Infections primaires",
                 "Troubles Hydrauliques",
                 "Pediatrie",
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
