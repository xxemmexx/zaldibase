# Library in packages used in this application
require(shiny)
require(DT)
require(DBI)
require(RSQLite)
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
#source('R/supporting_functions.R')

# Hard-coded data

user_base <- tibble(
  user = c("zaldijn001", "zaldijn002", "zaldijn003"),
  password = c("pendejouw", "pendejouw", "pendejouw"),
  permissions = c("admin", "resident", "user"),
  name = c("Julien Zaldivar", "Julien Zaldivar", "Julien Zaldivar"))

decisions <- c(" ", "Chirurgie", "A rappatrier", "Conservateur", "Hospitaliser", "Autre avis")
pathologies <- c("Le duele la cola", "Le huele la cola", "Le duele la verga", "Suegra", "Autre...")

# Configuration
db_config <- config::get()$db

# Create database connection
conn <- dbConnect(
  RSQLite::SQLite(),
  dbname = db_config$dbname
)

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
