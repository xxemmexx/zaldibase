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
#require(twilio)
source('R/supporting_functions.R')

user_base <- tibble(
  user = c("zaldijn001", "zaldijn002", "zaldijn003"),
  password = c("pendejouw", "pendejouw", "pendejouw"),
  permissions = c("admin", "resident", "user"),
  name = c("Julien Zaldivar", "Julien Zaldivar", "Julien Zaldivar"))

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
options(spinner.type = 8)
