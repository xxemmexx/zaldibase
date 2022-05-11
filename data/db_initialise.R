library(RSQLite)
library(tibble)
require(uuid)
require(dplyr)

# Create a connection object with SQLite
conn <- dbConnect(
  RSQLite::SQLite(),
  "data/patients.sqlite3"
)

# Create a query to prepare the 'mtcars' table with additional 'uid', 'id',
# & the 4 created/modified columns
create_patients_query = "CREATE TABLE patients (
  uid                             TEXT PRIMARY KEY,
  nom                             TEXT,
  prenom                          TEXT,
  date_naissance                  TEXT,
  condition                       TEXT,
  description                     TEXT,
  pre_decision                    TEXT,
  def_decision                    TEXT,
  hopital                         TEXT,
  contact                         TEXT,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT
)"

# dbExecute() executes a SQL statement with a connection object
# Drop the table if it already exists
dbExecute(conn, "DROP TABLE IF EXISTS patients")
# Execute the query created above
dbExecute(conn, create_patients_query)

# Read in the RDS file created in 'data_prep.R'
dat <- readRDS("data/patients_dummy.RDS")

# add uid column to the `dat` data frame
dat$uid <- uuid::UUIDgenerate(n = nrow(dat))

# reorder the columns
dat <- dat %>%
  select(uid, everything())

# Fill in the SQLite table with the values from the RDS file
DBI::dbWriteTable(
  conn,
  name = "patients",
  value = dat,
  overwrite = FALSE,
  append = TRUE
)

# List tables to confirm 'mtcars' table exists
dbListTables(conn)

# disconnect from SQLite before continuing
dbDisconnect(conn)
