require(dplyr)
require(lubridate)
require(tibble)

time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

patients <- tribble(~nom, ~prenom, ~date_naissance, ~phone_number_patient, ~pathologie, ~description, ~pre_decision, ~def_decision, ~created_at, ~created_by, ~modified_at, ~modified_by,
                    'Vera', 'Daniel', '1985-12-29', '+33725446765','Le duele la verga', '', '', '', time_now, 'Cassandra Gotsi', time_now, 'Cassandra Gotsi',
                    'Verdu', 'Anaïs', '1985-02-20', '+33612155572','Le huele la cola', '', '', '', time_now, 'Cassandra Gotsi', time_now, 'Cassandra Gotsi')

contacts <- tribble(~nom, ~prenom, ~phone_number_contact, ~email, ~affiliation,
                    'Gotsi', 'Cassandra', '+33760446761', 'c.gotsi@cham.fr', 'Centre Hospitalier Albertville-Moûtiers',
                    'Malfait', 'Albert', '+33660446751', 'a.malfait@cham.fr', 'Centre Hospitalier Albertville-Moûtiers',
                    'della Berche', 'Belami', '+33690935451', 'b.dberche@cbelle.fr', 'Clinique Belledone',
                    'Sanchez', 'Hugo', '+33643312109', 'h.sanchez@chai.fr', 'Centre Hospitalier Alpes-Isère',
                    'Bodoque', 'Benito', '+33760431761', 'b.bodoque@cedres.fr', 'Clinique des Cèdres',
                    'Palme', 'Olof', '+33760226760', 'o.palme@ghmg.fr', 'Groupe hospitalier mutualiste de Grenoble')

saveRDS(patients, file = 'data/patients_dummy.RDS')
saveRDS(contacts, file = 'data/contacts_dummy.RDS')