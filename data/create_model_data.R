require(dplyr)
require(lubridate)
require(tibble)

time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

patients <- tribble(~nom, ~prenom, ~date_naissance, ~condition, ~description, ~pre_decision, ~def_decision, ~hopital, ~contact, ~created_at, ~created_by, ~modified_at, ~modified_by,
                    'Vera', 'Daniel', '1985-12-29', 'Le duele la verga', '', '', '','Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait', time_now, 'Albert Malfait', time_now, 'Albert Malfait',
                    'Verdu', 'Anaïs', '1985-02-20', 'Le huele la cola', '', '', '','Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait', time_now, 'Albert Malfait', time_now, 'Albert Malfait',)

saveRDS(patients, file = 'data/patients_dummy.RDS')