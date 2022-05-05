library(dplyr)
require(tibble)

patients <- tribble(~nom, ~prenom, ~date_naissance, ~condition, ~hopital, ~contact,  
                    'Vera', 'Daniel', '1985-12-29', 'Le duele la verga', 'Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait',
                    'Verdu', 'Anaïs', '1985-02-20', 'Le huele la cola', 'Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait')

saveRDS(patients, file = 'data/patients_dummy.RDS')