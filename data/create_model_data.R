library(dplyr)
#library(tidyr)
require(tibble)

# attach data frame from 'mtcars' dataset to global environment
mtcars <- datasets::mtcars


patients <- tribble(~Nom, ~Prénom, ~`Date de naissance`, ~Condition, ~`Hôpital d'origine`, ~`Personne de contact`,  
                    'Vera', 'Daniel', '1985-12-29', 'Le duele la verga', 'Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait',
                    'Verdu', 'Anaïs', '1985-02-20', 'Le huele la cola', 'Centre Hospitalier Albertville-Moûtiers', 'Albert Malfait')


# convert the rownames to an actual column.  We suggest to never use row names, just
# use a regular column.
mtcars <- rownames_to_column(mtcars, var = 'model')

# Converting Weight (i.e. 'wt') from 1000's of lbs to lbs
mtcars$wt <- mtcars$wt * 1000


# Converting binary values to intended, character values
mtcars <- mtcars %>%
  mutate(
    vs = ifelse(vs == 0, 'V-shaped', 'Straight'),
    am = ifelse(am == 0, 'Automatic', 'Manual')
  )


saveRDS(mtcars, file = '01_traditional/data_prep/prepped/mtcars.RDS')
