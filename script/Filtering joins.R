# pacakges
library(tidyverse)

# let's start with the same persons_core and crashes_core 
crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

glimpse(crashes_core)
glimpse(persons_core)

# let's do a brief review of our mutating joins :) 

## what are the primary keys? what about the foreign key? 
## primary keys  identify rows in data sets
## foreign keys link data sets together, will be primary key of at least one
## other data set

## what are our mutating joins? what's the difference? 
## left_join, right_join, inner_join, full_join
## inner_join keeps row only where keys matching left and right database
## left-join keeps all rows in left dataset, matches matched keys. Unmatched
## will have NA for columns grom right_hand dataframe

## let's check out the homework briefly. 

# Which crashes involved at least one bicyclist? I want one row per crash. 
# we'll start by making a table of just the bicyclist person records. how many are there?


## crashes|>
## left_join (persons, join by (collision_id))
## grain: one row represents one crash-person record
## persons |>
## filter(person_type=="Bicyclist")
## grain: one row represents one bicyclist record

bicyclists <- persons_core %>%
  filter(PERSON_TYPE == "Bicyclist")

nrow(bicyclists)
n_distinct(bicyclists$COLLISION_ID)

crashes_bike <- crashes_core %>%
  left_join(persons_core, join_by(COLLISION_ID))

crashes_bike <- crashes_bike %>%
  filter(PERSON_TYPE == "Bicyclist")

glimpse(crashes_bike)

crashes_bike <- crashes_core %>%
  left_join(bicyclists, join_by(COLLISION_ID))
glimpse(crashes_bike)

crashes_bike %>%
  slice_head(n=10)



# does that number answer our question? why not? 
# no, multiple individuals or tandem bikes

# if it doesn't, which join should we reach for?


# use nrow() on the join and n_distinct() on that join's collision ID. Why are they different? 


# we can answer this by thinking about the grain.
# we're joining persons to the crashes grain, so what does one row represent? 

# is it every crash involving a bicyclist? let's check out the first 10 rows.  

# our mutating join adds columns so it has changed our grain, but we don't want it to right now. 

# so we'll need to use *filtering* joins
# we got exposed to one filtering join already: anti_join(). 
# which filtering join that will keep matches instead of non-matches?

bike_crashes <- crashes_core %>%
  semi_join(bicyclists, join_by(COLLISION_ID))
nrow(bike_crashes)
n_distinct(bike_crashes$COLLISION_ID)

glimpse(bike_crashes)




# this doesn't add more columns, so we're not working with crash-bicyclists combination

# now do anti_join for crashes that do not involve a bicyclist. 

no_bike_crashes <- crashes_core %>%
  anti_join(bicyclists, join_by(COLLISION_ID))
nrow(no_bike_crashes)
n_distinct(no_bike_crashes$COLLISION_ID)

nrow(bike_crashes) + nrow(no_bike_crashes) == nrow(crashes_core)


# what should the nrow() of each of your filtering joins dataframes be?

# now it's y'all's turn: identify crashes that involve at least one pedestrian, 
# one row per crash. 

pedestrians <- persons_core %>%
  filter(PERSON_TYPE == "Pedestrian")

nrow(pedestrians)

n_distinct(pedestrians)
ped_crashes <- crashes_core %>%
  semi_join(pedestrians, join_by(COLLISION_ID))
nrow(ped_crashes)
n_distinct(ped_crashes$COLLISION_ID)

no_ped_crashes <- crashes_core %>%
  anti_join(pedestrians, join_by(COLLISION_ID))
nrow(no_ped_crashes)
n_distinct(no_ped_crashes$COLLISION_ID)

nrow(ped_crashes) + nrow(no_ped_crashes) == nrow(crashes_core)

## after that, narrow it down. crashes where at least one pedestrian was recorded as female. 

pedestrians_female <- persons %>%
  filter(PERSON_TYPE == "Pedestrian" & PERSON_SEX == "F")

nrow(pedestrians_female)
n_distinct(pedestrians_female)
ped_f_crashes <- crashes_core %>%
  semi_join(pedestrians_female, join_by(COLLISION_ID))
nrow(ped_f_crashes)

n_distinct(ped_f_crashes$COLLISION_ID)

no_ped_f_crashes <- crashes_core %>%
  anti_join(pedestrians_female, join_by(COLLISION_ID))

nrow(no_ped_f_crashes)

n_distinct(no_ped_f_crashes$COLLISION_ID)

nrow(ped_f_crashes) + nrow(no_ped_f_crashes) == nrow(crashes_core)

# back together
## where did you put the PERSON_SEX condition? why?
## I put the PERSON_SEX condition in the first filtered step 