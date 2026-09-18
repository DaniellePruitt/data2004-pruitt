rm(list = ls())

library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

glimpse(crashes)
glimpse(persons)

# What does one row represent in each table?
# crashes - generic vehicle crashes in NYC - 91,316 rows, 28 colunmns
# persons- the person involved in the crash -  317,941 rows, 21 columns

# Grain 
# Crashes - one row represents one specific crash that occurs in MYC
# persons - one row represents a recorded person involved with the crashes

crashes_core <- crashes %>%
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons %>%
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

# You work for the NYC Department of Transportaion and you have been given a 
# request:
# How do person-level injury outcomes compare across boroughs?

# Can you answer this with one dataframe alone? Probably not, or else we 
# wouldn't do this on our join day

crashes_core %>%
  count(COLLISION_ID) %>%
  filter(n>1)

persons_core %>%
  count(UNIQUE_ID) %>%
  filter(n>1)

 
persons_core %>%
  count(COLLISION_ID) %>%
  filter(n>1) %>%
  arrange(desc(n))

crashes_core %>%
  filter(is.na(COLLISION_ID))


persons_core %>%
  filter(is.na(COLLISION_ID))

persons_core %>%
  filter(is.na(UNIQUE_ID))

# Cardinality is how many rows on each side can share a key value.
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? What does that tell you about what a join will do to
# our rows?

# for collision id,linked to several unique id's so one-to-many
# 
nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference tell 
# you before we join anything?

# Let's do our join. What should we join by? What's your guess for the number 
# of rows?

# We'll start by dosing a join that keeps observations where both cases exist.
# Can anyone remember which join this is?

# Use the console if you don't remember.

# inner - joins matches across both. drops the ones that don't match
# left - joins matches, but does NOT drop the lack of matches.

crashes_inner <- crashes_core %>%
  inner_join(persons_core, join_by(COLLISION_ID))

glimpse(crashes_inner)

# What does one row represent now? Is that the same as before?
# you keep the grain and the population of your left-hand join
# one row represents one crash record with person-level crash information

persons_inner <- persons_core %>%
  inner_join(crashes_core, join_by(COLLISION_ID))

glimpse(persons_inner)
# one row represents one person record with crash-level information.

# Which one keeps every row of the LEFT table whether it matched or not.

crashes_left <- crashes_core %>%
  left_join(persons_core, join_by(COLLISION_ID))

glimpse(crashes_left)

nrow(crashes_inner) # 317,941
nrow(crashes_left) # 318,319

# We know that there are some records that do not have person records involved.

# When would you want inner_Join? When would you want left_join?
# It depends on the comparison.
## Which depends on the question that you are asking.

crashes_core %>%
  anti_join(persons_core, join_by(COLLISION_ID)) %>%
  nrow()

# anti-join keeps left rows with no.....
# The four mutation Joins: inner_join, left_join, right-join, full_join

# Build a person-level table that includes borough.

# before you write anything:
## What should one row represent when you're done? ONe row should represent
## a person with borough level information.

## Which table goes on the left?  Persons goes on the left.

# Then:
## Join them

persons_borough <- persons %>%
  inner_join(crashes, join_by(COLLISION_ID))

## Declare the cardinality with relationship = and see if R agrees.
persons_borough <- persons %>%
  inner_join(crashes, join_by(COLLISION_ID),
             relationship = "many-to-many")
# The cardinality between persons_borough and persons is many-to-many.

## Use anti_join() to look at whatever didn't match.

persons_borough %>%
  anti_join(persons, join_by(COLLISION_ID)) %>%
  nrow()

# All of the data points matched between persons_borough and persons.

## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.

persons_borough %>%
  count(BOROUGH)
#1 BRONX          34190
#2 BROOKLYN       74536
#3 MANHATTAN      38463
#4 QUEENS         58409
#5 STATEN ISLAND   9114

persons_borough %>%
  count(PERSON_TYPE)
# 1 Bicyclist         6228
# 2 Occupant        299174
# 3 Other Motorized   2160
# 4 Pedestrian       10379


persons_borough %>%
  count(PERSON_INJURY)
# 1 Injured        54030
# 2 Killed           268
# 3 Unspecified   263643
