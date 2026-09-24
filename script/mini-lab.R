# Lab 2

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

# 1: Make a table of person records for female pedestrians 
female_pedestrians <- persons %>%
  filter(PERSON_TYPE == "Pedestrian" & PERSON_SEX == "F")

glimpse(female_pedestrians)
## what does one row represent? 
# One row represents a female pedestrian who was involved in a car crash.

# 2: Keep only the crashes that involved at least one female pedestrian. 

crashes_with_f_ped <- crashes %>%
  semi_join(female_pedestrians, join_by(COLLISION_ID))


# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 

crashes_f_ped_core <- crashes_with_f_ped %>%
  select(c(COLLISION_ID, BOROUGH, `VEHICLE TYPE CODE 1`, `VEHICLE TYPE CODE 2`,
           `VEHICLE TYPE CODE 3`, `VEHICLE TYPE CODE 4`, `VEHICLE TYPE CODE 5`))


# how can we select every variable that starts with "VEHICLE TYPE CODE"?
# We can use the contains command and put contains("VEHICLE TYPE CODE").

# does one row still represent one crash? check it. 
glimpse(crashes_f_ped_core)
# Yes, one row still represents one crash.

# why a filtering join instead of a mutating join? 
# We didn't want to add columns to the dataset, which is what a mutating join
# does. Instead, we wanted to investigate our dataset to understand 
# matches, and a filtering join can help with that.

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 

# We should have 24,700 rows.

crashes_f_ped_pivoted <- crashes_f_ped_core %>%
  pivot_longer(
    names_to = "Vehicle",
    values_to = "Values",
    cols = c("VEHICLE TYPE CODE 1", "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3",
             "VEHICLE TYPE CODE 4", "VEHICLE TYPE CODE 5")
  )

glimpse(crashes_f_ped_pivoted)

# how many missing values are in the new dataframe? 
sum(is.na(crashes_f_ped_pivoted))
# 24918 missing values are in the new dataframe.

# why do we think that slots 3, 4, and 5 have so many more missing values? 
# these slots are from the 3rd, 4th, and 5th vehicles involved in the crash.
# A lot of the crashes have at most 2 involved vehicles, so the "extra"
# vehicle codes are NA values.

# does every crash have a first vehicle recorded? 
# No, not every crash has a first vehicle recorded.

# are we safe to drop missing values?
# No, we are not safe to drop missing values because some crashes might not
# involve the types of vehicles listed (ex. a powerchair crash)

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 

unique(crashes_f_ped_pivoted$Values)
# [1] "Sedan"                                                                  
# [3] "Station Wagon/Sport Utility Vehicle" "E-Scooter"                          
# [5] "Ambulance"                           "Bike"                               
# [7] "Bus"                                 "PK"                                 
# [9] "Taxi"                                "E-Bike"                             
# [11] "Van"                                 "Motorscooter"                       
# [13] "Convertible"                         "Pick-up Truck"                      
# [15] "Box Truck"                           "Tractor Truck Diesel"               
# [17] "Moped"                               "Flat Rack"                          
# [19] "MOPED"                               "Tow Truck / Wrecker"                
# [21] "Motorbike"                           "Motorcycle"                         
# [23] "FORKLIFT"                            "Electric M"                         
# [25] "Dump"                                "3-Door"                             
# [27] "Suburban"                            "moped scoo"                         
# {29] "Pedicab"                             "charter bu"                         
# [31] "School Bus"                          "Bobcat"                             
# [33] "Ford EC2"                            "Gas dirt b"                         
# [35] "Honda HRV"                           "GOLF CART"                          
# [37] "Cargo van"                           "DELIVERY"                           
# [39] "Carry All"                           "Tractor Truck Gasoline"             
# [41] "Garbage or Refuse"                   "Fork lift"                          
# [43] "Open Body"                           "Access-A-R"                         
# [45] "LIMO"                                "golf cart"                          
# [47] "Chassis Cab"                         "0000"                               
# [49] "Forklift"                            "tank"                               
# [51] "PICKUP TRU"                          "Flat Bed"                           
# [53] "Commercial"                          "Standing s"                         
# [55] "Skid steer"                          "AMSGC"                              
# [57] "CEMENT TRU"  
crashes_f_ped_pivoted |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 40)
