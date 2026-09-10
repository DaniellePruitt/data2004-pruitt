# packages
library(tidyverse)

# import and look
data <- read_csv(
  "data/raw/co-est2025-alldata.csv",
  locale = locale(encoding = "Latin1"),
  col_types = cols(.default = col_character())
)


glimpse(data)
names(data)

# What can we determine from this? What can we not?
# We can determine it's about State and County level, proabaly about population.
# We can not determine a whole heck of a lot else

# What have you been given?
# Population changes, estimates, annual resident population

# Census Bureas match documentation in and the other data tthat you have.

# Find the documentation and record
# Who produced this?  The Census Bureau
# What does the file contain? Population for states and counties from 2020-2025
# What time period does it cover? 2020 - 2025

# Build a diagnostic view
## Find the documentation and look at the code above. What variables determine
## what one row represents?   

data_trimmed <- data %>%
  select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME, CTYNAME,
         POPESTIMATE2024, POPESTIMATE2025, NPOPCHG2025)

# Looking at your table:
## Does every row appear to represent the same kind of geographic observation?
### No. STates-county
## Which row(s) look different?
### 040 = state; 050 = county
## What in your table tells you this?
### SUMLEV column
## Is there a variable that appears to encode that differenece?
### SUMLEV column

data_trimmed %>%
  slice_head(n=10)

# Declare the grain

## One row represents a geographic location
## One row represents either a county or a state
## One row represents a mixed state-county grain.

data_trimmed_numeric <- data_trimmed %>%
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    pop2024 = as.numeric(POPESTIMATE2024),
    popchg2025 = as.numeric(NPOPCHG2025)
  )
 
data_trimmed_numeric %>%
  filter(STNAME == "Kentucky") %>%
  slice_head(n = 10)

data_trimmed_numeric %>% 
  filter(SUMLEV == "040") %>%
  summarise(
    total_pop_2025 = sum(pop2025)
  )

data_trimmed_numeric %>%
  filter(SUMLEV == "040") %>%
  select(STNAME)%>%
  print(n = 51)

# 341,784,857

# Which Kentucky counties grew the most from 2024 to 2025?
data_trimmed_numeric %>%
  filter(STNAME =="Kentucky", SUMLEV == "050") %>%
  filter(popchg2025 > 0) %>%
  select(CTYNAME, popchg2025) %>%
  arrange(desc(popchg2025)) %>%
  print(n=81)


data_trimmed_numeric %>%
  filter(SUMLEV == "050", STNAME == "Kentucky", popchg2025 > 0) %>%
  mutate(
    popchvalid = pop2025 - pop2024
  ) %>%
  select(CTYNAME, popchvalid) %>%
  arrange(desc(popchvalid)) %>%
  print(n=120)
  )
## Warren County and Fayette County

# How many counties or county-equivalent records are in this file?
data_trimmed_numeric %>%
  filter(SUMLEV == "050") %>%
  count()

### 3144