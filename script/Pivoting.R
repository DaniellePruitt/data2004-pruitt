rm(list = ls())
library(tidyverse)
library(readxl)

fishing <- read_excel("data/raw/commercial.xlsx", sheet = "Erie")

glimpse(fishing)
# What does one row represent?
## One row represents something about region-year weight (rounded lbs) of fish
## caught
# How would we look at our grain over time by region?

fishing %>%
  ggplot(aes(x = "what would go here?", y = "what would go here?"))
fishing %>%
  ggplot(aes(x = Year, y = `Grand Total`)) +
  geom_line()

# What goes on the y-axis?


# What goes on the color aesthetic if we want one line per region?

# Are these data tidy? If not, which one breaks the rules of tidy data?

# Let's predict something

nrow(fishing)

# We're about to move 7 columns into one. How many rows should we have?

# Pivot

fishing_long <- fishing %>%
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = c("Michigan (MI)", "New York (NY)", "Ohio (OH)",
             "Pennsylvania (PA)", "U.S. Total",  "Canada (ONT)",
             "Grand Total")
  ) 

nrow(fishing_long)
nrow(fishing_long) / nrow(fishing)

#Did we get what we predicted?

# Which one of these four pivots would you use? Why not some of the others?


fishing_long %>%
  distinct(region)

fishing_long %>% 
  filter(Year == 1885, Species == "Lake Whitefish") %>%
  select(region, values)
133 + 30 + 1249 + 2120 + 186
3718 - 186

fishing_long %>%
  filter(!region %in% c("U.S. Total", "Grand Total")) %>%
  summarise(total = sum(values, na.rm = TRUE))

# Why isn't the first total just triple the third total?

glimpse(fishing_long)

fishing_long %>% 
  filter(!region %in% c("U.S. Total", "Grand Total")) %>%
  mutate(species = fct_lump_n(Species, 6)) %>%
  ggplot(aes(x = Year, y = values, color = species)) +
  scale_color_manual(values = palette.colors(7, "Okabe-Ito")) +
  geom_line()

fishing_long %>%
  select(Year, Species, region, values) %>%
  pivot_wider(names_from = region, values_from = values) %>%
  print(width = Inf)

fishing_michigan <- read_excel("data/raw/commercial.xlsx", sheet = "Michigan")            

glimpse(fishing_michigan)
# One row represents the round weight (in lbs) of fish caught in a given region
# in a given year.

fishing_michigan_long <- fishing_michigan %>%
  pivot_longer(names_to = "region",
               values_to = "values",
               cols = c("Green Bay MI_CORA", "Green Bay MI_MiDNR", 
                        "Green Bay (MI)", "Mich. Proper_CORA", 
                        "Mich. Proper_MiDNR", "Mich. Proper (MI)",
                        "MI State Total", "Green Bay (WI)", 
                        "Mich. Proper (WI)", "WI State Total", 
                        "Illinois (IL)", "Indiana (IN)", "U.S. Total")
  )

fishing_michigan_long %>%
  filter(!region %in% c("U.S. Total", "MI State Total", "WI State Total")) %>%
  summarise(total = sum(values, na.rm = TRUE))

# The total catch for the lake is 2,262,843 lbs.

fishing_michigan_long %>%
  distinct(region)

# I am left with Green Bay MI, Mich. Proper, Green Bay WI, Mich. Proper WI, 
# Illinois and Indiana.

nrow(fishing_michigan)
nrow(fishing_michigan_long)
# I started with 2285 rows and the pivoted version has 29,705 rows.