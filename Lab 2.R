# Lab 2: is this file ready to analyze?

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key
# ONe administrative entity or library system
# FSCSKEY It's a key from documentation, and it has no duplicates.

libraries <- read_csv(
  "PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one public library system in the United States, including the District
# of Columbia and territories.

# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)
# Each "outlet" represents a different kind of library (traditional library,
# bookmobile, and different branches of libraries). The downloaded file only
# has data for one of these outlets.

# which column should identify a row? 
# FSCSKEY should identify a row.

# what did those tell you?
# This tells me FSCSKEY does not have any duplicates or missing values.

# what would it have meant if the count() came back with rows?
# This would mean FSCSKEY duplicates for at least 2 rows.

# let's focus on VISITS for now. what does this column mean?
# This column states the number of visits to the library system in a year.

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?
# A negative number of visits would be impossible.

libraries |>
  summarise(
    min_visits = min(VISITS, na.rm = TRUE),
    max_visits = max(VISITS, na.rm = TRUE)
  )

libraries |>
  filter(VISITS < 0) |>
  count(VISITS)

# how many different negative values? how many rows of each?
# There are 2 different negative values. Negative 3 visits has 5 values
# and negative 1 visits has 69 rows.

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?
# A negative number is missing data according to the documentation.
# No, we cannot tell this from the data alone and need to look at the
# documentation.

# does R think any of these are missing?
# No, R thinks these negative values are valid data points.

is.na(libraries$VISITS) |>
  table()

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?
# Here, R is not recognizing the negative values as NAs. The documentation
# explains that the different negative values tell us the reason behind
# the missing data points.

# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?
# -1 means "missing value." -3 means "temporarily closed." -4 means "not
# applicable." -9 means "suppressed value." I found this in appendix A of the
# documentation.

# do the two codes mean the same thing?
# No, -1 has a more general meaning of "missing value," while -3 specifically
# indicates that that library system was closed during that time.

# every numeric column has a flag column. find the one for VISITS.
# F_VISITS is the flag column for visits.

# what does the flag tell you? does it tell the two codes apart?
libraries %>%
  select(F_VISITS) %>%
  slice_tail(n = 10)
# the Flag tells me that all of the flags for visits are because the data
# was reported but not imputed.

# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?
# Listing the codes makes sure that only the negative 1 and negative 3 values
# of visits get recoded as NA.

# both codes just turned into NA. what did we lose?
# We lost the specific reason that each value is NA.
# where can we still find it?
# We can still find it in the VISITS column.

# did it do what we meant? three questions:
# same number of library systems?
# Yes, we have the same number of library systems in the dataset.

# did every code become NA?
libraries %>%
  filter(visits_clean == -1 | visits_clean == -3)
# Yes, every code became NA.

# did anything else become NA?
sum(is.na(libraries$visits_clean))
# Yes, 46 other values became NA.

# does any of this matter?
# Yes, this matters because the 46 values would be lost from any calculations.

# why is the raw mean lower? what's in each denominator?
# The raw mean is lower because the -1 and -3 values are included as numbers in
# the raw calculation. There's a negative in each denominator.

# your turn :)
# get in your group project groups

# do that process for each of the following:
# TOTATTEN - program attendance
# TOTPRO - number of programs
# TOTCIR - total circulation



# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible?
# For TOTATTEN, this should measure the number of people who attended the 
# library in that specific time. Negative values would be impossible.
# For TOTPRO, this should measure the number of people who went to a given
# program at the library. Again, negative values would be impossible.
# TOTCIR should measure the circulation of books in and out of the library.
# Negative values would be reasonable in this column because more books could
# be going out of the library than into the library.

# range. any odd values? how many of each?
libraries |>
  summarise(
    min_attendance = min(TOTATTEN, na.rm = TRUE),
    max_attendance = max(TOTATTEN, na.rm = TRUE)
  )

libraries |>
  filter(TOTATTEN < 0) |>
  count(TOTATTEN)

# The attendance goes from -3 to 1386900. Yes, -3 and -1 are odd values. -3 has
# 5 values, and -1 has 401 values.

# what does the user's guide say they mean?
# ("we couldn't find it" is an answer. "we assumed" isn't.)
# -1 means this is a missing value, while -3 means the library was
# temporarily closed at that time.

# what's the flag column? (the names get shortened. look for it.)
# F_TOTATT is the flag column.

# clean version. keep the raw column.
libraries <- libraries |>
  mutate(attendance_clean = if_else(TOTATTEN %in% c(-1, -3), NA_real_, TOTATTEN))

# check it: same rows? every code became NA? nothing else did?
libraries %>%
  filter(attendance_clean == -1 | attendance_clean == -3)

sum(is.na(libraries$attendance_clean))

# Yes, we have the same number of rows. Yes, every column became NA.
# 406 values became NAs.

# mean before and after. big change or small?
libraries %>%
  summarise(
    mean_attendance_before = mean(TOTATTEN),
    mean_attendance_after = mean(na.omit(attendance_clean))
  )
# before was 11387, and after is 11910.
# This is a fairly big change.

# where are they? 
# Both mean values are in the 11000s. 

# recoding to NA fixes the number. does it finish the job?
# No, it does not finish the job because we learned information about why the
# values are missing when we recode them to NA.

# are the coded rows spread out, or do they bunch up?
# The coded rows bunch up around libraries that are temporarily closed.

# if someone compares circulation across states, what goes wrong?
# Different states have different numbers of NA values, so the mean circulation
# artificially varies because of the NA values in each state.

# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.

# No, this file is not yet to analyze as is. Recoding the negative values to
# NA was helpful, but hides some information given in the dataset. Also,
# there are still some NA values after recoding.
