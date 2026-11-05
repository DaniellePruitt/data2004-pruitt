library(tidyverse)
library(httr2)
library(rvest)

two_thousand_ten_url <- "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_2010"

request(two_thousand_ten_url) |> 
  req_user_agent("Danielle Pruitt, DATA 2004 class, Translyvania University") |> 
  req_perform(path = "data/raw/2010_songs.html")

# Requested 11/03/2026 at 8:50pm.

two_thousand_ten_page <- read_html("data/raw/2010_songs.html")

two_thousand_ten_tables <- two_thousand_ten_page |> 
  html_elements("table.wikitable") |> 
  html_table()

length(two_thousand_ten_tables)

two_thousand_ten_tables[[1]]
two_thousand_ten_raw <- two_thousand_ten_tables[[1]]
View(two_thousand_ten_raw)

two_thousand_fifteen_url <- "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_2015"

request(two_thousand_fifteen_url) |> 
  req_user_agent("Danielle Pruitt, DATA 2004 class, Translyvania University") |> 
  req_perform(path = "data/raw/2015_songs.html")

# Requested 11/04/2026 at 5:50pm.

two_thousand_fifteen_page <- read_html("data/raw/2015_songs.html")

two_thousand_fifteen_tables <- two_thousand_fifteen_page |> 
  html_elements("table.wikitable") |> 
  html_table()

length(two_thousand_fifteen_tables)

two_thousand_fifteen_tables[[1]]
two_thousand_fifteen_raw <- two_thousand_fifteen_tables[[1]]
View(two_thousand_fifteen_raw)
