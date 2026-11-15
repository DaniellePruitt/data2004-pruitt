library(tidyverse)
library(httr2)
library(rvest)

songs_with_genres_2010s <- read.csv("data/raw/songs_normalize.csv")

two_thousand_ten_songs <- read_html("data/raw/2010_songs.html")
two_thousand_ten_tables <- two_thousand_ten_songs |> 
  html_elements("table.wikitable") |> 
  html_table()

length(two_thousand_ten_tables)

two_thousand_ten_tables[[1]]
two_thousand_ten_raw <- two_thousand_ten_tables[[1]]
View(two_thousand_ten_raw)

genres_2010 <- songs_with_genres_2010s |>
  filter(year == 2010) |>
  select(song, genre)

two_thousand_ten_cleaned <- two_thousand_ten_raw |>
  mutate(songs_normalized = str_remove_all(Title, '"'))

genres_clean_2010 <- genres_2010 |>
  mutate(songs_normalized = song)

two_thousand_ten_songs_with_genres <- two_thousand_ten_cleaned |>
  left_join(genres_clean_2010, join_by(songs_normalized)) |>
  filter(!is.na(genre)) |>
  distinct(songs_normalized, .keep_all = TRUE) |>
  mutate(genre = str_extract(genre, "^[^,]+"))

two_thousand_fifteen_songs <- read_html("data/raw/2015_songs.html")
two_thousand_fifteen_tables <- two_thousand_fifteen_songs |> 
  html_elements("table.wikitable") |> 
  html_table()

length(two_thousand_fifteen_tables)

two_thousand_fifteen_tables[[1]]
two_thousand_fifteen_raw <- two_thousand_fifteen_tables[[1]]
View(two_thousand_fifteen_raw)

genres_2015 <- songs_with_genres_2010s |>
  filter(year == 2015) |>
  select(song, genre)

two_thousand_fifteen_cleaned <- two_thousand_fifteen_raw |>
  mutate(songs_normalized = str_remove_all(Title, '"'))

genres_clean_2015 <- genres_2015 |>
  mutate(songs_normalized = song)

two_thousand_fifteen_songs_with_genres <- two_thousand_fifteen_cleaned |>
  left_join(genres_clean_2015, join_by(songs_normalized)) |>
  filter(!is.na(genre)) |>
  distinct(songs_normalized, .keep_all = TRUE) |>
  mutate(genre = str_extract(genre, "^[^,]+"))
  
 
