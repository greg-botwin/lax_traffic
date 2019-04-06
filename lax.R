library(rtweet)
library(tidytext)
library(tidyverse)



tl <- get_timeline(user = "flyLAXairport", n = 1000)

tl <- tl %>%
  filter(str_detect(text, "LAX TRAFFIC UPDATE")) %>%
  select(text, created_at) %>%
  mutate(time_str  = str_extract(text, "(?<=\\().+?(?=\\))")) %>%
  mutate(time_digit = str_extract(time_str, "\\d+(?=:)")) %>%
  mutate(am_pm = str_extract(time_str, "AM|PM"))

strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187 ", "482 952 3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_extract(strings, phone)
str_match(strings, phone)
