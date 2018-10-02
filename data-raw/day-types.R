library(tidyverse)

day_types <- readr::read_csv("data-raw/dayTypes1219.csv") %>%
    mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01")))

devtools::use_data(day_types, internal = TRUE)
