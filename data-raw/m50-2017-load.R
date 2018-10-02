m50_2017 <- readr::read_csv("data-raw/m50-2017.csv")

devtools::use_data(m50_2017, internal = TRUE)
