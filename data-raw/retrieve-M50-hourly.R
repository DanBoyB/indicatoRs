#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

retrieve_m50 <- function(year) {

    # create sequence of dates
    dates <- seq(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-01-02")), by = '1 day')

    day_types <- readr::read_csv("data-raw/dayTypes1219.csv") %>%
        mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01")))

    # motorway tmu sites
    sites <- c("000000001012",
               "000000001501",
               "000000001502",
               "000000001503",
               "000000001504",
               "000000001505",
               "000000001506",
               "000000001507",
               "000000001508",
               "000000001509",
               "000000015010",
               "000000015011",
               "000000015012")

    # produce dataframe of hourly traffic flows and speeds on M50
    hourly <- walk(dates, function(x) {

        # build list of urls
        url <- paste0("http://data.tii.ie/Datasets/TrafficCountData/",
                      year(x), "/",
                      sprintf("%02d", month(x)), "/",
                      sprintf("%02d", day(x)),
                      "/per-vehicle-records-",
                      x,
                      ".csv")
        # read csv, filter to M50 sites, group by hour, site, lane
        grp_hour <- read_csv(url) %>%
            filter(cosit %in% sites) %>%
            group_by(year, month, day, hour, cosit, lanename) %>%
            summarise(volume = n(),
                      avg_speed = mean(speed))

        # join day types data
        grp_hour <- grp_hour %>%
            mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
            left_join(day_types, by = "date")

        # calculate densities, los and define days
        grp_hour <- grp_hour %>%
            mutate(density = volume / avg_speed,
                   los = case_when(density <= 7 ~ "A",
                                   density > 7 & density <= 11 ~ "B",
                                   density > 11 & density <= 16 ~"C",
                                   density > 16 & density <= 22 ~ "D",
                                   density > 22 & density <= 28 ~ "E",
                                   density > 28 ~ "F"),
                   flow_cond = case_when(los %in% c("A", "B", "C") ~ "stable",
                                         los == "D" ~ "approaching unstable",
                                         los == "E" ~ "unstable",
                                         TRUE ~ "breakdown"),
                   day = case_when(dayType %in% c(0:4) ~ "Normal working day",
                                   dayType %in% c(7:11) ~ "School holiday",
                                   dayType %in% c(5:6, 12:14) ~ "Weekend / public holiday"))

        # define periods
        grp_hour <- grp_hour %>%
            mutate(period = case_when(dayType %in% c(5:6, 12:14) ~ "Off Peak",
                                      dayType %in% c(0:4, 7:11) & hour %in% c(0:6, 19:23) ~ "Off Peak",
                                      dayType %in% c(0:4) & hour %in% c(7:9) ~ "AM Working Day",
                                      dayType %in% c(0:4) & hour %in% c(16:18) ~ "PM Working Day",
                                      dayType %in% c(7:11) & hour %in% c(7:9) ~ "AM School Holiday",
                                      dayType %in% c(7:11) & hour %in% c(16:18) ~ "PM School Holiday",
                                      dayType %in% c(0:4) & hour %in% c(10:15) ~ "Inter Peak Working Day",
                                      dayType %in% c(7:11) & hour %in% c(10:15) ~ "Inter Peak School Holiday"))

        write_csv(grp_hour, paste0("data-raw/m50-", year, ".csv"), append = TRUE)

    })

}

retrieve_m50(2017)
