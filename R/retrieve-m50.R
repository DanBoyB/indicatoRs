#' Output dataframe of M50 daily volumes for specified year
#'
#' This function reads in base year AADT and the region in which
#' the scheme is located and outputs a list of traffic flow
#' projections based on PAG Unit 5.3 link based projections
#'
#' @param year The year required as integer
#'
#' @import dplyr
#' @import readr
#' @import lubridate
#' @import purrr
#' @return A dataframe of M50 daily traffic flows
#' @export
#'

retrieve_m50 <- function(year) {

    # create sequence of dates
    dates <- seq(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-01-02")), by = '1 day')

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

    # produce daily csv of 5 minute aggregate median speeds
    daily <- map_df(dates, function(x) {

                # build list of urls
        url <- paste0("http://data.tii.ie/Datasets/TrafficCountData/",
                      year(x), "/",
                      sprintf("%02d", month(x)), "/",
                      sprintf("%02d", day(x)),
                      "/per-site-class-aggr-",
                      x,
                      ".csv")

        read_csv(url) %>%
            filter(cosit %in% sites)


    })

    return(daily)
}
