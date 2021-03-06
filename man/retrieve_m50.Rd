% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve-m50.R
\name{retrieve_m50}
\alias{retrieve_m50}
\title{Output dataframe of M50 daily volumes for specified year}
\usage{
retrieve_m50(year)
}
\arguments{
\item{year}{The year required as integer}
}
\value{
A dataframe of M50 daily traffic flows
}
\description{
This function reads in base year AADT and the region in which
the scheme is located and outputs a list of traffic flow
projections based on PAG Unit 5.3 link based projections
}
