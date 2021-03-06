% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kalmanfilter.R
\name{kalmanfilter_ay}
\alias{kalmanfilter_ay}
\title{Calibarate accelerometer y-axis using speed data}
\usage{
kalmanfilter_ay(acc_data, speed_data, gps_data = NULL)
}
\arguments{
\item{acc_data}{accelerometer data containing time, x, y, z}

\item{speed_data}{speed data containing time and speed.}

\item{gps_data}{gps data for weak signal}
}
\value{
calibrated y-axis accelerometer data
}
\description{
Calibarate accelerometer y-axis using speed data
}
