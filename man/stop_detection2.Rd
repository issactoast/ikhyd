% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gps_smoothing.R
\name{stop_detection2}
\alias{stop_detection2}
\title{Detecting stops from accelerometer y-axis using regresion}
\usage{
stop_detection2(acc_sub, interval_sec = 3, slope_check = 0.005)
}
\arguments{
\item{acc_sub}{the accelerometer data}

\item{interval_sec}{interval check}

\item{slope_check}{thresh hold value}
}
\value{
A dataframe containing the position of stop starts and ends, and length of stop.
}
\description{
Detecting stops from accelerometer y-axis using regresion
}
