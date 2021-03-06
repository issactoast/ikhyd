% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlm_model.R
\name{kalmanfilter_telematics}
\alias{kalmanfilter_telematics}
\title{Calibrate telematics data with the Kalman filtering}
\usage{
kalmanfilter_telematics(file_path, GPS_outage = NULL)
}
\arguments{
\item{file_path}{route file path}

\item{GPS_outage}{index vector of indicating that the signal is outage}
}
\value{
dataframe of time, lon & lat acc., and speed
}
\description{
Calibrate telematics data with the Kalman filtering
}
