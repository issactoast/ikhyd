% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_speed.R
\name{indicate_error}
\alias{indicate_error}
\title{Find big difference in derivative with same sampling rate data}
\usage{
indicate_error(my_vector, threshold)
}
\arguments{
\item{my_vector}{vector needed to be checked out}

\item{threshold}{threshold value}
}
\value{
an index vector whose valuse is False at the invalid position
}
\description{
Find big difference in derivative with same sampling rate data
}
