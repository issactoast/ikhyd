#' Smoothing GPS data using smoothing spline
#'
#' @name smoothing_gps
#' @param gps_data the collection of GPS coordinates with time info.
#' @return smoothed GPS data with same number of GPS points.
#' @export
smoothing_gps <- function(gps_data){
    data.frame(time = gps_data$time,
               x = with(gps_data, stats::predict(stats::smooth.spline(time, x, nknots = ceiling(1.5 * max(time))), time))$y,
               y = with(gps_data, stats::predict(stats::smooth.spline(time, y, nknots = ceiling(1.5 * max(time))), time))$y)
}
