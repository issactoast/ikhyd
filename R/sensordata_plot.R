#' Visualization of accelerometer sensor data
#'
#' @name plot_acc
#' @param data accelerometer dataframe
#' @param tripname name of the trip
#' @param rate low pass filter parameter 0 to 1
#' @param ... arguments supported by base plot
#' @return plot of three axes accelerometer data
#' @export
plot_acc <- function(data, rate = 1, tripname = "a trip", ...){
    # prepare plotting
    graphics::par(mfrow = c(3, 1), mar = c(0, 5, 3, 2))
    plot(data$time, low_pass(data$x, rate),
         main = paste("Accelerometer information of", tripname),
         xlab = "Time", ylab = "\n x-axis",
         type = "l", ylim = c(-4, 4), xaxt = 'n', ...)
    graphics::abline(h = 0)
    graphics::par(mar = c(1.5, 5, 1.5, 2))
    plot(data$time, low_pass(data$y, rate),
         ylab = "Acceleration (m/s^2) \n y-axis", 
         type = "l", ylim = c(-4, 4), xaxt = 'n',...)
    graphics::abline(h = 0)
    graphics::par(mar = c(3, 5, 0, 2))
    plot(data$time, low_pass(data$z, rate),
         xlab = "Time (sec.) \n", ylab = "\n x-axis",
         type = "l", ylim = c(8, 12), ...)
    graphics::abline(h = 9.81865)
    
    # reset
    graphics::par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
}
#' Visualization of gps speed data
#'
#' @name plot_acc
#' @param data speed information dataframe (obd_speed can be added)
#' @param tripname name of the trip
#' @param showobd show obd speed info.
#' @param ... arguments supported by base plot
#' @return plot of speed information
#' @export
plot_speed <- function(data, tripname = "a trip", showobd = TRUE,...){
    plot(data$time, data$speed, type = "l",
         main = paste("Speed information", tripname) ,
         xlab = "Time (sec.)",
         ylab = "Speed (mph)", ...)
    if ("obd_speed" %in% names(data) & obd == TRUE) {
        graphics::points(data$time, data$obd_speed, type = "l", lty = "dashed")
    }
    graphics::abline(h = 0)
}
#' Visualization of telematics data
#'
#' @name plot_telematics
#' @param data speed information dataframe (obd_speed can be added)
#' @param rate low pass filter parameter (smoothing 0 to 1)
#' @param methodname name of the method
#' @param ... arguments supported by base plot
#' @return plot of calibrated telematics data
#' @export
plot_telematics <- function(data, rate = 1, methodname = "the calibration", ...){
    graphics::par(mfrow = c(3, 1))
    graphics::par(mar = c(0,5,3,2))
    plot(data$time, low_pass(data$a_lon, rate),
         main = paste("Model output from", methodname),
         xlab = "Time", ylab = "Acceleration \n longitudinal (m/s^2)",
         type = "l", ylim = c(-4, 4), xaxt = 'n', ...)
    graphics::abline(h = 0)
    graphics::par(mar = c(1.5,5,1.5,2))
    plot(data$time, low_pass(data$a_lat, rate),
         ylab = "Acceleration \n lateral (m/s^2)", 
         type = "l", ylim = c(-4, 4), xaxt = 'n',...)
    graphics::abline(h = 0)
    graphics::par(mar = c(3,5,0,2))
    max_speed <- max(data$speed)
    plot(data$time, low_pass(data$speed, rate),
         xlab = "Time (sec.) \n", ylab = "Speed (mph) \n",
         type = "l", ylim = c(0, max_speed), ...)
    graphics::abline(h = 0)
    
    # reset
    graphics::par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
}
#' Visualization of telematics data
#'
#' @name plot_acc_compare
#' @param data speed information dataframe (obd_speed can be added)
#' @param rate low pass filter parameter (smoothing 0 to 1)
#' @param sensor_name names of the sensors
#' @param ... arguments supported by base plot
#' @return comparison plot of two sensor data
#' @export
plot_acc_compare <- function(data, rate = 0.2, 
                             sensor_name = c("sensor 1", "sensor 2"), ...){
    # prepare plotting
    graphics::par(mar = c(4.1, 4.1, 4.1, 1.1))
    
    # prepare plotting
    plot(data$time, low_pass(data$acc1, rate),
         main = paste("Sensor values comparison \n", sensor_name[1], "vs.", sensor_name[2]),
         xlab = "Time \n", ylab = "Accelerations (m/s^2)",
         col = "red", type = "l", ylim = c(-4, 4), ...)
    graphics::points(data$time, low_pass(data$acc2, rate), type = "l", ...)
    graphics::abline(h = 0)
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("max_speed"))
}