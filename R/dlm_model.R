#' make y vectors for calibration
#'
#' @name make_y
#' @param file_path route file path
#' @return dataframe of observations
make_y <- function(file_path){
    gps_data   <- get_trip(file_path, data_option = 1)
    acc_data   <- get_trip(file_path, data_option = 2)
    acc_data <- sweep(acc_data, 2, c(0, -1.082211e-03, -1.281193e-03, 1.410833e-01), "-")
    gyro_data  <- get_trip(file_path, data_option = 4)
    gyro_data  <- sweep(gyro_data, 2, c(0, -0.01620287, 0.02951921, -0.01056747), "-") 
    angle_data <- get_trip(file_path, data_option = 5)
    speed_data <- get_trip(file_path, data_option = 6)
    
    n <- length(speed_data$speed)
    index <- rep(TRUE, n)
    index[seq(1, n, by = 25)] <- FALSE
    speed_data$speed[index] <- NA
    
    gps_lateral <- telematics_data_fromGPS(gps_data,
                                           speed_data)
    
    k <- length(gps_lateral$time)
    gps_omega_vec <- rep(NA, n)
    gps_later_vec <- rep(NA, n)
    for (i in 1:k){
        pos <- which.min((gps_lateral$time[i] - speed_data$time)^2)
        gps_later_vec[pos] <- gps_lateral$a_lat[i]
        gps_omega_vec[pos] <- gps_lateral$omega[i]
    }
    
    data.frame(time = acc_data$time,
               speed = speed_data$speed * 0.44704,
               acc_y = acc_data$y,
               acc_x = acc_data$x,
               latgps = gps_later_vec,
               latimu = speed_data$speed * 0.44704 * -gyro_data$gyroZ.rad.s.,
               time = acc_data$time,
               rollangle = angle_data$Roll.rads.,
               yawrate = -gyro_data$gyroZ.rad.s.,
               omega_gps = gps_omega_vec)
    
}

#' Calibrate telematics data with the Kalman filtering
#'
#' @name kalmanfilter_telematics
#' @param file_path route file path
#' @param GPS_outage index vector of indicating that the signal is outage
#' @return dataframe of time, lon & lat acc., and speed
#' @export
kalmanfilter_telematics <- function(file_path, GPS_outage = NULL){

    y <- make_y(file_path)
    y <- dplyr::mutate(y, bias = - 9.81865 * rollangle)
    
    # GPS outage reflect
    if (!is.null(GPS_outage)){
        y$speed[GPS_outage] <- NA
    }

    # observation vector making    
    obs_vec <- stats::ts(y[,c("speed", "acc_y", "acc_x",
                       "latimu", "latgps")], frequency = 25)
    
    # filtering
    myFilter <- dlm::dlmFilter(obs_vec, dlm_model)
    
    # using lowpass to get stable result
    a_y <- low_pass(myFilter$m[-1,2], 0.2)
    a_x <- low_pass(myFilter$m[-1, 3], 0.2)
    
    data.frame(time = y$time,
               a_lon = a_y,
               a_lat = a_x,
               speed = low_pass(myFilter$m[-1,1] * 2.23694, 0.2))
}

#' Calibrate telematics data with the Kalman smoothing
#'
#' @name kalmansmooth_telematics
#' @param file_path route file path
#' @param GPS_outage index vector of indicating that the signal is outage
#' @return dataframe of time, lon & lat acc., and speed
#' @export
kalmansmooth_telematics <- function(file_path, GPS_outage = NULL){

    y <- make_y(file_path)
    y <- dplyr::mutate(y, bias = - 9.81865 * rollangle)
    
    if (!is.null(GPS_outage)){
        y$speed[GPS_outage] <- NA
    }
    
    obs_vec <- stats::ts(y[,c("speed", "acc_y", "acc_x",
                       "latimu", "latgps")], frequency = 25)
    
    myFilter <- dlm::dlmFilter(obs_vec, dlm_model)
    myFilter <- dlm::dlmSmooth(myFilter)

    data.frame(time = y$time,
               a_lon = dlm::dropFirst(myFilter$s[,2]),
               a_lat = dlm::dropFirst(myFilter$s[,3]),
               speed = dlm::dropFirst(myFilter$s[,1]) / 0.44704)
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("rollangle"))
}