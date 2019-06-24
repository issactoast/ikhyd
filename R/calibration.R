#' Calibarate telematics data
#'
#' @name calibrate_telematics
#' @param file_path file path of telematics data
#' @param sync_time sync_time
#' @return calibrated long. and lat. acceleration of the vehicle
#' @export
calibrate_telematics <- function(file_path, sync_time = NULL){

        gps_data <- get_trip(file_path, data_option = 1, sync_time)
        acc_data <- get_trip(file_path, data_option = 2, sync_time)
       gyro_data <- get_trip(file_path, data_option = 4, sync_time)
      angle_data <- get_trip(file_path, data_option = 5, sync_time)
      speed_data <- get_trip(file_path, data_option = 6, sync_time)
        alt_data <- get_trip(file_path, data_option = 8, sync_time)
        alt_data <- adjust_alt(acc_data, alt_data)
        alt_data <- vspeed_from_baro(alt_data)
        speed_data <- speed_from_obd(speed_data, speed_data)

    # acc_z from alt_data
    dvt <- with(alt_data, tail(pre_vert_vel, -1) - head(pre_vert_vel, -1))
    dt <- with(alt_data, tail(time, -1) - head(time, -1))
    alt_data$acc_z <- c(0, (dvt / 2.23694) / dt)

    # long. acc from GPS speed
    dvt <- with(speed_data, tail(speed, -1) - head(speed, -1))
    dt <- with(speed_data, tail(time, -1) - head(time, -1))
    speed_data$a_y <- c(0, (dvt / 2.23694) / dt)

    # Get long. acc via Kalman filter
    result <- kalmanfilter_withalpha(acc_data,
                                     speed_data,
                                     angle_data,
                                     alt_data)

    acc_data$lon_acc <- result$acc_y
    acc_data$speed <- result$speed
    angle_data$roadgrade <- result$alpha

    # Get lat. acc via gyro, acc, and estimated speed
    acc_data <- dplyr::select(acc_data, -time) %>%
        cbind.data.frame(gyro_data) %>%
        dplyr::mutate(lat_acc = x - 0.44704 * speed * gyroZ.rad.s. +
                   with(angle_data, 9.81865 * Roll.rads.)) %>%
        dplyr::select(time, x, y, z, lon_acc, lat_acc, speed)

    data.frame(time = acc_data$time,
               a_lon = low_pass(acc_data$lon_acc, 0.2),
               a_lat = low_pass(acc_data$lat_acc, 0.2))
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("x", "y", "z", "lon_acc", "lat_acc"))
}
