#' Calibarate telematics data
#'
#' @name calibrate_telematics
#' @param file_path file path of telematics data
#' @return calibrated GPS data and accelerometer x-axis and y-axis data
#' @export
calibrate_telematics <- function(file_path){
    gps_data <- get_trip(file_path, data_option = 1)
    acc_data <- get_trip(file_path, data_option = 2)
    gyro_data <- get_trip(file_path, data_option = 4)
    heading_data <- get_trip(file_path, data_option = 6)

    # gps smoothing
    gps_data <- smoothing_gps(gps_data)

    # calculate speed from gps
    gps_speed <- speed_from_gps(gps_data)

    # calibration for acc y
    acc_y <- kalmanfilter_ay(acc_data, gps_speed)
    acc_speed <- speed_from_acc(acc_data$time, acc_y)

    # calibration for acc x
    acc_x <- kalmanfilter_ax(acc_data, gyro_data, acc_speed)

    cali_acc_data <- data.frame(time = acc_data$time,
                                x = acc_x, y = acc_y)

    cali_gps <- kalmanfilter_gps(gps_data, cali_acc_data,
                                 heading_data)

    list(acc_data = cali_acc_data,
         gps_data = cali_gps)
}
