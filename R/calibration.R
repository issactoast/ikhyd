#' Calibarate telematics data
#'
#' @name calibrate_telematics
#' @param file_path file path of telematics data
#' @param sync_time sync_time
#' @return calibrated GPS data and accelerometer x-axis and y-axis data
#' @export
calibrate_telematics <- function(file_path, sync_time = NULL){
    gps_data <- get_trip(file_path, data_option = 1, sync_time)
    acc_data <- get_trip(file_path, data_option = 2, sync_time)
    gyro_data <- get_trip(file_path, data_option = 4, sync_time)
    speed_data <- get_trip(file_path, data_option = 6, sync_time)
    heading_data <- get_trip(file_path, data_option = 6, sync_time)


    # detect stops from acc_data
    stop_info <- stop_detection(acc_data)

    # calibration for acc y
    acc_data$y <- kalmanfilter_ay(acc_data, speed_data)
    acc_speed <- with(acc_data, speed_from_acc(time, y))

    # calibration for acc x
    acc_data$x <- kalmanfilter_ax(acc_data, gyro_data, acc_speed)

    # gps smoothing
    gps_data <- smoothing_gps(gps_data, stop_info)

    # cali_acc_data <- data.frame(time = acc_data$time,
    #                             x = acc_x, y = acc_y)

    cali_gps <- kalmanfilter_gps(gps_data, acc_data,
                                 heading_data)

    list(acc_data = acc_data,
         gps_data = cali_gps)
}


