#' Sync OBD speed data with GPS speed data
#'
#' @name get_obd_trip
#' @param file_path file path of telematics data
#' @return synced OBD speed with time and speed of the vehicle
#' @export
get_obd_trip <- function(file_path){
    
    file_path_gps <- gsub(".txt", ".csv", file_path)
    
    # data loading
    gps_data <- get_trip(file_path_gps, data_option = 1)
    speed_data <- get_trip(file_path_gps, data_option = 6)
    speed_data_obd <- OBD2_data(file_path)
    
    # sync_obd_with_gps()
    speed_data_obd <- speed_from_obd(speed_data_obd, speed_data)

    # possible lag
    loss_fcn <- function(lag, speed_data_obd, speed_data){
        start_time <- lag
        if(start_time >= 0){
            padding <- which.max((speed_data$time - start_time) >= 0) - 1
            obd_speed_adjust <- utils::head(c(rep(0, padding), speed_data_obd$speed), 
                                     length(speed_data_obd$time))
            index <- ((obd_speed_adjust > 0 | speed_data$speed > 0) & gps_data$accuracy_horiz < 6)
            # loss <- sum(((obd_speed_adjust - speed_data$speed)[index])^2)
        } else {
            padding <- which.max((start_time + speed_data$time) > 0)
            obd_speed_adjust <- utils::tail(c(speed_data_obd$speed, rep(0, padding)), 
                                     length(speed_data_obd$time))
            index <- ((obd_speed_adjust > 0 | speed_data$speed > 0) & gps_data$accuracy_horiz < 6)
            # loss <- sum(((obd_speed_adjust - speed_data$speed)[index])^2)
        }
        loss <- sum(((obd_speed_adjust - speed_data$speed)[index])^2)
        loss
    }    
    
    plag <- seq(-5, 15, by = 0.25)
    result <- rep(0, length(plag))
    
    i <- 1
    for (lag in plag){
        result[i] <- loss_fcn(lag, speed_data_obd, speed_data)
        i <- i + 1
    }

    lag <- plag[which.min(result)]
    
    if(lag >= 0){
        padding <- which.max((speed_data$time - lag) > 0)
        speed_data_obd$speed <- utils::head(c(rep(0, padding), speed_data_obd$speed), 
                                     length(speed_data_obd$time))
    } else {
        padding <- which.max((lag + speed_data$time) > 0)
        speed_data_obd$speed <- utils::tail(c(speed_data_obd$speed, rep(0, padding)), 
                                     length(speed_data_obd$time))
    }
    speed_data_obd
}
