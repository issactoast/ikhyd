#' convert time stamp into seconds
#'
#' @name convert_time
#' @param timelist time value in list consists of 3 elements;
#' the first element is hour, the second element is min, the last one is seconds.
#' @return seconds corresponds with the input hour
convert_time <- function(timelist){
    timelist <- unlist(timelist)
    hr <- as.numeric(timelist[1]) * 3600
    min <- as.numeric(timelist[2]) * 60
    sec <- as.numeric(timelist[3])
    result <- hr + min + sec
    result
}
#' Loading the telematics trip csv file.
#'
#' get_trip function will read a csv file which records the driving trip data
#' @name get_trip
#' @importFrom magrittr "%>%"
#' @param data_path a path that the trip csv file located
#' @param data_option the variables that the user want to select from the data. \cr
#' The following list explains the options; \cr
#' Option 0 : all \cr
#' Option 1 : gps \cr
#' Option 2 : accel xyz \cr
#' Option 3 : linacc xyz \cr
#' Option 4 : gyro xyz \cr
#' Option 5 : roll, pitch, yaw \cr
#' Option 6 : speed, heading
#' Option 7 : magnetometer x, y, z \cr
#' @param sync_time this arguments will be used when the recorded data has different time line with video.
#' @export
get_trip <- function(data_path,
                     data_option = 0,
                     sync_time = NULL){

    temp_data <- utils::read.csv(data_path, header = TRUE, fill = TRUE)

    temp_time <- as.character(temp_data$Timestamp) %>% strsplit(" ") %>% unlist
    temp_time <- temp_time[seq(2, length(temp_time), by = 2)] %>% strsplit(":")

    temp_data$Timestamp <- lapply(temp_time, convert_time) %>% unlist()

    if (is.null(sync_time)){
        start_time <- temp_data$Timestamp[1]
    } else {
        sync_time <- strsplit(sync_time, ":")
        start_time <- convert_time(sync_time)
    }

    if (data_option == 0){
        data <- temp_data
    } else if (data_option == 1) {
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp, Long, Lat,
                              Alt.m., HorizontalAccuracy.m., VerticalAccuracy.m.)
        colnames(data) <- c("Timestamp", "x", "y", "z", "accuracy_horiz", "accuracy_vert")
    } else if (data_option == 2) {
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              accelX.g., accelY.g., accelZ.g.)
        colnames(data) <- c("Timestamp", "x", "y", "z")
        data$x <- -data$x * 9.81865
        data$y <- -data$y * 9.81865
        data$z <- -data$z * 9.81865
    } else if (data_option == 3){
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              accelUserX.g., accelUserY.g., accelUserZ.g.)
        colnames(data) <- c("Timestamp", "x", "y", "z")
        data$x <- data$x * 9.81865
        data$y <- data$y * 9.81865
        data$z <- data$z * 9.81865
    } else if(data_option == 4){
        # filter gyroscope data
        data <- dplyr::select(temp_data, Timestamp,
                              dplyr::starts_with("gyro"))
    } else if(data_option == 5){
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              dplyr::ends_with("rads."))
    } else if (data_option == 6){
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              Speed.m.s., TrueHeading)
        data$Speed.m.s. <- data$Speed.m.s. * 2.23694
        colnames(data)[2] <- "speed"
    } else if (data_option == 7){
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              magX.ÂµT., magY.ÂµT., magZ.ÂµT.)
        colnames(data) <- c("Timestamp", "x", "y", "z")
    }

    data <- data %>%
        dplyr::filter(Timestamp >= start_time) %>%
        dplyr::mutate(Timestamp = Timestamp - start_time)
    colnames(data)[1] <- "time"
    data
}

if(getRversion() >= "2.15.1") {
    temp_data <- utils::read.csv(system.file("extdata", "sample_trip.csv", package = "ikhyd"),
                                 header = TRUE, fill = TRUE)
    utils::globalVariables(names(temp_data))
}
