# option 0 : all
# option 1 : gps
# option 2 : accel xyz
# option 3 : linacc xyz
# option 4 : gyro xyz
# option 5 : roll, pitch, yaw
# option 6 : speed, heading

#' loading the telematics trip csv file.
#'
#' get_trip function will read the csv file which records the driving trip data
#' @importFrom magrittr "%>%"
get_trip <- function(data_folder = "", tripN,
                     data_option = 0,
                     sync_time = NULL){

    file_path <- paste0("./data/", data_folder,"/trip", tripN, ".csv")
    temp_data <- utils::read.csv(file_path, header = TRUE, fill = TRUE)

    temp_time <- as.character(temp_data$Timestamp) %>% strsplit(" ") %>% unlist
    temp_time <- temp_time[seq(2, length(temp_time), by = 2)] %>% strsplit(":")

    temp_data$Timestamp <- lapply(temp_time, convert_time) %>% unlist()

    if (is.null(sync_time)){
        start_time <- temp_data$Timestamp[1]
    } else {
        sync_time <- strsplit(sync_time, ":")
        start_time <- convert_time(sync_time)
    }

    if (data_option == 1) {
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp, Lat, Long)
        # data <- data[seq(10, dim(data)[1], by = 26),]
        # data <- dplyr::distinct(data, Lat,  .keep_all = TRUE)
        convertTOmeter <- function(pointVector){
            geosphere::distm(pointVector[1:2], pointVector[3:4], fun = geosphere::distHaversine)
        }
        tempD <- cbind.data.frame(head(data[,c(3,2)], -1),
                                  tail(data[,c(3,2)], -1))
        gps_speed <- apply(tempD, 1, convertTOmeter)
        # data$speed <- c(gps_speed, tail(gps_speed, 1)) * 2.27
        data$speed <- c(gps_speed, tail(gps_speed, 1)) * 2.23694
        colnames(data) <- c("Timestamp", "y", "x", "speed")
    } else if (data_option == 2) {
        # filter gps data
        data <- dplyr::select(temp_data, Timestamp,
                              accelX.g., accelY.g., accelZ.g.)
        colnames(data) <- c("Timestamp", "x", "y", "z")
        data$x <- data$x * 9.81865
        data$y <- data$y * 9.81865
        data$z <- data$z * 9.81865
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
    }

    data <- data %>%
        filter(Timestamp >= start_time) %>%
        mutate(Timestamp = Timestamp - start_time)
    # data$Timestamp <- data$Timestamp - start_time
    colnames(data)[1] <- "time"
    data
}
