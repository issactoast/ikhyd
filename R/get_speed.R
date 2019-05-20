#' Calculate distance between two GPS points into meter scale
#'
#' @name converTOmeter
#' @param pointVector two GPS coordinate points
#' @return distance between two GPS points
convertTOmeter <- function(pointVector){
    geosphere::distm(pointVector[1:2], pointVector[3:4],
                     fun = geosphere::distHaversine)
}

#' Calculate speed(mph) from GPS coordinates
#'
#' @name speed_from_gps
#' @param gps_data GPS coordinates
#' @return speed recorded as mile per hour
#' @export
speed_from_gps <- function(gps_data){
    # data has time, x, y
    # tempD consists x1, y1, x2, y2
    tempD <- cbind.data.frame(
        gps_data %>% dplyr::select(x, y) %>% utils::head(-1),
        gps_data %>% dplyr::select(x, y) %>% utils::tail(-1)
    )

    delta_t <- with(gps_data, utils::tail(time, -1) - utils::head(time, -1))

    gps_speed <- apply(tempD, 1, convertTOmeter) / delta_t
    gps_speed <- c(0, gps_speed) * 2.27
    data.frame(time = gps_data$time , speed = gps_speed)
}


#' Smooting speed(mph) from speed data
#'
#' @name smoothing_speed
#' @param speed_data speed_data
#' @param stop_info stop information
#' @return smoothed speed data recorded as mile per hour
#' @export
smoothing_speed <- function(speed_data, stop_info){

    unique_pos <- !duplicated(speed_data$speed)

    n <- dim(stop_info)[1]
    for(i in 1:n){
        speed_data$speed[stop_info$stop_start[i]:stop_info$stop_end[i]] <- 0
        unique_pos[stop_info$stop_start[i]:stop_info$stop_end[i]] <- TRUE
    }

    unique_speed <- speed_data[unique_pos,]

    # "fmm", "periodic", "natural", "monoH.FC"
    s1 <- with(unique_speed, stats::splinefun(time, speed, method = "monoH.FC"))

    data.frame(time = speed_data$time,
               speed = s1(speed_data$time),
               TrueHeading = speed_data$TrueHeading)
}


#' Calculate speed(mph) from accelerometer y values
#'
#' @name speed_from_acc
#' @param time_vec time vector
#' @param acc_vec accelerometer vector
#' @return a speed vector recorded as mile per hour
#' @export
speed_from_acc <- function(time_vec, acc_vec){
    dt <- (utils::tail(time_vec, -1) - utils::head(time_vec, -1))
    smdv_y <- utils::head(acc_vec, -1) * dt
    velocity <- rep(0, length(smdv_y))

    for( i in 2:length(smdv_y)){
        velocity[i] <- velocity[i-1] + smdv_y[i] * 2.23694
    }
    velocity <- c(0, velocity)
    velocity
}

#' Read speed data from OBD2 device
#'
#' @name OBD2_data
#' @param file_path OBD2 data
#' @return a OBD2 data with speed
#' @export
OBD2_data <- function(file_path){
    text_data <- readLines(file_path)
    speed_part <- text_data[grep("010D", text_data)]
    result <- unlist(strsplit(speed_part, ";"))[-c(1:3)]

    speed_data_time <- anytime::anytime(as.numeric(as.character(result[c(TRUE, FALSE, FALSE)]))/1000)

    start_time <- as.character(speed_data_time[1])

    speed_data_time <- unlist(strsplit(as.character(speed_data_time), " "))

    speed_data <- data.frame(time = speed_data_time[c(FALSE, TRUE)],
                             speed = result[c(FALSE, FALSE, TRUE)])

    time <- lubridate::period_to_seconds(lubridate::hms(speed_data$time))
    time <- time - time[1]

    speed_data$time <- time
    speed_data$speed <- as.numeric(as.character(speed_data$speed)) * 0.621371
    attr(speed_data, "start_time") <- start_time
    speed_data
}

#' Read speed data from OBD2 device
#'
#' @name OBD2gps_data
#' @param file_path OBD2 data
#' @return a OBD2 data with GPS
#' @export
OBD2gps_data <- function(file_path){
    file_path <- system.file("extdata", "trip2.txt", package = "ikhyd")
    text_data <- readLines(file_path)

    gps_part <- text_data[grep("gps", text_data)]
    result <- unlist(strsplit(gps_part, ";"))[-c(1:7)]

    gps_data_contents <- result[c(FALSE, FALSE, TRUE)]
    gps_data_contents <- unlist(strsplit(as.character(gps_data_contents), ":"))
    y <- gps_data_contents[c(T, F, F, F, F, F, F)]
    x <- gps_data_contents[c(F, T, F, F, F, F, F)]

    gps_data_time <- anytime::anytime(as.numeric(as.character(result[c(TRUE, FALSE, FALSE)]))/1000)

    start_time <- as.character(gps_data_time[1])

    gps_data_time <- unlist(strsplit(as.character(gps_data_time), " "))

    time = gps_data_time[c(FALSE, TRUE)]
    time <- lubridate::period_to_seconds(lubridate::hms(time))
    time <- time - time[1]

    gps_data <- data.frame(time = time,
                           x = as.numeric(as.character(x)),
                           y = as.numeric(as.character(y)))
    attr(gps_data, "start_time") <- start_time
    gps_data
}


if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("x", "y"))
}
