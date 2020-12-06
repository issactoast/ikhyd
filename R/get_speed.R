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

    n <- dim(stop_info)[1]
    if (n == 0){
        return(speed_data)
    } else {
        unique_pos <- !duplicated(speed_data$speed)

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

    text_data <- readLines(file_path)

    gps_part <- text_data[grep("gps", text_data)]
    result <- unlist(strsplit(gps_part, ";"))[-c(1:7)]

    gps_data_contents <- result[grep(":", result)]
    gps_data_contents <- unlist(strsplit(as.character(gps_data_contents), ":"))
    y <- gps_data_contents[c(T, F, F, F, F, F, F)]
    x <- gps_data_contents[c(F, T, F, F, F, F, F)]

    gps_data_time <- anytime::anytime(as.numeric(as.character(gps_data_contents[c(F, F, F, T, F, F, F)]))/1000)

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
#' Find big difference in derivative with same sampling rate data
#'
#' @name indicate_error
#' @param my_vector vector needed to be checked out
#' @param threshold threshold value
#' @return an index vector whose valuse is False at the invalid position
indicate_error <- function(my_vector, threshold){
    n <- length(my_vector)
    index_vec <- rep(TRUE, n)

    for (i in 2:n){
        if (abs(my_vector[i] - my_vector[i-1]) > threshold){
            index_vec[i] <- FALSE
        }
    }
    index_vec
}

#' Vertical speed from Barometer data
#' @name vspeed_from_baro
#' @param alt_data the altitude data
#' @return altitude data with vertical speed
#' @export
vspeed_from_baro <- function(alt_data){
    sub_data <- alt_data[c(rep(F, 24), T), ]

    ds <- with(sub_data, tail(rel.alt, -1) - head(rel.alt, -1))
    vert_velocity <- c(0, ds)
    my_index <- indicate_error(vert_velocity, 0.4)
    sub_data$vert_vel <- vert_velocity

    sub_data <- sub_data[my_index,]

    result <- with(sub_data,
                   stats::smooth.spline(
                       time, vert_vel,
                       df = ceiling(length(vert_vel) * 0.7)))
    alt_data$pre_vert_vel <- with(sub_data,
                                  stats::predict(result, alt_data$time)$y) * 2.23694
    alt_data
}

#' Spline obd speed data from OBD2 device
#'
#' @name speed_from_obd
#' @param obd_data OBD2 data
#' @param speed_data the speed data that you want to sync, which consists of time and speed
#' @return a estimated speed using spline from OBD2 speed data
#' @export
speed_from_obd <- function(obd_data, speed_data){

    unique_time <- unique(obd_data$time)
    n <- length(unique_time)
    index <- rep(0, n)

    for (i in 1:n){
        index[i] <- which(obd_data$time == unique_time[i])
    }

    obd_data <- obd_data[index,]

    s1 <- with(obd_data,
               stats::splinefun(time, speed,
                                method = "monoH.FC"))

    obd_speed = s1(speed_data$time)

    # Calibration
    a <- which.max(speed_data$speed > 0)
    b <- which.max(obd_speed > 0)
    if (a > b){
        obd_speed <- utils::head(c(rep(0, abs(a-b)), obd_speed), -abs(a - b))
    } else if ( a < b){
        obd_speed <- utils::tail(c(obd_speed, rep(0, abs(a-b))), -abs(a - b))
    }

    data.frame(time = speed_data$time,
               speed = obd_speed)
}

#' Calculate acceleration from OBD2 data
#'
#' @name acc_from_obd
#' @param obd_speed obd_speed which is splined already
#' @return a estimated longitudinal acceleration from OBD2 speed data
#' @export
acc_from_obd <- function(obd_speed){
    dt <- utils::tail(obd_speed$time, -1) - utils::head(obd_speed$time, -1)

    # dv in m/s unit
    dv <- utils::tail(obd_speed$speed * 0.44704, -1) - utils::head(obd_speed$speed * 0.44704, -1)

    acc <- c(0, dv / dt)
    obd_speed$dv_dt <- c(tail(acc, -25), rep(0, 25))
    obd_speed
}

#' altitude data length adjustment.
#'
#' @name adjust_alt
#' @param time_data a data which has time column
#' @param alt_data an altidute data
#' @return length adjusted alt data
#' @export
adjust_alt <- function(time_data, alt_data){
    alt_data <- alt_data %>%
        dplyr::mutate(time = time - 3) %>%
        dplyr::filter(time >= 0)

    n <- length(alt_data$rel.alt)
    m <- length(time_data$time)

    alt_data <- data.frame(time = time_data$time,
                           rel.alt = c(alt_data$rel.alt,
                                       rep(alt_data$rel.alt[n], m-n)),
                           pressure = c(alt_data$pressure,
                                        rep(alt_data$pressure[n], m-n)))
    alt_data
}


if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("x", "y"))
}
