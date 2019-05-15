#' Smoothing GPS data using smoothing spline
#'
#' @name smoothing_gps
#' @param gps_data the collection of GPS coordinates with time info.
#' @param acc_data the accelerometer data
#' @return smoothed GPS data with same number of GPS points.
#' @export
smoothing_gps <- function(gps_data, acc_data){
    stop_info <- stop_detection(acc_data)
    gps_data <- stop_correction(gps_data, stop_info)

    unique_pos <- !duplicated.array(gps_data[,2:3])

    n <- dim(stop_info)[1]
    for(i in 1:n){
        unique_pos[stop_info$stop_start[i]:stop_info$stop_end[i]] <- TRUE
    }

    unique_gps <- gps_data[unique_pos,]
    result <- with(unique_gps, stats::smooth.spline(time, x, nknots = length(time)))

    data.frame(time = gps_data$time,
               x = stats::predict(with(unique_gps, stats::smooth.spline(time, x, nknots = length(time))),
                                  gps_data$time)$y,
               y = stats::predict(with(unique_gps, stats::smooth.spline(time, y, nknots = length(time))),
                                  gps_data$time)$y)
}


# library(ikhyd)
# acc_data <- get_trip(system.file("extdata", "sample_trip.csv", package = "ikhyd"),
#                      data_option = 2)
#
# gps_data <- get_trip(system.file("extdata", "sample_trip.csv", package = "ikhyd"),
#                      data_option = 1)

#' Correction of GPS based on stop information
#'
#' @name stop_correction
#' @param gps_data the GPS data
#' @param stop_info the information of stop
#' @return A GPS data which does not have different GPS points when the vehicle is stop
stop_correction <- function(gps_data, stop_info){
    n <- dim(stop_info)[1]

    for(i in 1:n){
        tempD <- unique.array(gps_data[stop_info$stop_start[i]:stop_info$stop_end[i], 2:3])
        if (dim(tempD)[1] == 1){
            next
        } else {
            gps_data[stop_info$stop_start[i]:stop_info$stop_end[i], 2:3] <- tempD[1,]
        }
    }
    gps_data
}

#' Detecting stops from accelerometer y-axis
#'
#' @name stop_detection
#' @param acc_data the accelerometer data
#' @return A dataframe containing the position of stop starts and ends, and length of stop.
stop_detection <- function(acc_data){

    acc_vec <- acc_data$y
    window_size = 3 * 25
    result <- rep(0, length(acc_vec))
    mean_value <- rep(0, length(acc_vec))
    for (i in 1:(length(acc_vec) - window_size)){
        check_range <- stats::var(acc_vec[i:(i + window_size)])
        if (check_range < 0.004){
            result[i] <- 1
            mean_value[i] <- mean(acc_vec[i:(i + window_size)])
        }
    }

    for (i in (1 + window_size):length(acc_vec)){
        check_range <- stats::var(acc_vec[(i-window_size):i])
        if (check_range < 0.004){
            result[i] <- 1
            mean_value[i] <- mean(acc_vec[(i-window_size):i])
        }
    }

    dresult <- (utils::tail(result, -1) - utils::head(result, -1))

    stop_start <- which(dresult == 1) + 1
    stop_end   <- which(dresult == -1)

    if (stop_start[1] > stop_end[1]) { stop_start <- c(1, stop_start) }
    if (utils::tail(stop_start, 1) > utils::tail(stop_end, 1)) {
        stop_end <- c(stop_end, length(acc_vec))
    }

    check_vec <- utils::tail(stop_start, -1) - utils::head(stop_end, -1)
    check_vec <- c(check_vec, 1000)
    nstop <- sum(check_vec > 25)
    result <- matrix(0, nrow = nstop, ncol = 2)

    k <- 1
    i <- 1
    while(k != (nstop + 1)){
        result[k, 1] <- stop_start[i]
        while(check_vec[i] < 25) {
            i <- i + 1
        }
        result[k, 2] <- stop_end[i]
        k <- k + 1
        i <- i + 1
    }

    stop_start <- result[,1]
    stop_end <- result[,2]

    data.frame(stop_start = result[,1],
               stop_end   = result[,2],
               legnth_sec = (result[,2] - result[,1])/25 )
}

