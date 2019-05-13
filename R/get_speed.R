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
#' @param gps_coordinates GPS coordinates
#' @return speed recorded as mile per hour
#' @export
speed_from_gps <- function(gps_coordinates){
    # data has time, x, y
    # tempD consists x1, y1, x2, y2
    tempD <- cbind.data.frame(
        gps_coordinates %>% dplyr::select(x, y) %>% utils::head(-1),
        gps_coordinates %>% dplyr::select(x, y) %>% utils::tail(-1)
    )

    delta_t <- with(gps_coordinates, utils::tail(time, -1) - utils::head(time, -1))

    gps_speed <- apply(tempD, 1, convertTOmeter) / delta_t
    gps_speed <- c(0, gps_speed) * 2.27
    data.frame(time = gps_coordinates$time , speed = gps_speed)
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

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("x", "y"))
}
