#' Smoothing GPS data using smoothing spline
#'
#' @name smoothing_gps
#' @param gps_data the collection of GPS coordinates with time info.
#' @param stop_info stop infomation
#' @return smoothed GPS data with same number of GPS points.
#' @export
smoothing_gps <- function(gps_data, stop_info){

    gps_data <- stop_correction(gps_data, stop_info)

    unique_pos <- !duplicated.array(gps_data[,2:3])

    n <- dim(stop_info)[1]
    for(i in 1:n){
        unique_pos[stop_info$stop_start[i]:stop_info$stop_end[i]] <- TRUE
    }

    unique_gps <- gps_data[unique_pos,]

    # "fmm", "periodic", "natural", "monoH.FC"
    # s1 <- with(unique_gps, stats::splinefun(time, x, method = "monoH.FC"))
    # s2 <- with(unique_gps, stats::splinefun(time, y, method = "monoH.FC"))

    s1 <- with(unique_gps, stats::approxfun(time, x, method = "linear"))
    s2 <- with(unique_gps, stats::approxfun(time, y, method = "linear"))

    data.frame(time = gps_data$time,
               x = s1(gps_data$time),
               y = s2(gps_data$time))
}

gps_data1 <- spline_gps(gps_data, acc_data)
# gps_data2 <- spline_gps2(gps_data, acc_data)

m <- 40
k <- floor( max(gps_data$time) / m )
i <- 1
for (i in 1:k){
    st_p <- (m/2 * (i-1))
    if (i == k){
        gps_sub <- gps_data1 %>%
            filter(time >= st_p)

        with(gps_sub, plot(x, y, type = "l"), asp=1)
        with(gps_data %>% filter(time >= st_p),
             points(x, y))
        with(gps_data %>% filter(accuracy_horiz > 5),
             points(x, y, col = "red"))
    } else {
        gps_sub <- gps_data1 %>%
            filter(time >= st_p & time <= (st_p + (m-1)))
        with(gps_sub, plot(x, y, type = "l",
                           main = paste(i)), asp=1)
        with(gps_data %>% filter(time >= st_p &
                                     time <= (st_p + (m-1))),
             points(x, y))
        with(gps_data %>% filter(accuracy_horiz > 5),
             points(x, y, col = "red"))
    }
}

spline_gps <- function(gps_data, acc_data){

    k <- floor( max(gps_data$time) / 30 )

    for (i in 1:k){
        print(paste(i, "out of", k))

        st_p <- (30 * (i-1))
        if (i == k){
            gps_sub <- gps_data %>%
                filter(time >= st_p)
            acc_sub <- acc_data %>%
                filter(time >= st_p)
        } else {
            gps_sub <- gps_data %>%
                filter(time >= st_p & time <= (st_p+30))
            acc_sub <- acc_data %>%
                filter(time >= st_p & time <= (st_p+30))
        }

        stop_info <- stop_detection2(acc_sub,
                                     slope_check = 0.005)

        gps_sub <- stop_correction(gps_sub, stop_info)

        unique_pos <- !duplicated.array(gps_sub[,2:3])

        n <- dim(stop_info)[1]
        for(j in 1:n){
            unique_pos[stop_info$stop_start[j]] <- TRUE
            unique_pos[stop_info$stop_end[j]] <- TRUE
        }

        unique_gps <- gps_sub[unique_pos,]



        if (length(unique(unique_gps$x)) != 1){
            result <- with(unique_gps,
                           smooth.spline(
                               time, x,
                               df = ceiling(length(x) * 0.8)))
            pre_x <- with(unique_gps,
                          predict(result, gps_sub$time)$y)
        } else {
            pre_x <- rep(unique_gps$x[1], length(gps_sub$time))
        }

        if (length(unique(unique_gps$y)) != 1){
            result2 <- with(unique_gps,
                            smooth.spline(
                                time, y,
                                df = ceiling(length(y) * 0.8)))

            pre_y <- with(unique_gps,
                          predict(result2, gps_sub$time)$y)

        } else {
            pre_y <- rep(unique_gps$y[1], length(gps_sub$time))
        }

        if (i == k){
            gps_data$x[gps_data$time >= st_p] <- pre_x
            gps_data$y[gps_data$time >= st_p] <- pre_y
        } else {
            gps_data$x[gps_data$time >= st_p &
                           gps_data$time <= (st_p+30)] <- pre_x
            gps_data$y[gps_data$time >= st_p &
                           gps_data$time <= (st_p+30)] <- pre_y
        }

        # with(unique_gps, plot(x, y, asp = 1))
        # points(pre_x, pre_y, type = "l")
    }

    gps_data

}

spline_gps2 <- function(gps_data, acc_data){

    stop_info <- stop_detection2(acc_data,
                                 slope_check = 0.003)

    gps_data <- stop_correction(gps_data, stop_info)

    unique_pos <- !duplicated.array(gps_data[,2:3])

    n <- dim(stop_info)[1]
    for(j in 1:n){
        unique_pos[stop_info$stop_start[j]] <- TRUE
        unique_pos[stop_info$stop_end[j]] <- TRUE
    }

    unique_gps <- gps_data[unique_pos,]

    result <- with(unique_gps,
                   smooth.spline(
                       time, x,
                       df = ceiling(length(x) * 0.8)))
    pre_x <- with(unique_gps,
                  predict(result, gps_data$time)$y)

    result2 <- with(unique_gps,
                    smooth.spline(
                        time, y,
                        df = ceiling(length(y) * 0.8)))

    pre_y <- with(unique_gps,
                  predict(result2, gps_data$time)$y)


    gps_data$x <- pre_x
    gps_data$y <- pre_y

    gps_data

}

# library(tidyverse)
# library(ggmap)
#
# # api from google map
# api <- "AIzaSyCzLbAEmXBeFEdQuzHWCxb4Kvz6ErdZ2m4"
# register_google(key = api)
#
# trip_map1 <- get_tripmap(55, 75, gps_data)
# draw_map(ggmap(trip_map1), gps_data)
#
# get_tripmap <- function(start_t, end_t, data){
#     data %>%
#         filter(time > start_t & time < end_t) %>%
#         select(x, y) %>%
#         summarise(c_x = mean(x), c_y = mean(y)) -> center_vec
#     get_googlemap(center = c(center_vec$c_x, center_vec$c_y),
#                   maptype = "hybrid", zoom = 18, scale = 4)
# }
#
# draw_map <- function(obj, p_data){
#     obj +
#         geom_point(data = p_data, aes(x = x, y = y),
#                    size = 1, color = "red")+
#         theme(plot.margin= unit(c(0, 0, 0, 0), "lines"),
#               axis.title.x=element_blank(),
#               axis.text.x=element_blank(),
#               axis.ticks.x=element_blank(),
#               axis.title.y=element_blank(),
#               axis.text.y=element_blank(),
#               axis.ticks.y=element_blank())
# }

# library(ikhyd)
library(tidyverse)
# library(magrittr)
gps_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                     data_option = 1)

acc_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                     data_option = 2)

lacc_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                     data_option = 3)

gyro_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                      data_option = 4)
roll_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                      data_option = 5)

compass_data <- get_trip(system.file("extdata", "trip1.csv", package = "ikhyd"),
                      data_option = 7)


head(gps_data)

with(gps_data  %>% filter(time < 30) ,
     plot(time, z, type = "l"))

with(acc_data %>% filter(time < 30),
     plot(time, y, type = "l"))

with(gyro_data %>% filter(time < 300),
     plot(time, gyroX.rad.s., type = "l"))
with(roll_data %>% filter(time < 100),
     plot(time, Roll.rads., type = "l"))

gps_data[gps_data$accuracy_horiz > 5,] %>% select(time) %>% unlist() %>% hist
# head(gps_data)
#
# acc_data <- get_trip(system.file("extdata", "sample_trip.csv", package = "ikhyd"),
#                      data_option = 2)

#' Correction of GPS based on stop information
#'
#' @name stop_correction
#' @param gps_data the GPS data
#' @param stop_info the information of stop
#' @return A GPS data which does not have different GPS points when the vehicle is stop
#' @export
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
#' @param interval_sec interval check
#' @param var_check thresh hold value
#' @return A dataframe containing the position of stop starts and ends, and length of stop.
#' @export
stop_detection <- function(acc_data,
                           interval_sec = 3,
                           var_check = 0.004){

    acc_vec <- acc_data$y
    window_size = interval_sec * 25
    result <- rep(0, length(acc_vec))
    mean_value <- rep(0, length(acc_vec))
    for (i in 1:(length(acc_vec) - window_size)){
        check_range <- stats::var(acc_vec[i:(i + window_size)])
        if (check_range < var_check){
            result[i] <- 1
            mean_value[i] <- mean(acc_vec[i:(i + window_size)])
        }
    }

    for (i in (1 + window_size):length(acc_vec)){
        check_range <- stats::var(acc_vec[(i-window_size):i])
        if (check_range < var_check){
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

    # check_vec <- utils::tail(stop_start, -1) - utils::head(stop_end, -1)
    # check_vec <- c(check_vec, 100000)

    check_vec <- stop_end - stop_start
    result <- data.frame(stop_start = stop_start,
                         stop_end = stop_end)
    result <- result[check_vec > 75,]

    data.frame(stop_start = result[,1],
               stop_end   = result[,2],
               legnth_sec = (result[,2] - result[,1])/25 )
}

#' Detecting stops from accelerometer y-axis using regresion
#'
#' @name stop_detection2
#' @param acc_sub the accelerometer data
#' @param interval_sec interval check
#' @param slope_check thresh hold value
#' @return A dataframe containing the position of stop starts and ends, and length of stop.
#' @export
stop_detection2 <- function(acc_sub,
                           interval_sec = 3,
                           slope_check = 0.005){

    acc_vec <- acc_sub$y
    window_size = (interval_sec * 25) - 1
    result <- rep(0, length(acc_vec))
    mean_value <- rep(0, length(acc_vec))
    for (i in 1:(length(acc_vec) - window_size)){
        x <- 1:(interval_sec*25)
        y <- acc_vec[i:(i + window_size)]

        check_range <-  stats::cor(x, y) * (stats::sd(y) / stats::sd(x))
        if ((abs(check_range) < slope_check) & (abs(var(y)) < 0.03)){
            result[i] <- 1
            mean_value[i] <- mean(acc_vec[i:(i + window_size)])
        }
    }

    for (i in (1 + window_size):length(acc_vec)){
        x <- 1:(interval_sec*25)
        y <- acc_vec[(i-window_size):i]
        check_range <-  stats::cor(x, y) * (stats::sd(y) / stats::sd(x))
        if ((abs(check_range) < slope_check) & (abs(var(y)) < 0.03)){
            result[i] <- 1
            mean_value[i] <- mean(acc_vec[(i-window_size):i])
        }
    }

    dresult <- (utils::tail(result, -1) - utils::head(result, -1))


    if( sum(abs(dresult)) != 0){

        if (sum(dresult == 0) == (length(dresult) - 1) &
            sum(dresult == 1) == 1){
            # 1 to the end
            stop_start <- which(dresult == 1) + 1
            stop_end <- length(result)
        } else if (sum(dresult == 0) == (length(dresult) - 1) &
                   sum(dresult == -1) == 1){
            # 1 from start
            stop_start <- 1
            stop_end   <- which(dresult == -1)
        } else {
            stop_start <- which(dresult == 1) + 1
            stop_end   <- which(dresult == -1)

            if (stop_start[1] > stop_end[1]) { stop_start <- c(1, stop_start) }
            if (utils::tail(stop_start, 1) > utils::tail(stop_end, 1)) {
                stop_end <- c(stop_end, length(acc_vec))
            }
        }

        check_vec <- stop_end - stop_start
        myresult <- data.frame(stop_start = stop_start,
                             stop_end = stop_end)

    } else if (sum(result) == length(result)){
        myresult <- data.frame(stop_start = 1,
                             stop_end = length(result))
    } else {
        myresult <- data.frame(stop_start = 1,
                             stop_end = 1)
    }

    data.frame(stop_start = myresult[,1],
               stop_end   = myresult[,2],
               legnth_sec = (myresult[,2] - myresult[,1])/25 )
}

# with(acc_data, plot(time, y, type="l", ylim=c(-6,6)))
# stop_info <- stop_detection(acc_data)
# acc_data <- stop_correction_acc(acc_data, stop_info)
# with(acc_data, plot(time, y, type="l",ylim=c(-6,6)))

