#' 1-D Kalmanfilter
#'
#' @name kalmanfilter_1D
#' @param x observation from sensor 1
#' @param z observation from sensor 2
#' @param P variance of observation from sensor 1 at the previous time step
#' @param u control vector
#' @param a scale constant related to x
#' @param b scale constant related to u
#' @param h scale constant between sensor 1 and 2
#' @param Q noise of the observation from sensor 1
#' @param R noise of the observation from sensor 2
#' @return the list of two value; optimal estimate for x and P
kalmanfilter_1D <- function(x, u, a, b, z, h,
                            P, Q, R){
    ht <-  h
    at <-  a
    xhat <- a * x + b * u
    Phat <- a * P * at + Q
    K    <- Phat * ht * (h * Phat * ht + R)^-1
    xnew <- xhat + K * (z - h * xhat)
    Pnew <- (1 - K * h) * Phat
    return( list(xhat = xnew, Phat = Pnew) )
}

#' Kalmanfilter general
#'
#' @name kalmanfilter
#' @importFrom magrittr "%>%"
#' @param x observation from sensor 1
#' @param z observation from sensor 2
#' @param P variance of observation from sensor 1 at the previous time step
#' @param u control vector
#' @param A scale constant related to x
#' @param B scale constant related to u
#' @param H scale constant between sensor 1 and 2
#' @param Q noise of the observation from sensor 1
#' @param R noise of the observation from sensor 2
#' @return the list of two value; optimal estimate for x and P
kalmanfilter <- function(x, P, z, u,
                         A, B, H, Q, R){
    Ht <-  t(H)
    At <-  t(A)
    xhat <- A %*% x + B %*% u
    Phat <- A %*% P %*% At + Q
    K    <- Phat %*% Ht %*% solve(H %*% Phat %*% Ht + R)
    xnew <- xhat + K %*% (z - H %*% xhat)
    Pnew <- (diag(dim(K)[1]) - K %*% H) %*% Phat
    return( list(xhat = xnew, Phat = Pnew) )
}

#' Calibarate accelerometer y-axis using speed data
#'
#' @name kalmanfilter_ay
#' @param acc_data accelerometer data containing time, x, y, z
#' @param speed_data speed data containing time and speed.
#' @param gps_data gps data for weak signal
#' @return calibrated y-axis accelerometer data
#' @export
kalmanfilter_ay <- function(acc_data,
                            speed_data,
                            gps_data = NULL){

    n <- length(acc_data$time)

    if (!is.null(gps_data)){
        weak_info <- rep(FALSE, n)
        weak_info[gps_data$accuracy_horiz > 5 |
                      gps_data$accuracy_vert > 5] <- TRUE
    } else {
        weak_info <- rep(FALSE, n)
    }


    # for (i in which(weak_info == TRUE)){
    #     weak_info[max(0, (i - 200)):min((i + 200), n)] <- TRUE
    # }
    #
    # points(acc_data$time[weak_info],
    #        rep(0, length(acc_data$time[weak_info])),
    #        col= "red")

    stop_info <- stop_detection(acc_data,
                                interval_sec = 1,
                                var_check = 0.001)

    speed_data <- smoothing_speed(speed_data, stop_info)

    stop_mom <- 0
    n <- dim(stop_info)[1]
    for(i in 1:n){
        stop_mom <- c(stop_mom,
                      stop_info$stop_start[i]:stop_info$stop_end[i])
    }

    acc_data$y[stop_mom] <- 0

    n <- length(acc_data$y)
    dt <- with(acc_data, utils::tail(time, -1) - utils::head(time, -1))
    estimated_states <- matrix(0, nrow = n, ncol = 2)

    estimated_states[1, ] <- 0
    est_data <- 0
    # P = 1; h = 2.23694;
    P = 1; h = 1;

    # flat road
    # P = 1; Q = 0.0001; R = 1; h = 1;

    # hills
    # P = 1; Q = 1; R = 1; h = 1;
    # P = 1; Q = 100; R = 0.001; h = 2.23694;
    # plot(estimated_states[2800:2850,1] * 2.23694)
    # i <- 2823

    previous_speed <- 0
    acc_data$y[1] <- 0

    i <- 2
    for (i in 2:n){
        A <- 1
        B <- dt[i-1]
        u <- as.numeric(acc_data$y[i-1])
        z <- as.numeric(speed_data$speed[i] * 0.44704)

        if( weak_info[i] == TRUE){
            Q = 0.0001; R = 1;
        } else {
            Q = 0.0001; R = 1;
        }

        result <- kalmanfilter_1D(est_data, u, A, B, z, h,
                                  P, Q, R)

        est_data <- as.numeric(result$xhat)
        acc_y <- (est_data - previous_speed)/dt[i-1]
        previous_speed <- est_data

        P <- result$Phat
        estimated_states[i, 1] <- est_data
        estimated_states[i-1, 2] <- acc_y
    }

    as.numeric(estimated_states[, 2])
}

#' Correct accelerometer values for stop
#'
#' @name stop_correction_acc
#' @param acc_data accelerometer data containing time, x, y, z
#' @param stop_info information of stop
#' @return An accelerometer data whose values are zero at stop moments
#' @export
stop_correction_acc <- function(acc_data, stop_info){

    n <- dim(stop_info)[1]
    i <- 1
    for(i in 1:n){
        acc_data$y[stop_info$stop_start[i]:stop_info$stop_end[i]] <- 0
        acc_data$x[stop_info$stop_start[i]:stop_info$stop_end[i]] <- 0
    }
    acc_data
}


#' Calibarate accelerometer x-axis using Gyroscope data
#'
#' @name kalmanfilter_ax
#' @param acc_data accelerometer data containing time, x, y, z
#' @param smth_speed_data speed data which has same number of points with accelerometer data.
#' @param gyro_data Gyroscope data containing time, x, y, z
#' @return calibrated x-axis accelerometer data
#' @export
kalmanfilter_ax <- function(acc_data, gyro_data, smth_speed_data){

    n <- length(acc_data$x)
    dt <- with(acc_data, utils::tail(time, -1) - utils::head(time, -1))
    lat_acc <-  -0.44704 * smth_speed_data * gyro_data$gyroZ.rad.s.
    estimated_states <- rep(0, n)

    estimated_states[1] <- 0
    est_data <- 0

    h <- 1; P <- 1; R <- 0.5; Q <- 1
    A <- 1; B <- 1
    i <- 2;
    for (i in 2:n){
        u <- as.numeric(acc_data$x[i] - estimated_states[i-1])
        z <- as.numeric(lat_acc[i])

        result <- kalmanfilter_1D(est_data, u, A, B, z,
                                  h, P, Q, R)

        est_data <- as.numeric(result$xhat)
        P <- result$Phat
        estimated_states[i] <- est_data
    }

    as.numeric(estimated_states)

}

#' Convert accelerometer value into lat., long. acceleration
#'
#' @name get_NE
#' @importFrom magrittr "%>%"
#' @param acc_data accelerometer data which has time, x and y
#' @param heading_data heading data based on magnetometer which has time and trueheading
#' @return a dataframe which has latudinal and longitudinal acceleration data.
get_NE <- function(acc_data, heading_data){
    cbind.data.frame(heading_data %>% dplyr::select(TrueHeading), acc_data) %>%
        dplyr::select(TrueHeading, x, y) %>%
        dplyr::mutate(angle_y = dplyr::if_else(y > 0, TrueHeading, (TrueHeading + 180) %% 360)) %>%
        dplyr::mutate(angle_x = dplyr::if_else(x > 0, (TrueHeading + 90) %% 360, (TrueHeading + 270) %% 360)) %>%
        dplyr::mutate(angle_x = angle_x * (2 * pi / 180)) %>%
        dplyr::mutate(angle_y = angle_y * (2 * pi / 180)) %>%
        dplyr::mutate(a_est = abs(y) * sin(angle_y) + abs(x) * sin(angle_x)) %>%
        dplyr::mutate(a_nor = abs(y) * cos(angle_y) + abs(x) * cos(angle_x)) %>%
        dplyr::select(a_est, a_nor)
}


#' Calibarate GPS data using accelerometer data
#'
#' @name kalmanfilter_gps
#' @importFrom magrittr "%>%"
#' @param gps_data GPS data which has the same number of points with accelerometer data
#' @param acc_data accelerometer data containing time, x, y, z
#' @param heading_data heading data based on magnetometer which has time and trueheading data.
#' @return calibrated GPS data
#' @export
kalmanfilter_gps <- function(gps_data, acc_data, heading_data){

    # library(ikhyd)
    # heading_data <- get_trip(system.file("extdata", "trip2.csv", package = "ikhyd"),
    #                      data_option = 6)

    acc_data_2d_orig <- acc_data %>%
        dplyr::mutate(x = x * 0.00001,
                      y = y * 0.00001)

    dt <- utils::tail(gps_data$time, -1) - utils::head(gps_data$time, -1)
    acc_data_NE <- get_NE(acc_data_2d_orig, heading_data)

    v_e <- speed_from_acc(acc_data$time, acc_data_NE$a_est)
    v_n <- speed_from_acc(acc_data$time, acc_data_NE$a_nor)

    gps_data <- gps_data %>%
        dplyr::select(x, y)

    N <- dim(gps_data)[1]
    estimated_states <- matrix(0, nrow = N, ncol = 4)
    estimated_states[1,] <- as.numeric(c(gps_data[1,], 0, 0))

    est_data <- as.numeric(c(gps_data[1,], 0, 0))


    H <- diag(4); P <- diag(4)
    # R <- diag(4); Q <- diag(4)
    R <- diag(4) * 0.0008; Q <- diag(4)

    i <- 2
    for (i in 2:N){
        A <- matrix(c(1, 0, dt[i-1], 0,
                      0, 1, 0, dt[i-1],
                      0, 0, 1, 0,
                      0, 0, 0, 1), ncol = 4, byrow = TRUE)
        B <- matrix(c(0.5 * dt[i-1]^2, 0, dt[i-1], 0,
                      0, 0.5 * dt[i-1]^2, 0, dt[i-1]), ncol = 2)
        u <- as.numeric(acc_data_NE[i-1,])
        # z <- as.numeric(c(gps_data[i,], (gps_data[i,] - gps_data[i-1,]) / dt[i-1]))
        z <- as.numeric(c(gps_data[i,], v_e[i], v_n[i]))

        result <- kalmanfilter(est_data, P,
                               z, u, A, B, H, Q, R)

        est_data <- as.numeric(result$xhat)
        P <- result$Phat
        estimated_states[i,] <- est_data
    }

    data.frame(time = acc_data$time,
               x = estimated_states[,1],
               y = estimated_states[,2])

}


#' Low pass filter
#'
#' @name low_pass
#' @param input_vec input vector that needs to be filtered
#' @param alpha constant parameter for low pass rate
#' @return filtered data
#' @export
low_pass <- function(input_vec, alpha){
    n <- length(input_vec)
    output_vec <- rep(0, n)
    for (i in 1:n){
        if (i == 1){
            output_vec[i] = input_vec[i]
        } else {
            output_vec[i] = (1 - alpha) * output_vec[i-1] + alpha * input_vec[i]
        }
    }
    output_vec
}

#' kalman filter with road grade for lon. accelerometer
#'
#' @name kalmanfilter_withalpha
#' @param acc_data acc data
#' @param speed_data speed data from gps
#' @param angle_data angle data from gyro
#' @param alt_data altitude data from baro
#' @return filtered acc data with road grade
#' @export
kalmanfilter_withalpha <- function(acc_data,
                                   speed_data,
                                   angle_data,
                                   alt_data){

    alpha <- angle_data$Pitch.rads.
    dt <- utils::tail(acc_data$time, -1) -
        utils::head(acc_data$time, -1)

    n <- dim(acc_data)[1]
    estimated_states <- matrix(0, nrow = n, ncol = 2)

    estimated_states[1,] <- as.numeric(c(0, alpha[1]))

    past_speed <- 0
    alpha_est <- alpha[1]
    est_data <- as.numeric(estimated_states[1,])
    acc_y <- rep(0, n)

    stop_info <- stop_detection(acc_data, interval_sec = 1)
    stop_vec <- get_stopvec(stop_info)

    H <- diag(2); P <- diag(2)
    R <- matrix(c(1, 0,
                  0, 1), nrow = 2, byrow = T)
    Q <- matrix(c(0.001, 0,
                  0, 0.1), nrow = 2, byrow = T)

    # Q <- matrix(c(0.001, 0,
    #               0, 1), nrow = 2, byrow = T)

    i <- 2

    for (i in 2:n){
        A <- matrix(c(1, -9.81865 * dt[i-1],
                      0, 1), ncol = 2, byrow = TRUE)
        B <- matrix(c(dt[i-1], 0), ncol = 1)
        u <- as.numeric(acc_data$y[i])

        if (abs(alpha[i] - est_data[2]) > 0.005){
            alpha_est <- est_data[2]
        } else {
            alpha_est <- alpha[i]
        }

        if (i %in% stop_vec){
            z <- as.numeric(c(0, alpha[i]))
            R <- diag(2) * 0.001
        } else {
            z <- as.numeric(c(speed_data$speed[i] * 0.44704, alpha_est))
            R <- matrix(c(1, 0,
                          0, 0.1), nrow = 2, byrow = T)
        }

        result <- kalmanfilter(est_data, P,
                               z, u, A, B, H, Q, R)

        acc_y[i] <- (result$xhat[1] - past_speed) / dt[i]
        past_speed <- result$xhat[1]

        alpha_est <- (acc_data$y[i] - acc_y[i]) / 9.81865
        alpha_est <- est_data[2] * 0.98 + 0.01 * result$xhat[2] +
            alpha_est * 0.01

        result$xhat[2] <- alpha_est

        est_data <- as.numeric(result$xhat)

        P <- result$Phat
        estimated_states[i,] <- est_data
    }

    data.frame(time = acc_data$time,
               speed = estimated_states[,1] * 2.23694,
               alpha = estimated_states[,2],
               acc_y = acc_y)
}

#' kalman filter with road grade for lon. accelerometer
#'
#' @name kalmanfilter_accfirst
#' @param acc_data acc data
#' @param speed_data speed data from gps
#' @param angle_data angle data from gyro
#' @param alt_data altitude data from baro
#' @return filtered acc data with road grade
#' @export
kalmanfilter_accfirst <- function(acc_data,
                               speed_data,
                               angle_data,
                               alt_data,
                               gps_data = NULL){

    alpha <- angle_data$Pitch.rads.
    dt <- utils::tail(acc_data$time, -1) -
        utils::head(acc_data$time, -1)

    n <- length(acc_data$time)

    if (!is.null(gps_data)){
        weak_info <- rep(FALSE, n)
        weak_info[gps_data$accuracy_horiz > 5 |
                      gps_data$accuracy_vert > 5] <- TRUE
    } else {
        weak_info <- rep(FALSE, n)
    }

    estimated_states <- matrix(0, nrow = n, ncol = 3)

    estimated_states[1,] <- as.numeric(c(0, alpha[1], 0))

    past_speed <- 0
    alpha_est <- alpha[1]
    est_data <- as.numeric(c(0, alpha[1]))
    acc_y <- rep(0, n)

    stop_info <- stop_detection(acc_data, interval_sec = 1)
    stop_vec <- get_stopvec(stop_info)

    H <- diag(3); P <- diag(2)

    i <- 2

    for (i in 2:n){
        if (weak_info[i]){
            R <- matrix(c(10, 0, 0,
                          0, 10, 0,
                          0, 0, 10), nrow = 3, byrow = T)

            Q <- matrix(c(0.001, 0, 0,
                          0, 0.001, 0,
                          0, 0, 0.001), nrow = 3, byrow = T)
        } else {
            R <- matrix(c(1, 0, 0,
                          0, 1, 0,
                          0, 0, 1), nrow = 3, byrow = T)

            Q <- matrix(c(0.01, 0, 0,
                          0, 0.01, 0,
                          0, 0, 0.01), nrow = 3, byrow = T)

        }

        A <- matrix(c(1, -9.81865 * dt[i-1],
                      0, 1,
                      0, -9.81865), ncol = 2, byrow = TRUE)
        B <- matrix(c(dt[i-1], 0, 1), ncol = 1)
        u <- as.numeric(acc_data$y[i])

        if (abs(alpha[i] - est_data[2]) > 0.005){
            alpha_est <- est_data[2]
        } else {
            alpha_est <- alpha[i]
        }

        if (i %in% stop_vec){
            z <- as.numeric(c(0, alpha[i], 0))
            R <- diag(3) * 0.001
        } else {
            z <- as.numeric(c(speed_data$speed[i] * 0.44704, alpha_est, speed_data$a_y[i]))
        }

        result <- kalmanfilter(est_data, P,
                               z, u, A, B, H, Q, R)

        acc_y[i] <- result$xhat[3] * 0.2 + acc_y[i-1] * 0.8
        past_speed <- result$xhat[1]

        alpha_est <- (acc_data$y[i] - acc_y[i]) / 9.81865
        alpha_est <- est_data[2] * 0.98 + 0.01 * result$xhat[2] +
            alpha_est * 0.01

        result$xhat[2] <- alpha_est

        est_data <- as.numeric(result$xhat[1:2])

        P <- result$Phat[1:2, 1:2]
        estimated_states[i,] <- result$xhat
    }

    data.frame(time = acc_data$time,
               speed = estimated_states[,1] * 2.23694,
               alpha = estimated_states[,2],
               acc_y = estimated_states[,3])
}


if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("time", "x", "y",
                             "speed", "TrueHeading", "gyroZ.rad.s.",
                             "angle_x", "angle_y", "a_est", "a_nor"))
}
