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
#' @return calibrated y-axis accelerometer data
#' @export
kalmanfilter_ay <- function(acc_data, speed_data){

    n <- length(acc_data$y)
    dt <- with(acc_data, utils::tail(time, -1) - utils::head(time, -1))
    estimated_states <- matrix(0, nrow = n, ncol = 2)

    estimated_states[1, ] <- 0
    est_data <- 0
    P = 1; Q = 0.0001; R = 1; h = 2.23694;
    i <- 2

    for (i in 2:n){
        A <- 1
        B <- dt[i-1]
        u <- as.numeric(acc_data$y[i-1])
        z <- as.numeric(speed_data$speed[i])

        result <- kalmanfilter_1D(est_data, u, A, B, z, h,
                                  P, Q, R)

        est_data <- as.numeric(result$xhat)
        P <- result$Phat
        estimated_states[i, 1] <- est_data
        estimated_states[i, 2] <- (est_data - estimated_states[i-1, 1])/dt[i-1]
    }
    as.numeric(estimated_states[, 2])
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
#' @param gps_data GPS data which has the same number of points with accelerometer data
#' @param acc_data accelerometer data containing time, x, y, z
#' @param heading_data heading data based on magnetometer which has time and trueheading data.
#' @return calibrated GPS data
#' @export
kalmanfilter_gps <- function(gps_data, acc_data, heading_data){

    acc_data_2d_orig <- acc_data %>%
        dplyr::mutate(x = x * 0.00001,
                      y = y * 0.00001)
    gps_data <- gps_data %>% dplyr::select(x, y)
    dt <- utils::tail(acc_data_2d_orig$time, -1) - utils::head(acc_data_2d_orig$time, -1)
    acc_data_NE <- get_NE(acc_data_2d_orig, heading_data)

    N <- dim(gps_data)[1]
    estimated_states <- matrix(0, nrow = N, ncol = 4)
    estimated_states[1,] <- as.numeric(c(gps_data[1,], 0, 0))

    est_data <- as.numeric(c(gps_data[1,], 0, 0))


    H <- diag(4)
    P <- diag(4)
    R <- diag(4)
    Q <- diag(4) * 0.0008

    i <- 2
    for (i in 2:N){
        A <- matrix(c(1, 0, dt[i-1], 0,
                      0, 1, 0, dt[i-1],
                      0, 0, 1, 0,
                      0, 0, 0, 1), ncol = 4, byrow = TRUE)
        B <- matrix(c(0.5 * dt[i-1]^2, 0, dt[i-1], 0,
                      0, 0.5 * dt[i-1]^2, 0, dt[i-1]), ncol = 2)
        u <- as.numeric(acc_data_NE[i-1,])
        z <- as.numeric(c(gps_data[i,], (gps_data[i,] - gps_data[i-1,]) / dt[i-1]))

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

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("time", "x", "y", "gyroZ.rad.s.", "TrueHeading",
                             "angle_x", "angle_y", "a_est", "a_nor"))
}
