#' Trip visualization of telematics data
#'
#' @name track_vis
#' @param gps_data gps data dataframe from load_telematics_data 
#' @param timepoint specific time point in the trip
#' @param animate make TRUE for obtaining trip animation
#' @param api google map api
#' @param zoom zooming parameter
#' @return ggmap object
#' @export
track_vis <- function(gps_data, timepoint = NA, 
                      animate=FALSE,
                      api, zoom = 1){
    
    n <- dim(gps_data)[1]
    if (!is.na(timepoint)){
        if (timepoint > n){
            print(paste("timepoint > the length of the vector", n))
            return(NA)
        } else {
            index <- max(timepoint-400, 0):min(timepoint+800, n)
            gps_data <- gps_data[index,]
        }
    }
    
    # highway data
    bgdata <- data.frame(x = gps_data$x,
                         y = gps_data$y)
    
    # api from google map
    ggmap::register_google(key = api)
    
    trip_range <- ggmap::make_bbox(lon = bgdata$x, lat = bgdata$y, f = zoom)
    
    mapdata <- ggmap::get_map(location = trip_range,
                       maptype = "satellite",
                       source = "google")
    
    # make ggmap plot
    p <- ggmap::ggmap(mapdata) + 
        ggplot2::geom_path(data = gps_data,
                           ggplot2::aes(x, y), col = "black") +
        ggplot2::geom_point(data = gps_data,
                            ggplot2::aes(x, y), col = "red", size = 1) + 
        ggplot2::guides(fill=FALSE) +
        ggplot2::theme(axis.text    = ggplot2::element_blank(),
                       axis.title   = ggplot2::element_text(size = 15, face="bold"),
                       plot.margin  = ggplot2::margin(0.2, 0.2, 0.2, 0.2, "cm"),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank())
    
    if (!is.na(timepoint)){
        p <- p +
            ggplot2::geom_point(data = gps_data[which(index ==timepoint),],
                                ggplot2::aes(x, y), col = "yellow", size = 1)
    }
    
    if (animate){
        p <- p +
            gganimate::transition_reveal(time) +
            gganimate::ease_aes('linear')
    }
    p
}

#' get trip map from google map
#'
#' @name get_tripmap
#' @importFrom magrittr "%>%"
#' @param start_t starting time
#' @param end_t end time
#' @param api google map api
#' @param gps_data GPS data
#' @return ggmap object
#' @export
get_tripmap <- function(start_t, end_t, gps_data, api){

    # api from google map
    ggmap::register_google(key = api)

    gps_data %>%
        dplyr::filter(time > start_t & time < end_t) %>%
        dplyr::select(x, y) %>%
        dplyr::summarise(c_x = mean(x),
                         c_y = mean(y)) -> center_vec
    ggmap::get_googlemap(center = c(center_vec$c_x, center_vec$c_y),
                         maptype = "hybrid", zoom = 18, scale = 2)
}

#' get trip map without google account
#'
#' @name get_tripmap2
#' @importFrom magrittr "%>%"
#' @param time_insec time
#' @param gps_data GPS data
#' @param api google map api
#' @param zoom_scale zoom scale
#' @return ggmap object
#' @export
get_tripmap2 <- function(time_insec, gps_data, api,
                         zoom_scale = 20){

    # api from google map
    ggmap::register_google(key = api)

    time_pos <- which.min(abs(gps_data$time - time_insec))

    gps_data[time_pos, ] -> sub_data

    map_obj <- ggmap::get_googlemap(center = c(sub_data$x, sub_data$y),
                         maptype = "hybrid",
                         zoom = zoom_scale, scale = 1)
    map_obj <- ggmap::ggmap(map_obj)
    attributes(map_obj)$time_pos <- time_pos
    map_obj
}

#' draw GPS points
#'
#' @name draw_map
#' @param map_obj the object from google map
#' @param gps_data GPS data
#' @return ggmap object
#' @export
draw_map <- function(map_obj, gps_data){
    mapdraw <- map_obj +
        ggplot2::geom_point(data = gps_data, ggplot2::aes(x = x, y = y),
                            size = 1.5, color = "red") +
        ggplot2::theme(plot.margin= ggplot2::unit(c(0, 0, 0, 0), "lines"),
                       axis.title.x=ggplot2::element_blank(),
                       axis.text.x =ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),
                       axis.text.y =ggplot2::element_blank(),
                       axis.ticks.y=ggplot2::element_blank())

    if( length(attributes(map_obj)$time_pos) == 1 ){
        mapdraw <- mapdraw +
            ggplot2::geom_point(data = gps_data[attributes(map_obj)$time_pos,],
                                ggplot2::aes(x = x, y = y),
                                size = 1.5, color = "yellow")
    }
    mapdraw
}

#' Draw a heatmap for telematics data
#'
#' @name drawHeatmap
#' @param telematics_data telematics data with time, a_lon, a_lat, and speed coloums
#' @param drawRange vector for drawing range of heat map
#' @param speedbucket vector of speed bucket for drawing range of heat map
#' @return telematics lon-lat density w.r.t speed bucket drawn by ggplot
#' @export
drawHeatmap <- function(telematics_data,
                        drawRange= c(4, 4),
                        speedbucket = NULL){

    # Color housekeeping
    rf <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- rf(32)

    # heatmap_data
    # telematics_data$speed <- with(telematics_data, speed_from_acc(time, a_lon))

    if (!is.null(speedbucket)){
        telematics_data <- dplyr::filter(telematics_data,
                                         (speed >= speedbucket[1]) &
                                             (speed <= speedbucket[2]) )
    }

    heatmap_data <- dplyr::filter(telematics_data,
                           (abs(a_lon) < drawRange[1]) &
                               (abs(a_lat) < drawRange[2]) )

    p <- ggplot2::ggplot(data = heatmap_data,
                         ggplot2::aes(x = a_lat, y = a_lon)) +
        ggplot2::xlim(-drawRange[1], drawRange[1]) +
        ggplot2::ylim(-drawRange[2], drawRange[2]) +
        # Add estimated density
        ggplot2::geom_point(fill = "lightgray",
                   size = 0.5,
                   alpha = 0.1)

    p <- p + ggplot2::stat_bin2d(bins=200) +
        ggplot2::coord_equal() +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_gradientn(colours = r, trans = "log") +
        ggplot2::labs(y = "Longitudinal acceleration (m / s^2)",
             x = "Lateral acceleration (m / s^2)")
    p
}

#' Draw a v-a heatmap for telematics data
#'
#' @name draw_vaHeatmap
#' @param telematics_data telematics data with a_lon, a_lat and speed coloums
#' @param drawRange vector for drawing speed range of heat map
#' @param alim vector for drawing acceleration range of heat map
#' @return telematics heatmap drawn by ggplot
#' @export
draw_vaHeatmap <- function(telematics_data,
                        drawRange = c(0, 80), alim = 4){

    # Color housekeeping
    rf <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- rf(32)

    # heatmap_data
    # telematics_data$speed <- with(telematics_data, speed_from_acc(time, a_lon))

    heatmap_data <- dplyr::filter(telematics_data,
                                  (speed > drawRange[1]) &
                                      (speed <= drawRange[2]) )

    p <- ggplot2::ggplot(data = heatmap_data,
                         ggplot2::aes(x = speed, y = a_lon)) +
        ggplot2::xlim(drawRange[1], drawRange[2]) +
        ggplot2::ylim(-alim, alim) +
        # Add estimated density
        ggplot2::geom_point(fill = "lightgray",
                            size = 0.5,
                            alpha = 0.1)

    p <- p + ggplot2::stat_bin2d(bins=100) +
        ggplot2::theme_light() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_gradientn(colours = r, trans = "log") +
        ggplot2::labs(y = "Longitudinal acceleration (m / s^2)",
                      x = "Speed (mph)")
    p
}


#' Preparing a telematics data from GPS data for heatmap drawing
#'
#' @name telematics_data_fromGPS
#' @param gps_data GPS data
#' @param speed_data Speed data
#' @return telematics data object for drawing heatmap
#' @export
telematics_data_fromGPS <- function(gps_data, speed_data){

    n <- floor(max(gps_data$time))
    index_pickup <- floor(seq(1, length(gps_data$time),
                              length.out = n+1))

    gps_data_temp <- gps_data[index_pickup,]
    speed_data_temp <- speed_data[index_pickup,]

    my_data <- cbind.data.frame(gps_data_temp,
                                speed_data_temp[,c("speed", "TrueHeading")])

    dt <- utils::tail(my_data$time, -1) - utils::head(my_data$time, -1)
    my_data$accel <- c(0, (utils::tail(my_data$speed, -1) -
                               utils::head(my_data$speed, -1)) / dt)

    # latacc
    dangle <- utils::tail(my_data$TrueHeading, -1) -
        utils::head(my_data$TrueHeading, -1)

    dangle[dangle > 180] <- dangle[dangle > 180] - 360
    dangle[dangle < -180] <- dangle[dangle < -180] + 360
    omega <- dangle * pi / 180 / dt # radian
    my_data$latacc <- c(0, my_data$speed[-1] * 0.44704 * omega)

    data.frame(time = my_data$time,
               a_lon = my_data$accel,
               a_lat = my_data$latacc,
               omega = c(0, omega),
               speed = my_data$speed)
}


if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("a_lon", "a_lat"))
}
