#' get trip map from google map
#'
#' @name get_tripmap
#' @importFrom magrittr "%>%"
#' @param start_t starting time
#' @param end_t end time
#' @param gps_data GPS data
#' @return ggmap object
#' @export
get_tripmap <- function(start_t, end_t, gps_data){
    
    # api from google map
    api <- "AIzaSyCzLbAEmXBeFEdQuzHWCxb4Kvz6ErdZ2m4"
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
#' @param zoom_scale zoom scale
#' @return ggmap object
#' @export
get_tripmap2 <- function(time_insec, gps_data,
                         zoom_scale = 20){
    
    # api from google map
    api <- "AIzaSyCzLbAEmXBeFEdQuzHWCxb4Kvz6ErdZ2m4"
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
#' @param end_t end time
#' @param gps_data GPS data
#' @return ggmap object
#' @export
draw_map <- function(map_obj, gps_data){
    mapdraw <- map_obj + 
        ggplot2::geom_point(data = gps_data, aes(x = x, y = y),
                            size = 1.5, color = "red") +
        ggplot2::theme(plot.margin= unit(c(0, 0, 0, 0), "lines"),
                       axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank())
    
    if( length(attributes(map_obj)$time_pos) == 1 ){
        mapdraw <- mapdraw +
            ggplot2::geom_point(data = gps_data[attributes(map_obj)$time_pos,],
                                aes(x = x, y = y),
                                size = 1.5, color = "yellow")
    }
    mapdraw
}
