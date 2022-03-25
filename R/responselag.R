#' Visualize response lag
#'
#' The \code{responselag} function creates plots to visualize the amount of time between hunt date and response date in daily Harvest Survey data.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate as_date
#' @importFrom lubridate ymd
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_jitter
#' @importFrom viridis scale_color_viridis
#' @importFrom ggplot2 geom_abline 
#' 
#' @param data Daily data table
#' @param type Default is "count"
#'  \itemize{
#'  \item "count" - returns the distribution of response lag
#'  \item "lag" - returns the relationship between time lag and number of birds retrieved
#'  \item "date" - returns the relationship between response date and harvest date
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
responselag <- 
  function(data, type = "count"){
    if(type == "count"){
      data %>% 
        select(harvested_date, created_on, retrieved, unretrieved) %>% 
        mutate(
          cr = as_date(ymd_hms(created_on)),
          hd = ymd(harvested_date),
          time_lag = cr - hd) %>% 
        group_by(time_lag) %>% 
        summarize(n = n()) %>% 
        ungroup() %>% 
        ggplot(aes(x = as.numeric(time_lag), y = n)) + 
        geom_bar(stat = "identity") + 
        labs(x = "Time lag (days)", y = "Responses (count)") +
        theme_classic()
    }else if(type == "lag"){
      data %>% 
        select(harvested_date, created_on, retrieved, unretrieved) %>% 
        mutate(
          cr = as_date(ymd_hms(created_on)),
          hd = ymd(harvested_date),
          time_lag = cr - hd) %>% 
        select(time_lag, retrieved) %>% 
        ggplot(aes(x = as.numeric(time_lag), y = retrieved)) +
        geom_jitter(aes(color = retrieved, size = retrieved), alpha = 0.5) +
        labs(x = "Time lag (days)", y = "Retrieved (number of birds)",
             col = "Retrieved", size = "Retrieved") +
        scale_color_viridis(direction = -1) + 
        theme_classic()
    }else if(type == "date"){
      data %>% 
        select(harvested_date, created_on, retrieved, unretrieved) %>% 
        mutate(
          cr = as_date(ymd_hms(created_on)),
          hd = ymd(harvested_date),
          time_lag = cr - hd) %>% 
        ggplot(aes(x = cr, y = hd, color = retrieved, size = retrieved)) +
        geom_jitter(alpha = 0.5) +
        geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
        geom_abline(aes(intercept = -30, slope = 1), linetype = 2) +
        geom_abline(aes(intercept = -60, slope = 1), linetype = 2) +
        geom_abline(aes(intercept = -90, slope = 1), linetype = 2) +
        geom_abline(aes(intercept = -120, slope = 1), linetype = 2) +
        labs(y = "Harvest date", x = "Response date", size = "Retrieved",
             color = "Retrieved") +
        scale_color_viridis(direction = -1) + 
        theme_classic()
    }else{
      message("Error: `type` must be 'count', 'lag', or 'date'.")
    }
  }