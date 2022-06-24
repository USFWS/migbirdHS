#' Compare daily data and season Harvest Survey data
#'
#' The \code{compare} function takes 2 data tables (daily_data and season_data) to plot 3 data fields (bag size, number of birds retrieved, number of days hunted) and breaks those data into 4 groups: season submitted, daily submitted, season non-submit, and daily non-submit.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom MetBrewer met.brewer
#' @importFrom ggplot2 stat_summary 
#' @importFrom ggplot2 coord_cartesian
#' @importFrom viridis scale_color_viridis
#' @importFrom ggplot2 scale_x_discrete
#' 
#' @param daily_data Daily data table
#' @param season_data Season data table
#' @param type Default is "line"
#'  \itemize{
#'  \item "line" - returns a scatter plot with lines to visualize the relationship between number of birds retrieved and number of days spent hunting when data are divided into 4 submission groups
#'  \item "days" - returns the distribution of number of days hunted per submission group; bubbles represent number of birds retrieved and red lines indicate mean values
#'  \item "retrieved" - returns the distribution of number of birds retrieved per submission group; bubbles represent number of birds retrieved and red lines indicate mean values
#'  }
#' @param ylim Optional value to be supplied if an outlier stretches the y-axis; defaults to NA
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
compare <-
  function(daily_data, season_data, type = "line", ylim = NA){
    versus_data <-
      daily_data %>% 
      select(
        selected_hunterID, sampled_state, has_submitted, sp_group_estimated,
        harvested_date, retrieved) %>% 
      group_by(
        selected_hunterID, sampled_state, sp_group_estimated, has_submitted) %>% 
      summarize(
        sum_retrieved = sum(retrieved),
        days_hunted = n()) %>% 
      ungroup() %>% 
      rename(retrieved = sum_retrieved) %>% 
      mutate(
        type = ifelse(has_submitted == "Y", "daily_submit", "daily_nonsub"),
        retrieved = as.numeric(retrieved),
        days_hunted = as.numeric(days_hunted)) %>%  
      bind_rows(
        season_data %>% 
          filter(
            !selected_hunterID %in%
              c(daily_data %>% select(selected_hunterID) %>% pull())) %>%
          select(
            selected_hunterID, sampled_state, sp_group_estimated, has_submitted, 
            retrieved, days_hunted) %>% 
          mutate(
            type = 
              ifelse(has_submitted == "Y", "season_submit", "season_nonsub"),
            retrieved = as.numeric(retrieved),
            days_hunted = as.numeric(days_hunted))) 
    if(type == "line"){
      versus_data %>% 
        ggplot(aes(x = days_hunted, y = retrieved, color = type, fill = type)) +
        geom_jitter(aes(shape = type), alpha = 0.5, size = 3) +
        stat_smooth(
          aes(linetype = type), method = "lm", size = 1, alpha = 0.3) +
        labs(
          x = "Time spent hunting (days)", y = "Retrieved (number of birds)",
          fill = "Type", linetype = "Type", color = "Type", shape = "Type") +
        theme_classic() +
        scale_shape_manual(
          values = c(15, 16, 17, 18), 
          labels = c("Daily non-submit", "Daily submit", "Season non-submit", 
                     "Season submit")) +
        scale_linetype_manual(
          values = c(1, 2, 3, 4), 
          labels = c("Daily non-submit", "Daily submit", "Season non-submit", 
                     "Season submit")) +
        scale_fill_manual(
          values = MetBrewer::met.brewer("Hokusai3", 4), 
          labels = c("Daily non-submit", "Daily submit", "Season non-submit", 
                     "Season submit")) +
        scale_color_manual(
          values = MetBrewer::met.brewer("Hokusai3", 4), 
          labels = c("Daily non-submit", "Daily submit", "Season non-submit", 
                     "Season submit")) 
    }else if(type == "days"){
      versus_data %>% 
        mutate(
          type = 
            factor(type, 
                   levels = c("daily_nonsub", "daily_submit", "season_submit", 
                              "season_nonsub"), ordered = T)) %>% 
        group_by(type) %>% 
        mutate(box_mean = mean(days_hunted)) %>% 
        ungroup() %>% 
        ggplot(
          aes(x = type, y = days_hunted, size = retrieved, color = retrieved)) +
        geom_jitter() +
        stat_summary(fun = "mean", geom = "crossbar", width = .9, 
                     color = "red", show.legend = F) +
        coord_cartesian(ylim = c(0, ylim)) +
        labs(x = "Type", y = "Number of days hunted", color = "Retrieved", 
             size = "Retrieved") +
        theme_classic() +
        scale_color_viridis(direction = -1) +
        scale_x_discrete(
          labels = c("Daily\nnon-submit", "Daily\nsubmit", "Season\nsubmit", 
                     "Season\nnon-submit"))
    }else if(type == "retrieved"){
      versus_data %>% 
        mutate(
          type = 
            factor(
              type, 
              levels = c("daily_nonsub", "daily_submit", "season_submit", 
                         "season_nonsub"), 
              ordered = T)) %>% 
        group_by(type) %>% 
        mutate(box_mean = mean(retrieved)) %>% 
        ungroup() %>% 
        ggplot(
          aes(
            x = type, y = retrieved, size = days_hunted, color = days_hunted)) +
        geom_jitter() +
        stat_summary(fun = "mean", geom = "crossbar", width = .9, 
                     color = "red", show.legend = F) +
        coord_cartesian(ylim = c(0, ylim)) +
        labs(x = "Type", y = "Number of birds retrieved", color = "Days hunted", 
             size = "Days hunted") +
        theme_classic() +
        scale_color_viridis(direction = -1) +
        scale_x_discrete(
          labels = c("Daily\nnon-submit", "Daily\nsubmit", "Season\nsubmit", 
                     "Season\nnon-submit"))
    }else{
      message("Error: `type` must be 'line', 'days', or 'retrieved'.")
    }
  }