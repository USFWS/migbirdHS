#' Summarize number of days hunted
#'
#' The \code{bagdays} function determines the total number of days hunted per hunter and species group in the daily Harvest Survey data.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme 
#' @importFrom ggplot2 facet_wrap
#' @importFrom MetBrewer met.brewer
#' @importFrom dplyr n_distinct
#' 
#' @param data Daily data table
#' @param output Default is "table"
#'  \itemize{
#'  \item "table" - returns a table
#'  \item "plot" - returns a ggplot
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHS}
#' 
bagdays <-
  function(data, output = "table"){
    if(output == "table"){
      data |>
        select(
          selected_hunterID, harvested_date, sp_group_estimated, retrieved) |>
        group_by(selected_hunterID, sp_group_estimated) |>
        summarize(
          n_days = n(),
          n_retrieved = sum(retrieved)) |>
        ungroup()
    }else if(output == "plot"){
      data |>
        select(
          selected_hunterID, harvested_date, sp_group_estimated, retrieved) |>
        group_by(selected_hunterID, sp_group_estimated) |>
        summarize(
          n_days = n(),
          n_retrieved = sum(retrieved)) |>
        ungroup() |> 
        rename(sp = sp_group_estimated) |> 
        mutate(sp = ifelse(str_detect(sp, "Sea"), "Sea Ducks", sp)) |> 
        ggplot(
          aes(x = n_days, y = n_retrieved, color = sp, fill = sp)) +
        geom_jitter(alpha = 0.2, size = 2) +
        stat_smooth(method = "lm", linewidth = 1, alpha = 0.3) +
        labs(x = "Days hunted", y = "Birds retrieved", color = "Species", 
             shape = "Species", linetype = "Species", fill = "Species", 
             alpha = "Species") +
        theme_classic() +
        scale_color_manual(
          values = 
            met.brewer("Hokusai3", n_distinct(daily_records$sp_group_estimated))) + 
        scale_fill_manual(
          values = 
            met.brewer("Hokusai3", n_distinct(daily_records$sp_group_estimated))) +  
        theme(legend.position = "none") +
        facet_wrap(~sp)
    }else{
      message("Error: `output` must be 'table' or 'plot'.")
    }
  }