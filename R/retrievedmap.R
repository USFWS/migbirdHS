#' Map mean retrieved birds per species and state
#'
#' The \code{retrievedmap} function creates a hex bin map of the United States from daily Harvest Survey data. Maps are plotted in a grid to display all species. States that are red have the highest mean harvest, states that are yellow have the lowest mean harvest, and blank (or white) states have no data.
#' 
#' @importFrom geojsonio geojson_read
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom rgeos gCentroid
#' @importFrom broom tidy
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom stringr str_detect
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 coord_map
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#'  
#' @param data Daily data table
#' @param output Default is "grid"
#'  \itemize{
#'  \item "grid" - returns a single plot with a map for each species arranged in a grid
#'  \item "series" - returns each individual species plot in the "Plots" pane; toggle using the GUI left and right arrows
#'  }
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdMBHS}
#' 
retrievedmap <- 
  function(data, output = "grid"){
    spdf <- 
      spdf_retrievedmap
    
    spdf@data <- 
      spdf@data %>%
      mutate(google_name = gsub(" \\(United States\\)", "", google_name))
    
    spp_data <- 
      retrieved(data) %>%  
      rename(id = sampled_state)
    
    centers <- 
      cbind.data.frame(
        data.frame(gCentroid(spdf, byid = TRUE), id = spdf@data$iso3166_2)) %>% 
      filter(!id %in% c("DC", "HI")) 
    
    spdf_fortified <- 
      tidy(spdf, region = "google_name") %>% 
      left_join(spp_data, by = "id") %>% 
      filter(!is.na(sp_group_estimated)) %>% 
      left_join(
        tibble(
          state_abbr = datasets::state.abb,
          id = datasets::state.name),
        by = "id")
    
    plot_list <- 
      map(
        spp_data %>% 
          select(sp_group_estimated) %>% 
          distinct() %>% 
          mutate(
            sp_group_estimated = 
              ifelse(
                str_detect(sp_group_estimated, "Sea"), 
                "Sea Ducks", 
                sp_group_estimated)) %>% 
          pull(),
        function(.x){
          spdf_fortified %>% 
            mutate(
              sp_group_estimated = 
                ifelse(
                  str_detect(sp_group_estimated, "Sea"), 
                  "Sea Ducks", 
                  sp_group_estimated)) %>% 
            filter(sp_group_estimated == .x) %>% 
            ggplot() +
            geom_polygon(
              aes(x = long, y = lat, group = group, fill = mean_retrieved),
              color = "white") +
            geom_text(
              data = centers, 
              aes(x = x, y = y, label = id), nudge_y = 0, size = 2.5) +
            labs(title = .x, fill = "Mean\nRetrieved") +             theme_void() +
            scale_fill_gradientn(
              colors = c("#ffffb2",  "#fb9e4f", "#e91332"),
              na.value = "white") +
            coord_map(xlim = c(-132, -70), ylim = c(27, 55)) 
        }
      )
    
    if(output == "series"){
      print(plot_list)
    }
    else if(output == "grid"){
      wrap_plots(
        map(plot_list, ~.x + theme(plot.margin = unit(c(0,0,0,.5), "cm"))), 
        ncol = 2)
    }
    else{
      message("Error: Invalid `ouput`. Use 'grid' or 'series'.")
    }
  }
