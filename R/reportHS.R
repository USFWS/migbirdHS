#' Write an R markdown report from template
#'
#' Create documentation with figures and tables that summarizes Harvest Survey daily and season data.
#'
#' @importFrom rmarkdown render
#'
#' @param daily Daily data table
#' @param season Season data table
#' @param ref_data Reference data table for the same year as the daily data
#' @param partypath File path to the folder containing results from \code{\link{partyproof}} function
#' @param type Type of report. One of the following options may be supplied:
#' \itemize{
#' \item survey_analytics - for a summary report of survey analytics
#' \item season_report - for a summary of the Harvest Survey data}
#' @param year The hunting season for which Harvest Survey data were collected
#' @param outpath Folder in which to save the completed report
#' @param file What the report file should be named
#' @export
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHS}

reportHS <-
  function(daily, season, ref_data, partypath, type, year, outpath, file){
    
    # Create Rmd for download
    if(type == "survey_analytics"){
      render(
        input =
          # Use the specified template
          system.file(
            "templates",
            paste0(type, ".Rmd"),
            package = "migbirdHS"),
        # Include the specified parameters so the functions can run
        params =
          list(
            daily = daily,
            season = season,
            ref_data = ref_data,
            partypath = partypath,
            year = year),
        output_file = file,
        output_dir = outpath,
        # Don't show lengthy knitting status text in console
        quiet = TRUE)
    }
    else if(type == "season_report"){
      render(
        input =
          # Use the specified template
          system.file(
            "templates",
            paste0(type, "_HS", ".Rmd"),
            package = "migbirdHS"),
        # Include the specified parameters so the functions can run
        params =
          list(
            daily = daily,
            season = season,
            ref_data = ref_data,
            partypath = partypath,
            year = year),
        output_file = file,
        output_dir = outpath,
        # Don't show lengthy knitting status text in console
        quiet = TRUE)
    }
    else{
      message("Indicated type must be 'survey_analytics' or 'season_report'.")
    }
    
  }