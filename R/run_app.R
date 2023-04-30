#' Run the Shiny Application
#'
#' @param country Country Name
#' @param assessment_name Please define the assessment name
#' @param year Year of the assessment
#' @inheritParams shiny::shinyApp
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#'
create_dashboard <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    country ,
    assessment_name,
    year,
    analysis_file =NULL,
    key_index_column=NULL,
    stat_column=NULL,
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern

    ),
    golem_opts = list(country= country,
                      assessment_name =assessment_name,
                      year=year,
                      analysis_file =analysis_file,
                      key_index_column =key_index_column,
                      stat_column =stat_column,
                      ...)
  )
}
