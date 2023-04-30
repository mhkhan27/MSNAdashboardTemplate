#' introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduction_ui <- function(id){
  ns <- NS(id)
  tabPanel("Overview",
           icon = shiny::icon("chart-bar"),

           br()

  ) # end tabpanel
}

#' introduction Server Functions
#'
#' @noRd
mod_introduction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_introduction_ui("introduction_1")

## To be copied in the server
# mod_introduction_server("introduction_1")
