#' Demographic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Demographic_ui <- function(id){
  ns <- NS(id)
  tabPanel("Demographic!",
           icon = shiny::icon("chart-area"),

           br()
  ) # end tabpanel
}

#' Demographic Server Functions
#'
#' @noRd
mod_Demographic_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Demographic_ui("Demographic_1")

## To be copied in the server
# mod_Demographic_server("Demographic_1")
