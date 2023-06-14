#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tabPanel("Map!",
           icon = shiny::icon("fa-solid fa-map"),
           tags$div(
             class = "ident-picker",
             shinyWidgets::pickerInput(ns("select_pop"),
                                       label = "Select population group:",
                                       choices =  unique(validated_analysis$group_var_value_1),
                                       selected = unique(validated_analysis$group_var_value_1)[1],
                                       multiple = F,
                                       options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
             ),style="display:inline-block"),

           tags$div(shinyWidgets::pickerInput(ns("select_sector"),
                                              label = "Select Sector:",
                                              choices =  unique(validated_analysis$sector),
                                              selected = unique(validated_analysis$sector)[1],
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),


           tags$div(shinyWidgets::pickerInput(ns("select_indicator"),
                                              label = "Select Indicator:",
                                              choices =  NULL, ## Need to add
                                              selected = NULL, ## Need to add
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),

           tags$div(shinyWidgets::pickerInput(ns("select_choice"),
                                              label = "Select choice:",
                                              choices =  NULL,## Need to add
                                              selected = NULL,## Need to add
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),

           br()
  ) # end tabpanel
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    validated_analysis_strata


  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
