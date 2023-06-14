#' Graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Graph_ui <- function(id){
  ns <- NS(id)
  tabPanel("Graph!",
           icon = shiny::icon("chart-area"),

           br(),
           shiny::sidebarPanel(
             tags$style(".well {background-color:#F2F2F2;}"),
             width = 12,
           # if length more than 1
           tags$div(
             class = "ident-picker",
             shinyWidgets::pickerInput(ns("select_pop"),
                                              label = "Select population group:",
                                              choices =  unique(validated_analysis$group_var_value_1),
                                              selected = unique(validated_analysis$group_var_value_1)[1],
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),


           # shinyWidgets::pickerInput(ns("select_analysis_level"),
           #                           label = "Select analysis level:",
           #                           choices =  ,
           #                           selected = ,
           #                           multiple = F,
           #                           options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           # ),style="display:inline-block"),

           tags$div(shinyWidgets::pickerInput(ns("select_sector"),
                                              label = "Select Sector:",
                                              choices =  unique(validated_analysis$sector),
                                              selected = unique(validated_analysis$sector)[1],
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),


           tags$div(shinyWidgets::pickerInput(ns("select_indicator"),
                                              label = "Select Indicator:",
                                              choices =  NULL,
                                              selected = NULL,
                                              multiple = F,
                                              options = shinyWidgets::pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
           ),style="display:inline-block"),
           ), # end slidebar
           hr(),
           shiny::mainPanel(width = 12,

           plotly::plotlyOutput(ns("pie"))
           )# end mainpanel

  ) # end tabpanel
}

#' Graph Server Functions
#'
#' @noRd
mod_Graph_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # dashboard_input <- golem::get_golem_options("dashboard_input")

    data_pop <- reactive({validated_analysis |> dplyr::filter(group_var_value_1 == input$select_pop)})


    sector_list <-shiny::reactive({
      data_pop()$sector |> unique()

    })


    shiny::observe({
      shinyWidgets::updatePickerInput(session, "select_sector",
                                      choices = sector_list() ,
                                      selected = sector_list()[1])
    })



    sector_selected <- shiny::reactive({input$select_sector})

    indicator_list <- shiny::reactive({
      data_pop()[data_pop()$sector == sector_selected(),"indicator"] |> unique()

    })

    ####################### available indicator name in the selected governorate ############


    shiny::observe({
      shinyWidgets::updatePickerInput(session, "select_indicator",
                                      choices = indicator_list() ,
                                      selected = indicator_list()[1])
    })




    ## Apply filter

    dash_df <- reactive({

      validated_analysis |> dplyr::filter(group_var_value_1 == input$select_pop &
                                                          sector == input$select_sector &
                                                          indicator == input$select_indicator)

      })



    output$pie <- plotly::renderPlotly ({




      if( dash_df()$analysis_type[1] == "prop_select_one" & !is.na( dash_df()$analysis_type[1]) &
          length(dash_df()$analysis_var_value_1) <6){
        plotly::plot_ly() |> plotly::add_trace(hole = .6,type = "pie",
                                               labels = dash_df()$analysis_var_value_1,
                                               values= dash_df()$stat,
                                               showlegend =T) |>
          plotly::layout(title =  unique(dash_df()$indicator),
                         legend = list(orientation = 'h',
                                       xanchor = "center",
                                       x = 0.5))
      }


      else if( dash_df()$analysis_type[1] == "prop_select_one" & !is.na( dash_df()$analysis_type[1]) &
               length(dash_df()$analysis_var_value_1) >5){

        ## Barchart
        plotly::plot_ly(data = dash_df(),height = 500,
                        type = "bar",
                        y = ~analysis_var_value_1,
                        x= ~stat,
                        texttemplate = '%{x}', textposition = 'outside',
                        marker = list(color = "#585858")) |>
          plotly::layout(title =  list(text =  paste0("<b>",unique(dash_df()$indicator,"</b>")),font = title_style),
                         yaxis = list(title = "",categoryorder = "total ascending"),
                         xaxis = list(title = "",ticksuffix = "%"))
      }




      else if( dash_df()$analysis_type[1] == "prop_select_multiple" & !is.na(dash_df()$analysis_type[1])){

        ## Barchart
        plotly::plot_ly(data = dash_df(),height = 500,
                        type = "bar",
                        y = ~analysis_var_value_1,
                        x= ~stat,
                        texttemplate = '%{x}', textposition = 'outside',
                        marker = list(color = "#585858")) |>
          plotly::layout(title =  list(text =  paste0("<b>",unique(dash_df()$indicator,"</b>")),font = title_style),
                         yaxis = list(title = "",categoryorder = "total ascending"),
                         xaxis = list(title = "",ticksuffix = "%"),
                         annotations = list(text = "<i>Note: This is a multiple choice question,<br>the percentages can add up to more than 100%.</i>",
                                            x = max(dash_df()$stat,na.rm = T), y=1 ,showarrow=FALSE ))
      }



    })

  ## find analysis type

  ## bar chart if select multiple

  ## pie chart if select one

  ## integer??



})
  }

## To be copied in the UI
# mod_Graph_ui("Graph_1")

## To be copied in the server
# mod_Graph_server("Graph_1")
