dashboard_input <- read.csv("data-raw/dashboard_input.csv") |> dplyr::mutate(

  group_var_value_1 = dplyr::case_when(is.na(group_var_value_1) ~ "Overall",
                                       T~ group_var_value_1)) ## to be added in data

dashboard_input <- dashboard_input |> dplyr::filter(analysis_type %in% c("prop_select_one","prop_select_multiple"))


# dashboard_input <- dashboard_input |> dplyr::group_by(analysis_type,analysis_var_1, group_var_value_1) |>
#   dplyr::arrange(-stat) |> dplyr::ungroup() |> as.data.frame()




# plotly_titel ------------------------------------------------------------

title_style <- list(
  family = "Roboto Condensed",
  size = 20,
  color = "#333333")
