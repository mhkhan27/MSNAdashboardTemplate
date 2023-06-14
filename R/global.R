
# validated analysis  -----------------------------------------------------

## Need to change, must be assume that I have only key_index and stat
validated_analysis <- read.csv("data-raw/validated_analysis_final.csv") |> dplyr::mutate(

  group_var_value_1 = dplyr::case_when(is.na(group_var_value_1) ~ "Overall",
                                       T~ group_var_value_1)) ## to be added in data

validated_analysis <- validated_analysis |> dplyr::filter(analysis_type %in% c("prop_select_one","prop_select_multiple"))

unit_col <- validated_analysis |> dplyr::select(dplyr::starts_with("group_var_value_")) |> names()

validated_analysis <- validated_analysis |> tidyr::unite(col = "group_var_value",unit_col,sep = " ~ ",remove = F)

# validated_analysis <- validated_analysis |> dplyr::group_by(analysis_type,analysis_var_1, group_var_value_1) |>
#   dplyr::arrange(-stat) |> dplyr::ungroup() |> as.data.frame()




# plotly_titel ------------------------------------------------------------

title_style <- list(
  family = "Roboto Condensed",
  size = 20,
  color = "#333333")



# read text file  ---------------------------------------------------------

text_file <- openxlsx::read.xlsx("data-raw/text_file.xlsx",sheet = 2)
overview <- text_file |> dplyr::filter(id == "Overview") |> dplyr::pull(details)
methodology <- text_file |> dplyr::filter(id == "Methodology") |> dplyr::pull(details)
limitation <- text_file |> dplyr::filter(id == "Limitations") |> dplyr::pull(details)
contact_name <- text_file |> dplyr::filter(id == "Contact_person_name") |> dplyr::pull(details)
contact_email <- text_file |> dplyr::filter(id == "contact_person_email") |> dplyr::pull(details)
contact <- paste0("<br><a href = mailto:" , contact_email, ">",contact_name,"</a>")


### Create basemap

admin_three_sf <- sf::st_read("data-raw/admin_renamed/OCHA_renamed_adm3.shp")
admin_two_sf <- sf::st_read("data-raw/admin_renamed/OCHA_renamed_adm2.shp")
admin_one_sf <- sf::st_read("data-raw/admin_renamed/OCHA_renamed_adm1.shp")
admin_zero_sf <- sf::st_read("data-raw/admin_renamed/OCHA_renamed_adm0.shp")



base_map <- leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
  leaflet::addPolygons(data = admin_zero_sf,color = "#EE5859",fillColor = "transparent")


# Assessed and non assessed map  ------------------------------------------

# data --------------------------------------------------------------------

HH_data<- openxlsx::read.xlsx("data-raw/clean_validated_dataset/cleaned_data.xlsx",sheet = "HH_data") ## HH data
# HH_data <- HH_data |> dplyr::filter(consent == "yes") ### need to adjust the conset
strata_checking <- openxlsx::read.xlsx("data-raw/data_diagnosis.xlsx",sheet = "strata_checking")


### group by

assessed_not_assesd <- list()
for (i in strata_checking$pop_group ){
  grouping_variable <- strata_checking |> dplyr::filter(pop_group == i) |> dplyr::pull(strata)
  admin_level <- strata_checking |> dplyr::filter(pop_group == i) |> dplyr::pull(admin_level)

  df_pop <- HH_data |> dplyr::filter(pop_group == i)

  number_survey<- df_pop |> dplyr::group_by(!!rlang::sym(grouping_variable)) |> dplyr::summarise(
    completed_survey = dplyr::n()
  ) |> dplyr::rename(
    admin_name = grouping_variable
  ) |> dplyr::mutate(
    admin_label = dplyr::case_when(admin_level == "admin1"~ "admin1_la",
                                   admin_level == "admin2"~ "admin2_la",
                                   admin_level == "admin3"~ "admin3_la")
  )


  #  |> dplyr::mutate(
  #   pop_group = i,
  #   strata = grouping_variable,
  #   admin_level = admin_level
  # )
  #

  if(admin_level == "admin1") {
    if(any(!number_survey$admin_name %in% admin_one_sf$admin1)) {stop("Strata not found")}
    assessed_not_assesd[[i]] <- admin_one_sf |> dplyr::left_join(number_survey,by = c("admin1" = "admin_name"))
  }
  if(admin_level == "admin2") {
    if(any(!number_survey$admin_name %in% admin_two_sf$admin2)) {stop("Strata not found")}
    assessed_not_assesd[[i]] <- admin_two_sf |> dplyr::left_join(number_survey,by = c("admin2" = "admin_name"))
  }
  if(admin_level == "admin3") {
    if(any(!number_survey$admin_name %in% admin_three_sf$admin3)) {stop("Strata not found")}
    assessed_not_assesd[[i]] <- admin_three_sf |> dplyr::left_join(number_survey,by = c("admin3" = "admin_name"))
  }

}

### Overview_map

overview_map <- base_map

for(i in 1:length(assessed_not_assesd)){


  label <- assessed_not_assesd[[i]] |> dplyr::filter(!is.na(completed_survey)) |> dplyr::pull(admin_label)  |> unique()


  dom <- assessed_not_assesd[[i]] |> dplyr::filter(!is.na(completed_survey)) |> dplyr::pull(completed_survey)

  bins <- c(0, 10, 20, 50, 100, 200, 500, Inf)
  pal <- leaflet::colorBin("YlOrRd", domain =dom , bins = bins,na.color = "#585858")

  overview_map <- overview_map |>
    leaflet::addPolygons(data = assessed_not_assesd[[i]],color = "#58585A",
                         label = ~htmltools::htmlEscape(get(label)),
                         labelOptions = leaflet::labelOptions(noHide = F,
                                                              sticky = T ,
                                                              textOnly = TRUE,
                                                              textsize = "11px"),
                         popup = paste( "<b>",assessed_not_assesd[[i]][[label]], "</b><br>",
                           "<b>Number of survey:</b>", assessed_not_assesd[[i]]$completed_survey),
                         weight = 2,dashArray = "3",fillColor = ~pal(assessed_not_assesd[[i]]$completed_survey),
                         highlightOptions = leaflet::highlightOptions(weight = 5,
                                                                      color = "#666",
                                                                      dashArray = "",
                                                                      fillOpacity = 0.7,
                                                                      bringToFront = TRUE),
                         group = names(assessed_not_assesd[i]))


}

overview_map <- overview_map |> leaflet::addLayersControl(
  baseGroups = names(assessed_not_assesd),
  options = leaflet::layersControlOptions(collapsed = FALSE))



### Getting Strata + population
look_for_strata<-list()
for(i in strata_checking$pop_group){
strata <- strata_checking |> dplyr::filter(pop_group == i)  |> pull(strata)

key <- paste0(i, " ~ " , unique(HH_data[[strata]]))

if(all(key %in% validated_analysis$group_var_value)) {key <- key}
if(all(!key %in% validated_analysis$group_var_value)) {key <-  paste0(unique(HH_data[[strata]]), " ~ " , i)}


look_for_strata[[i]]<- data.frame(
   group_var_value = key,
   pop_group = i,
   strata = strata,
   # group_val_value_1 = i,
   admin_name = unique(HH_data[[strata]])
 )

}
look_for_strata_df <- do.call("bind_rows",look_for_strata)


######## Strata level data for MAP

validated_analysis_strata <- validated_analysis |> dplyr::filter(group_var_value %in% look_for_strata_df$group_var_value)

validated_analysis_strata <- validated_analysis_strata |> dplyr::left_join(look_for_strata_df)

validated_analysis_strata |> names()

validated_analysis_strata <- validated_analysis_strata |> left_join(strata_checking,by = c("pop_group","strata"))

# validated_analysis_strata can be use in map tab, picker input for population group and indicator + choice and then leftjoing with
# admin boundy ( based on admin_level column), finally show the result



