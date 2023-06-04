rm(list = ls())
library(illuminate)
library(purrr)
library(openxlsx)
library(stringr)
library(readxl)
library(tidyverse)
library(srvyr)

read_sheets("../../../08_REACH_IRAQ_UPDATE/mh1/22_Surge/02_lebanon/01_LEB_MSNA_Feb23/06_outputs/03_cleaned_data/05_final_validated_data/cleaned_data.xlsx")

read_sheets("../../../08_REACH_IRAQ_UPDATE/mh1/22_Surge/02_lebanon/01_LEB_MSNA_Feb23/03_dap_and_tool/01_tool/kobo_tool.xlsx")

cols_to_ana <- (HH_data |> names()) [c(310:679,692:773)]
cols_to_ana_indv <- (INDV_data |> names()) [c(3:77,137:170)]



analysis_hh <- illuminate::survey_analysis(df = HH_data,weights = F,vars_to_analyze = cols_to_ana)

analysis_group_by <- illuminate::survey_analysis(df = HH_data,weights = F,vars_to_analyze = cols_to_ana,
                                                 disag = "pop_group" ) |> ungroup()  |> select(stat,key_index)

analysis_indv <- illuminate::survey_analysis(df = INDV_data,weights = F,vars_to_analyze = cols_to_ana_indv) |> select(stat,key_index)

analysis_group_by_indv <- illuminate::survey_analysis(df = INDV_data,weights = F,vars_to_analyze = cols_to_ana_indv,
                                                      disag = "pop_group" ) |> ungroup() |> select(stat,key_index)


analysis_file <- analysis_hh |> bind_rows(analysis_group_by) |> bind_rows(analysis_indv) |> bind_rows(analysis_group_by_indv)


analysis_file <- analysis_file[!duplicated(analysis_file$key_index ),]

write.csv(analysis_file,"data-raw/validate_analysis.csv")


MSNAdashboardTemplate::write_dap_for_dashboard(list_of_dataset = list(HH_data=HH_data,
                                                                      INDV_data= INDV_data),
                                               column_name_for_populaion_group = "pop_group",
                                               kobo_survey = survey )

########### Create dashboard input #####

validated_anaysis <- read.csv("data-raw/validate_analysis.csv")
dap <- read_excel("data-raw/data_diagnosis.xlsx",1)

dap_filted <- dap |> dplyr::filter(!is.na(indicator) & !is.na(sector))

##### logic :: indicator and dap should not be black [either both should be filled or both should be blank]


analysis_table<- create_analysis_key_table(.results = validated_anaysis,analysis_key = "key_index") |>
  left_join(validated_anaysis,by = "key_index")

analysis_table_filtered <- analysis_table |> dplyr::filter(analysis_var_1 %in% dap_filted$main_variable) |> dplyr::left_join(
  dap_filted,by= c("analysis_var_1" = "main_variable")
) |> dplyr::mutate(stat = dplyr::case_when(analysis_type %in% c("prop_select_one","prop_select_multiple") ~ round(stat*100,0),
            T~stat))

write.csv(analysis_table_filtered ,"data-raw/dashboard_input.csv")


####


