# validated_result=
# stat_column = "stat"
# key_index = "key_index"
# dap_from_data_diagnosis =
# output_path = "data-raw/validated_analysis_final.csv"
#
#
#
# validated_result <- validated_result |> dplyr::select(dplyr::all_of(stat_column),dplyr::all_of(key_index))
# dap_filted <- dap_from_data_diagnosis |> dplyr::filter(!is.na(indicator) & !is.na(sector))
#
# ##### logic :: indicator and dap should not be black [either both should be filled or both should be blank]
#
#
# analysis_table<- presentresults::create_analysis_key_table(.results = validated_result,analysis_key = "key_index") |>
#   dplyr::left_join(validated_result,by = "key_index")
#
# analysis_table_filtered <- analysis_table |> dplyr::filter(analysis_var_1 %in% dap_filted$main_variable) |> dplyr::left_join(
#   dap_filted,by= c("analysis_var_1" = "main_variable")
# ) |> dplyr::mutate(stat = dplyr::case_when(analysis_type %in% c("prop_select_one","prop_select_multiple") ~ round(stat*100,0),
#                                            T~stat))
#
# write.csv(analysis_table_filtered ,output_path)
