#
#
# bind_loops <- function(loop_list){
#
# col_common = colnames(loop_list[[1]])
# for (i in 2:length(loop_list)){
#   col_common = intersect(col_common, colnames(loop_list[[i]]))
# }
#
# message("The below columns will be used as a primary key to join, please check carefully- ")
# print(col_common)
#
#
# choices <- c("Yes,the list is correct","No, the list is NOT correct")
# choice_index <- menu(choices, title = "Choose an option:")
# verification <- choices[choice_index]
# cat("You selected:", verification, "\n")
#
#
#
#
# if(verification == "Yes,the list is correct"){col_common <- col_common}
# if(verification == "No, the list is NOT correct"){
#   col_common <- readline(prompt =  "Please write down the primary keys- " )
# }
#
#
#
#
# indv_df <- loop_list %>% reduce(full_join, by = col_common)
#
#
# common_na_response <- indv_df |> select(col_common) |>  get_na_response_rates()
#
# col_name_na_in_reseponse <- common_na_response[common_na_response$num_non_response != 0,]$question
#
#
# if(any(common_na_response$num_non_response != 0)) {message("There should be no NAs in the common columns please check the following columns")
#   stop(paste0(col_name_na_in_reseponse,collapse = " ,"))}
#
# return(indv_df)
#
# }
#
#
# loop_edu <- loop_edu |> filter(!uuid %in% c("c78a316d-9b3e-4831-b353-48be3fd18bac"))
#
# loop_list <- list(loop_edu,loop_demo,loop_sante)
# a <- bind_loops(loop_list = loop_list)
#
