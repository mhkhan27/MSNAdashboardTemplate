# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


# delete everything from data-raw
# Make a copy to your data and tool in the data-raw

rm(list = ls())

new <- c(T,F)[2] # change it to one for the first time.


if(new == T){

# Download OCHA cod -------------------------------------------------------

  download_hdx_adm(country_code = "lbn",admin_level = 2,keep_zip = T,output_path = "data-raw/")


# create_dap for dashboard ------------------------------------------------

survey<- readxl::read_excel("data-raw/kobo_tool.xlsx",sheet = "survey")

HH_data<- readxl::read_excel("data-raw/clean_validated_dataset/cleaned_data.xlsx",sheet = "HH_data") ## HH data
INDV_data<- readxl::read_excel("data-raw/clean_validated_dataset/cleaned_data.xlsx",sheet = "INDV_data") ## indv data

list_of_dataset <- list(
  HH_data = HH_data,
  INDV_data = INDV_data
)


MSNAdashboardTemplate::write_dap_for_dashboard(list_of_dataset = list_of_dataset,
                                               kobo_survey = survey,
                                               column_name_for_populaion_group = "pop_group")



# STEP 003. Create list of mismatched strata ------------------------------
cntry_adm_3 <- sf::st_read("data-raw/ocha_cod_raw/lbn_admbnda_adm3_cdr_20200810.shp")

strata_checking <- readxl::read_excel("data-raw/data_diagnosis.xlsx",sheet = "strata_checking")
df <- readxl::read_excel("data-raw/REACH_LBN_cleaned_data.xlsx",sheet = "HH_data")


MSNAdashboardTemplate::write_mismatched_strata(ocha_cod_level_3 = cntry_adm_3,
                                               cod_admin1_name = "admin1Name",
                                               cod_admin2_name = "admin2Name",
                                               cod_admin3_name = "admin3Name",
                                               strata_checking_sheet = strata_checking,
                                               hh_data=df,
                                               population_group_column_name_strata_checking = "pop_group" )



# STEP 004. Rename OCHA name ----------------------------------------------

mistached_list <- readxl::read_excel("data-raw/admin_mismatch_fix.xlsx")


MSNAdashboardTemplate::write_renamed_ocha_admin_name(ocha_cod_level_3 = cntry_adm_3,
                                                     cod_admin1_name = "admin1Name",
                                                     cod_admin2_name = "admin2Name",
                                                     cod_admin3_name = "admin3Name",
                                                     strata_checking_sheet =    strata_checking,
                                                     mistached_list = mistached_list,
                                                     output_path = "data-raw/admin_renamed/OCHA_renamed_adm3.shp")

###export_admin

admin_three <- sf::st_read("data-raw/admin_renamed/OCHA_renamed_adm3.shp")
admin_two <- admin_three |> dplyr::group_by(admin1,admin1_la,admin2,admin2_la) |> dplyr::summarise()
admin_one <- admin_two |> dplyr::group_by(admin1,admin1_la) |> dplyr::summarise()
admin_zero <- admin_one |> dplyr::group_by() |> dplyr::summarise()

sf::st_write(admin_two, "data-raw/admin_renamed/OCHA_renamed_adm2.shp")
sf::st_write(admin_one, "data-raw/admin_renamed/OCHA_renamed_adm1.shp")
sf::st_write(admin_zero, "data-raw/admin_renamed/OCHA_renamed_adm0.shp")



}

pkgload::load_all(export_all = FALSE,helpers = FALSE,
                  attach_testthat = FALSE)

options( "golem.app.prod" = TRUE)

MSNAdashboardTemplate::create_dashboard(country = "lebanon",
                                        assessment_name =  "MsNa",
                                        year = 2024) # add parameters here (if any)

