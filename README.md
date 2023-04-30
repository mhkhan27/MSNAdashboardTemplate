MSNA Dashboard Template
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## MSNAdashboardTemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of MSNAdashboardTemplate is to …

<!-- TOC -->

## Installation

You can install the development version of MSNAdashboardTemplate like
so:

``` r
devtools:install_github("MSNAdashboardTemplate")
```

## Step by step guideline

#### STEP000:: Call the library

``` r
library(MSNAdashboardTemplate)
# library(analysistools)
```

#### STEP 001. Preapare the kobo tool

At the very beginning where you are developing the KOBO tool please make
sure you have used OCHA COD name in your KOBO tool’s name column. You’re
free to modify the label as desired, but please avoid changing the name
column. You can download OCHA COD using the `download_hdx_adm()`
function, which automatically retrieves the data from HDX. To obtain a
name that can be directly copied to your KOBO tool, use
`snakecase::to_snake_case()`. Here’s an example:

``` r
admin_boundary <- MSNAdashboardTemplate::download_hdx_adm(country_code = "lbn",admin_level = 2,df_index = 2)  ## Downloading OCHA-COD admin 2 data for Lebanon
admin_boundary <- admin_boundary |> mutate(
  name = snakecase::to_snake_case(admin2Name), ## Change admin2Name as necessary for your country
  `label::fr` = admin2Name
) |> as.data.frame()|> select( `label::en`,name)
```

The code above should generates a table like following, that can be
copied directly into the KOBO choices tab. While you’re free to modify
the label::en, please refrain from changing the name, as it will be used
for automation/upcoming functions that will be available soon.

#### STEP 002. Create DAP for the dashboard

``` r

survey<- readxl::read_excel("data-raw/kobo_tool.xlsx",sheet = "survey")

HH_data<- readxl::read_excel("data-raw/REACH_LBN_cleaned_data.xlsx",sheet = "HH_data")
INDV_data<- readxl::read_excel("data-raw/REACH_LBN_cleaned_data.xlsx",sheet = "INDV_data")

list_of_dataset <- list(
  HH_data = HH_data,
  INDV_data = INDV_data
)


MSNAdashboardTemplate::write_dap_for_dashboard(list_of_dataset = list_of_dataset,
                        kobo_survey = survey,
                        column_name_for_populaion_group = "pop_group")
```

Once you run the above code, you will have a excel file with two tab.
One `dap_for_dashboard` which is looks like -

    # A tibble: 484 × 6
       sector indicator main_variable        analysis_level `label::English`        
       <lgl>  <lgl>     <chr>                <chr>          <chr>                   
     1 NA     NA        today                HH_data        today                   
     2 NA     NA        pop_group            HH_data        What is the name of the…
     3 NA     NA        data_collection_mode HH_data        Is this interview condu…
     4 NA     NA        governorate          HH_data        In which governorate is…
     5 NA     NA        district             HH_data        In which district is th…
     6 NA     NA        point_accessible     HH_data        Is the point accessible?
     7 NA     NA        female_voice         HH_data        Are you conducting dual…
     8 NA     NA        female_enum          HH_data        Is there a female enume…
     9 NA     NA        consent              HH_data        Hello, I am from (REACH…
    10 NA     NA        head_hh              HH_data        Are you the head or co-…
    # ℹ 474 more rows
    # ℹ 1 more variable: `label::Arabic` <chr>

Here you should fill out the `sector` from dropdown and `indicator` name
aganist main_variable. Please note that if you keep `indicator` empty
then it wont be showing in the dashboard. In the second tab, please
select the `strata` and `admin_level` from dropdown menue

    # A tibble: 3 × 3
      pop_group strata admin_level
      <chr>     <lgl>  <lgl>      
    1 lebanese  NA     NA         
    2 prl       NA     NA         
    3 migrant   NA     NA         

#### STEP 003. Create list of mismatched strata

To join the map data with MSNA data, the strata name from the dataset
should be same as admin boundary name in the OCHA boundary. You can use
`write_mismatched_strata()` to compare the the admin names.

``` r
cntry_adm_3 <- MSNAdashboardTemplate::download_hdx_adm(country_code = "lbn",admin_level = 3,df_index = 2,keep_zip = T)

strata_checking <- readxl::read_excel("data-raw/data_diagnosis.xlsx",sheet = "strata_checking")
df <- readxl::read_excel("data-raw/REACH_LBN_cleaned_data.xlsx",sheet = "HH_data")


MSNAdashboardTemplate::write_mismatched_strata(ocha_cod_level_3 = cntry_adm_3,
                        cod_admin1_name = "admin1Name",
                        cod_admin2_name = "admin2Name",
                        cod_admin3_name = "admin3Name",
                        strata_checking_sheet = strata_checking,
                        hh_data=df,
                        population_group_column_name_strata_checking = "pop_group" )
```

The function will give an excel file which will look like the following
table. Please make sure you have filled the `ocha_cod_name` from the
drop down menu.

    # A tibble: 9 × 3
      df_admin_name  ocha_cod_name     level 
      <chr>          <chr>             <chr> 
    1 meten          el_meten          admin2
    2 batroun        el_batroun        admin2
    3 hermel         el_hermel         admin2
    4 koura          el_koura          admin2
    5 minieh_dennie  el_minieh_dennie  admin2
    6 w_bekaa        west_bekaa        admin2
    7 baalbek_hermel baalbek_el_hermel admin1
    8 mnt_lbn        mount_lebanon     admin1
    9 nabatieh       el_nabatieh       admin1

#### STEP 004. Rename OCHA name

Once you fill the output file from `write_mismatched_strata` function
then we will need to apply the changes to either dataset or to OCHA
cod.However as MSNA data mostly contain multiple loop so its better to
rename the ocha boundaries. You can apply
`write_renamed_ocha_admin_name` so write the renamed OCHA boundary as
shapefile to use in the dashboard.

``` r
MSNAdashboardTemplate::write_renamed_ocha_admin_name(ocha_cod_level_3 = cntry_adm_3,
                              cod_admin1_name = "admin1Name",
                              cod_admin2_name = "admin2Name",
                              cod_admin3_name = "admin3Name",
                              strata_checking_sheet =    strata_checking,
                              mistached_list = mistached_list,
                              output_path = "data-raw/OCHA_renamed_adm3.shp")
```

#### STEP 005. Create Analysis file \[Ignore incase you have validated analysis file with key index\]

You can do analysis using any tools or package or whatever you want but
make sure you have `key_index` and `state`

#### step 005. Data preparation for dashboard

\[to be done - 1. Identify grouping variables from key index 2. Identify
main variable from key index 3. Identify analysis type from key index 4.
Identify Question type from key index/kobo/dap\] 5. Joining DAP
information

#### STEP 006. Create text file

The function `create_text_input()` will create the an excel file to be
shown in the dashboard `overview` page

#### STEP 005. Create summary

Ideally it will check all the module for the dashboard It should provide
list of error/issue to fix before running the create_dashboard

#### STEP 007. Create dashboard

``` r

MSNAdashboardTemplate::create_dashboard(country = "Iraq",
                               assessment_name  = "McNA",
                               year = 2024,
                               validated_analysis_file = validated_analysis_file,
                               key_index_column= key_index_column, 
                               stat_column = stat_column
                               ) 
```

### How to customize the dashboard?

In case you want to customize the dashboard, you can follow the
following steps -

#### Step 0.1.1 clone the repo

To customized the dashboard, you first needs to clone the repository
from the github.

#### Step 0.1.1 understanding the repo

Once you are done with the repo, now you need to understand the
structure of repo. The repository is created using `golem` package.

##### What is golem

The `golem` package is a framework for building and deploying
production-ready Shiny applications in R. Shiny is a web application
framework for R that allows users to create interactive web applications
using R code. However, while Shiny is great for creating interactive
prototypes, it is not always well-suited for building production-ready
applications with robust performance, security, and scalability.

This is where`golem` comes in. `Golem` provides a framework for
organizing and structuring your Shiny code to create scalable and
maintainable applications. It includes a set of best practices and
conventions for building Shiny applications, such as separating the UI
(user interface) and server logic into separate files, using reactive
programming to minimize data processing, and leveraging package
management to simplify dependency management.

In addition to these best practices, `golem` also provides a set of
tools for testing, debugging, and deploying Shiny applications. For
example, it includes a command-line interface for creating and managing
application templates, as well as tools for managing application
configuration, logging, and error handling.

Overall, the `golem` package is a powerful tool for building robust,
scalable, and maintainable Shiny applications in R.

##### Repository Stucture

The Golem package in R is designed to facilitate the creation of
production-ready Shiny applications. It is organized into several
subdirectories, each of which serves a specific purpose:

1.  `R/:` This directory contains the R code for your application. You
    should place your application logic, including any functions you
    write, in this directory.

2.  `data/:` This directory is used to store any data files that your
    application needs to function. You can also store any other
    resources that your application requires in this directory.

3.  `www/:` This directory is used to store any static files that your
    application requires, such as images, stylesheets, or JavaScript
    files.

4.  `inst/:` This directory is used to store any additional files that
    your application needs to function, such as configuration files or
    documentation.

5.  `tests/:` This directory is used to store any test files for your
    application. You can write unit tests for your application logic in
    this directory.

6.  `man/:` This directory contains the documentation for your
    application. You should document your functions and other objects in
    this directory using the Roxygen2 syntax.

7.  `NAMESPACE:` This file specifies the package’s exported functions
    and other objects.

8.  `DESCRIPTION:` This file contains metadata about your package,
    including its name, version, and dependencies.

9.  `README.md:` This file contains information about your package,
    including how to install and use it

``` r
 fs::dir_tree()
.
├── R
│   ├── app_config.R
│   ├── app_server.R
│   ├── app_ui.R
│   ├── run_app.R
│   ├── golem_utils_ui.R
│   ├── golem_utils_server.R
│   ├── 02_bind_loops.R
│   ├── download_hdx_admin.R
│   ├── utils_helpers.R
│   ├── prepare_dap_for_dashboard.R
│   ├── identifying_mismatch_strata_name_with_ocha_cod.R
│   ├── rename_cod.R
│   ├── create_analysis_from_dap.R
│   ├── mod_introduction.R
│   ├── mod_map.R
│   ├── mod_Graph.R
│   └── mod_Demographic.R
├── MSNAdashboardTemplate.Rproj
├── dev
│   ├── 01_start.R
│   ├── 02_dev.R
│   ├── 03_deploy.R
│   └── run_dev.R
├── inst
│   ├── app
│   │   └── www
│   │       ├── favicon.ico
│   │       └── reach_logo.png
│   └── golem-config.yml
├── man
│   ├── figures
│   │   └── README-pressure-1.png
│   ├── download_hdx_adm.Rd
│   ├── write_mismatched_strata.Rd
│   ├── write_dap_for_dashboard.Rd
│   ├── write_renamed_ocha_admin_name.Rd
│   └── create_dashboard.Rd
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── LICENSE.md
├── README.Rmd
├── README.md
├── tests
│   ├── testthat
│   │   ├── test-golem_utils_ui.R
│   │   ├── test-golem_utils_server.R
│   │   ├── test-download_hdx_admin.R
│   │   ├── test-utils_helpers.R
│   │   ├── test-mod_map.R
│   │   ├── test-mod_Graph.R
│   │   └── test-mod_Demographic.R
│   └── testthat.R
├── style.css
├── app.R
├── recommendatio.html
└── data-raw
    ├── admin_mismatch_fix.xlsx
    └── data_diagnosis.xlsx
```

##### Understanding Module

With in the `R/` folder you will scripts started with `mod_` which I
will be calling module. Each module represent a `tab` in the dashboard.
Within the scripts, each module have two parts, 1. user interface (ui)
2. server.

#### Step 0.1.1 removing any module

If you want to remove any module/tab from the dashboard, you can easily
do it from `app_ui.R` and `app_server.R`. All you have to do is delete
the module from both `app_ui` and `app_server`

#### Step 0.1.1 adding any module

If you want to add a new tab then you will need to create a module first
and then add the module to the `app_ui.R` and `app_server.R`

``` r
## to create the module
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
```

#### Step 0.1.1 changing any module

You can change a specific tab without creating error in out tab by
editing existing module. just make the changes in `mod_name_of_module1`
and then you are all set. \[Make sure you have made changes in both ui
and server\]

#### Step 0.1.1 Check the tests

If you made any changes in the dashboard, please make sure you have run
the tests before deploying the app. It will make sure the the changes
didn’t change anything in other tab or in the app. You can hit
`Ctrl + Shift + T` to run the tests

#### Step 0.1.1 CMD check

It is also important to run the CDM check. If your CDM check fails then
the app may runs in local but in the shinyserver it might not run. You
can hit `Ctrl + Shift + E` to run the CMD checks

#### Step 0.1.1 Run the app

``` r


MSNAdashboardTemplate::create_dashboard(country = "Iraq",
                               assessment_name  = "McNA",
                               year = 2024,
                               validated_analysis_file = validated_analysis_file,
                               key_index_column= key_index_column, 
                               stat_column = stat_column
                               ) 
```
