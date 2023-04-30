



create_text_input <- function(output_path = "data-raw/text_file.xlsx"){

  col_id <- c("Overview","Methodology",
              "Limitations","Contact_person_name",
              "contact_person_email")

  df <- tibble::tibble(
    id = col_id,
    details = NA_character_ )

  ## how to fill this
  readme <- tibble::tibble(
    readme = "This should be fillup xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    trips = "use strong() for bold"
  )




  ################################ wrtie excel ##########################

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "README")
  openxlsx::addWorksheet(wb, "text_input")


  openxlsx::writeDataTable(wb, "README", x = readme)
  openxlsx::writeDataTable(wb, "text_input", x = df )

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)


}
