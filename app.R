# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

rm(list = ls())

pkgload::load_all(export_all = FALSE,helpers = FALSE,
                  attach_testthat = FALSE)

options( "golem.app.prod" = TRUE)

MSNAdashboardTemplate::create_dashboard(country = "lebanon",
                                        assessment_name =  "MsNa",
                                        year = 2024) # add parameters here (if any)

