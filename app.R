# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

#pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
aiti_dashboard() # add parameters here (if any)
 