function(input, output, session) {

  # Use session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'notification@subvertising.org'

  # Everything related to shinyauthr ------------------------------------------
  credentials <- shinyauthr::loginServer(id = "login",
                                         data = user_base,
                                         user_col = user,
                                         pwd_col = password,
                                         log_out = reactive(logout_init()))
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  # shinyauthr ----------------------------------------------------------------
  
  
  # Call the server function portion of the `patients_table_module.R` module file
  callModule(
    patients_table_module,
    "patients_table",
    reactive(credentials()$user_auth),
    reactive(credentials()$info[["permissions"]])
  )
}
