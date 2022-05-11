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
  
  output$role <- renderText({
    req(credentials()$user_auth)
    # use is.null(session$user) so it still works when testing locally
    credentials()$info[['permissions']]

  })

  # Call the server functions in the module files in the R dir
  patientsTableModuleServer("patients_table",
                            reactive(credentials()$user_auth))
  
  dossiersTableModuleServer("dossiers_table",
                            reactive(credentials()$user_auth))
  
  #load saved parameters 
  # observeEvent(reactive(logout_init()), {
  #   updateNavbarPage(session, "tabs", selected = "About")
  # })  
  
  # set suspendWhenHidden to FALSE so it renders even without output
  outputOptions(output, 'role', suspendWhenHidden = FALSE)
}
