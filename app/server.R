function(input, output, session) {

  # Everything related to shinyauthr ------------------------------------------
  credentials <- shinyauthr::loginServer(id = "login",
                                         data = user_base,
                                         user_col = user,
                                         pwd_col = password,
                                         sodium_hashed = TRUE,
                                         reload_on_logout = TRUE,
                                         log_out = reactive(logout_init()))
  
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))

  # shinyauthr ----------------------------------------------------------------
  
  session$userData$username <- reactive({
    if(is.null(credentials()$user_auth)) {
      username <- 'amas'
    } else {
      username <- credentials()$info[['user']]
    }
    username
    })
  
  
  output$role <- renderText({
    req(credentials()$user_auth)
    # use is.null(session$user) so it still works when testing locally
    credentials()$info[['permissions']]

  })
  
  # output$user_table <- renderTable({
  #   credentials()$info
  # })

  # Call the server functions in the module files in the R dir
  # patientsTableModuleServer("patients_table",
  #                           reactive(credentials()$user_auth))
  
  dossiersTableModuleServer("dossiers_table",
                            reactive(credentials()))
  
  #output$target_uid <- renderText({selected_patient_uid()})
  
  # fichePatientModuleServer("fiche_patient",
  #                          selected_patient_uid())
   
  
  # set suspendWhenHidden to FALSE so it renders even without output
  outputOptions(output, 'role', suspendWhenHidden = FALSE)
  
}
