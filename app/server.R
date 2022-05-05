function(input, output, session) {

  # Use session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$email <- 'notification@subvertising.org'

  # Call the server function portion of the `cars_table_module.R` module file
  callModule(
    patients_table_module,
    "patients_table"
  )
}
