fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  # Application Title
  titlePanel(
    h1("PendejAPP", align = 'center'),
    windowTitle = "Ejemplo"
  ),
  patients_table_module_ui("patients_table")
)

