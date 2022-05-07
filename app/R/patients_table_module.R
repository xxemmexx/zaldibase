#' Patients Table Module UI
#'
#' The UI portion of the module for displaying the datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
patients_table_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        actionButton(
          ns("add_patient"),
          "Nouveau patient",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon('plus'),
          width = '100%'
        ),
        tags$br(),
        tags$br()
      )
    ),
    fluidRow(
      column(
        width = 12,
        title = "Mes patients",
        DTOutput(ns('patients_table')) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "patients_table_module.js"),
    tags$script(paste0("patients_table_module_js('", ns(''), "')"))
  )
}

#' Patients Table Module Server
#'
#' The Server portion of the module for displaying the datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

patients_table_module <- function(input, output, session, user_autho) {
  
  
  # trigger to reload data from the "patients" table
  session$userData$patients_trigger <- reactiveVal(0)

  # Read in "patients" table from the database
  patients <- reactive({
    session$userData$patients_trigger()

    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(
          created_at = as.POSIXct(created_at, tz = "UTC"),
          modified_at = as.POSIXct(modified_at, tz = "UTC")
        ) %>%
        arrange(desc(modified_at))
    }, error = function(err) {


      msg <- "Database Connection Error"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })

    out 
  })

  #user_autho <- reactiveVal(NULL)
  patients_table_prep <- reactiveVal(NULL)

  observeEvent(patients(), {
    
    out <- patients()

    ids <- out$uid

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Modifier" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Effacer" id = ', id_, ' style="margin: 0"><i class="fa fa-times-circle"></i></button>
        </div>'
      )
    })

    # Select relevant columns for the user
    out <- out %>%
      select(nom, prenom, date_naissance, condition)

    # Set the Action Buttons row to the first column of the `patients` table
    out <- cbind(
      tibble(" " = actions),
      out
    )

    if (is.null(patients_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      patients_table_prep(out)

    } else {

      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(patients_table_proxy, out, resetPaging = FALSE, rownames = FALSE)

    }
  })
  
  
  
  output$patients_table <- renderDT({
    req(user_autho(), patients_table_prep())
    
    
    out <- patients_table_prep()

    datatable(
      out,
      rownames = FALSE,
      colnames = c('Nom', 'Prénom', 'Date de naissance', 'Condition'),
                   #'Hôpital', 'Personne de contact', 'Created At',
                   #'Created By', 'Modified At', 'Modified By'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      options = list(
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) #%>%
      # formatDate(
      #   columns = c("created_at", "modified_at"),
      #   method = 'toLocaleString')

  })

  patients_table_proxy <- DT::dataTableProxy('patients_table')

  callModule(
    patients_edit_module,
    "add_patient",
    modal_title = "Registrer un nouveau patient",
    patient_to_edit = function() NULL,
    modal_trigger = reactive({input$add_patient})
  )

  observeEvent(is.null(user_autho()), {
    toggle("add_patient")
    
  })
  
  patient_to_edit <- eventReactive(input$patient_id_to_edit, {

    patients() %>%
      filter(uid == input$patient_id_to_edit)
  })

  callModule(
    patients_edit_module,
    "edit_patient",
    modal_title = "Modification du profil",
    patient_to_edit = patient_to_edit,
    modal_trigger = reactive({input$patient_id_to_edit})
  )

  patient_to_delete <- eventReactive(input$patient_id_to_delete, {

    patients() %>%
      filter(uid == input$patient_id_to_delete) %>%
      as.list()
  })

  callModule(
    patients_delete_module,
    "delete_patient",
    modal_title = "Effacer profil",
    patient_to_delete = patient_to_delete,
    modal_trigger = reactive({input$patient_id_to_delete})
  )

}
