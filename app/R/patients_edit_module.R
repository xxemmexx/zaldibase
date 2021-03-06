

#' Add & Edit Module
#'
#' Module to add & edit patients in the patients database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param patient_to_edit reactive returning a 1 row data frame of the patient to edit
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
patientsEditModuleServer <-
  function(id,
           modal_title,
           patient_to_edit,
           modal_trigger) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   
                   observeEvent(modal_trigger(), {
                     hold <- patient_to_edit()
                     
                     showModal(modalDialog(
                       fluidRow(
                         column(
                           width = 5,
                           textInput(ns("nom"),
                                     'Nom',
                                     value = ifelse(is.null(hold), "", hold$nom)),
                           textInput(
                             ns("prenom"),
                             'Prénom',
                             value = ifelse(is.null(hold), "", hold$prenom)
                           ),
                           dateInput(
                             ns("date_naissance"),
                             'Date de naissance',
                             value = ifelse(is.null(hold), "", hold$date_naissance),
                             language = "fr"
                           ),
                         ),
                         column(
                           width = 7,
                           # selectInput(
                           #   ns('hopital'),
                           #   "Hôpital d'origine",
                           #   choices = c(
                           #     'Centre Hospitalier Albertville-Moûtiers',
                           #     'Centre Hospitalier Universitaire de Grenoble'
                           #   ),
                           #   selected = ifelse(is.null(hold), "", hold$hopital)
                           # ),
                           # textInput(
                           #   ns("contact"),
                           #   'Personne de contact',
                           #   value = ifelse(is.null(hold), "", hold$contact)
                           # ),
                           selectInput(
                             ns('condition'),
                             'Condition',
                             choices = c(
                               'Le duele la verga',
                               'Le duele la cola',
                               'Le huele la cola',
                               'Suegra',
                               'Autre...' = 'Autre'
                             ),
                             selected = ifelse(is.null(hold), "", hold$condition)
                           ),
                           conditionalPanel(
                             "input.condition == 'Autre'",
                             textAreaInput(ns('description'),
                                           'Description',
                                           placeholder = "Decrivez..."),
                             ns = ns
                           )
                         )
                       ),
                       title = modal_title,
                       size = 'm',
                       footer = list(
                         modalButton('Annuler'),
                         actionButton(
                           ns('submit'),
                           printButtonLabel(modal_title),
                           class = "btn btn-primary mb1 bg-olive",
                           style = "color: white"
                         )
                       )
                     ))
                     
                     # Observe event for "Nom" text input in Add/Edit Patient
                     # `shinyFeedback`
                     observeEvent(input$nom, {
                       if (input$nom == "") {
                         shinyFeedback::showFeedbackDanger("nom",
                                                           text = "Le nom du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("nom")
                         shinyjs::enable('submit')
                       }
                     })
                     
                   })
                   
                   
                   
                   edit_patient_dat <- reactive({
                     hold <- patient_to_edit()
                     
                     out <- list(
                       uid = if (is.null(hold))
                         NA
                       else
                         hold$uid,
                       data = list(
                         "nom" = input$nom,
                         "prenom" = input$prenom,
                         "date_naissance" = format(as.Date(input$date_naissance), '%Y-%m-%d'),
                         "condition" = input$condition
                         # "hopital" = input$hopital,
                         # "contact" = input$contact
                       )
                     )
                     
                     time_now <-
                       as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
                     
                     if (is.null(hold)) {
                       # adding a new patient
                       
                       out$data$created_at <- time_now
                       out$data$created_by <- session$userData$email
                       
                     } else {
                       # Editing existing patient
                       
                       out$data$created_at <- as.character(hold$created_at)
                       out$data$created_by <- hold$created_by
                       
                     }
                     
                     out$data$modified_at <- time_now
                     out$data$modified_by <- session$userData$email
                     
                     out
                   })
                   
                   validate_edit <- eventReactive(input$submit, {
                     dat <- edit_patient_dat()
                     
                     # Logic to validate inputs...
                     
                     dat
                   })
                   
                   observeEvent(validate_edit(), {
                     removeModal()
                     dat <- validate_edit()
                     
                     tryCatch({
                       if (is.na(dat$uid)) {
                         # creating a new car
                         uid <- uuid::UUIDgenerate()
                         
                         dbExecute(
                           conn,
                           "INSERT INTO patients (uid, nom, prenom, date_naissance, condition, 
          created_at, created_by, modified_at, modified_by) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
                           params = c(list(uid),
                                      unname(dat$data))
                         )
                       } else {
                         # editing an existing car
                         #print('About to execute')
                         
                         dbExecute(
                           conn,
                           "UPDATE patients SET nom=$1, prenom=$2, date_naissance=$3, condition=$4, 
          created_at=$5, created_by=$6, modified_at=$7, modified_by=$8
          WHERE uid=$9",
                           params = c(unname(dat$data),
                                      list(dat$uid))
                         )
                       }
                       
                       session$userData$patients_trigger(session$userData$patients_trigger() + 1)
                       showToast("success", message = printToastMessage(modal_title))
                     }, error = function(error) {
                       msg <- paste0("Erreur - contactez votre admin")
                       
                       
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       # print the actual error to log it
                       print(error)
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                     })
                   })
                 })
    
    
  }
