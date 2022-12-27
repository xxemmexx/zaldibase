#' Add & Edit Module
#'
#' Module to add & edit dossiers in the patients database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param rendezvous_patient reactive returning a 1 row data frame of the dossier to edit
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
rendezvousEditModuleServer <- function(id, 
                                     modal_title, 
                                     rendezvous_patient, 
                                     modal_trigger) {
    
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 
                 
                 
                 
                   ############# MODAL TRIGGER #################################
                   observeEvent(modal_trigger(), {
                     
                     hold <- rendezvous_patient()
                     
                     docs <- user_base %>%
                       filter(permissions %in% c('chef', 'resident')) %>%
                       transmute(`Médecins` = name)
                     
                     docs <- c(" ", docs)
                     
                     #------------FILL-IN FORM----------------------------------
                     
                     showModal(
                       modalDialog(
                         div(style = "padding: 30px;",
                             fluidRow(
                               
                               HTML(paste0("<h4 style=text-align:center;>Nouveau rendez-vous pour M./Mme. ",
                                           hold$prenom, " ", str_to_upper(hold$nom), "</h4>")),
                               tags$br(),
                               tags$br(),
                               column(width = 6,
                                      dateInput(ns("date_rendezvous"),
                                                'Date (AAAA-MM-DD)',
                                                value = ifelse((str_trim(hold$date_rendezvous) == "" | is.na(hold$date_rendezvous)), 
                                                               as.character(today()+7), 
                                                               hold$date_rendezvous),
                                                language = "fr")),
                               column(width = 3,
                                      numericInput(ns("heure_rendezvous"),
                                                   'Heure',
                                                   value = 14,
                                                   min = 0,
                                                   max = 23,
                                                   step = 1,
                                                   width = '100%')),
                               column(width = 3,
                                      numericInput(ns("min_rendezvous"),
                                                   'Minute',
                                                   value = 15,
                                                   min = 0,
                                                   max = 55,
                                                   step = 5,
                                                   width = '100%'))
                               
                             ), # Close fluidrow
                             fluidRow(column(width = 2),
                                      column(width = 8,
                                             selectInput(ns("rendezvous_avec"),
                                                         "Médecin",
                                                         choices = docs,
                                                         selected = " ")),
                                      column(width = 2)
                                      ) # Close fluidrow
                             ), # Close div
                         title = modal_title,
                         size = 'l',
                         footer = list(modalButton('Annuler'),
                                       actionButton(ns('submit'),
                                                    "Envoyer",
                                                    class = "btn btn-primary mb1 bg-olive")) 
                         ) # Close modal dialog
                       ) # Close showModal
                     
                     #------------END FILL-IN FORM------------------------------
                     
                     #------------FIELD VALIDATION - FEEDBACK-------------------
                     
                     formFields <- reactiveValues(date_rendezvous = 0,
                                                  rendezvous_avec = 0)
                     
                     observeEvent(input$date_rendezvous, {
                       if (length(input$date_rendezvous) < 1) {
                         shinyFeedback::showFeedbackDanger("date_rendezvous",
                                                           text = "Choisissez une date pour le rendez-vous")
                         shinyjs::disable('submit')
                       } else if (ymd(input$date_rendezvous) < today()) {
                         shinyFeedback::showFeedbackDanger("date_rendezvous",
                                                           text = "Choisissez une date dans l'avenir")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("date_rendezvous")
                         formFields$date_rendezvous = 1
                       }
                     })
                     
                     observeEvent(input$rendezvous_avec, {
                       if (str_trim(input$rendezvous_avec) == "") {
                         shinyFeedback::showFeedbackDanger("rendezvous_avec",
                                                           text = "Choisissez un médecin")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("rendezvous_avec")
                         formFields$rendezvous_avec = 1
                       }
                     })
                     
                     observe({
                       
                       if(formFields$date_rendezvous == 1 &
                          formFields$rendezvous_avec == 1) {
                         shinyjs::enable('submit')
                       }
                     })
                     
                   }) # Close modal trigger
                 
                 ############# END MODAL TRIGGER ###############################
                 
                 
                 ################# CAPTURE DATA ################################
                 
                 rendez_vous_dat <- reactive({
                   
                   hold <- rendezvous_patient()
                   
                   #time_now <- Sys.time() %>% ymd_hms()
                   
                   out <- list(uid = deliverUID(hold),
                               data = list("date_rendezvous" = input$date_rendezvous,
                                           "time_rendezvous" = deliverTimeString(input$heure_rendezvous, input$min_rendezvous),
                                           "rendezvous_avec" = input$rendezvous_avec,
                                           "nom" = hold$nom,
                                           "prenom" = hold$prenom,
                                           "date_naissance" = hold$date_naissance,
                                           "staff_decision" = hold$staff_decision,
                                           "explication" = hold$explication,
                                           "contact_email" = hold$contact_email))
              
                   out
                   })
                 
                 ############# END CAPTURE DATA ################################
                 
                 
                 ############# SUBMIT ACTION: VALIDATE DATA ####################
                 
                 validate_edit <- eventReactive(input$submit, {
                   dat <- rendez_vous_dat()
                     
                     # Logic to validate inputs...
                     
                     dat
                   })
                 
                 ############# END VALIDATE DATA ###############################
                 
                 
                 ############# TALK TO DB ######################################
                 
                 observeEvent(validate_edit(), {
                   
                   removeModal()
                   
                   dat <- validate_edit()
                   
                   tryCatch({
                     
                     showModal(patientezDialog)
                     
                     query <- writeRendezVousDetailsQuery(dat$data$date_rendezvous,
                                                          dat$data$time_rendezvous,
                                                          dat$data$rendezvous_avec,
                                                          dat$uid)
                     
                     print('Trying to execute query...')
                     
                     dbExecute(conn, query)
                     
                     nomCompletPatient <- paste0(dat$data$prenom, " ", str_to_upper(dat$data$nom))

                     print('Trying to send notification email...')

                     generateRendezvousEmail(nomCompletPatient,
                                             dat$data$date_naissance,
                                             dat$data$staff_decision,
                                             dat$data$date_rendezvous,
                                             dat$data$time_rendezvous,
                                             dat$data$rendezvous_avec,
                                             dat$data$explication) %>%
                       smtp_send(
                         to = dat$data$contact_email,
                         from = zaldibase,
                         subject = "Nouvelle notification CHU - équipue du neurochirurgical",
                         credentials = creds_file(credentialsPath)
                       )
                     
                     session$userData$rendezvous_trigger(session$userData$rendezvous_trigger() + 1)
                     
                     removeModal()
                     
                     showToast("success", message = "Rendez-vous enregistré")}, 
                     
                     error = function(error) {
                       
                       msg <- paste0("Erreur en faisant un rendez-vous")
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       # print the actual error to log it
                       print(error)
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                     }
                   ) # Close try-catch
                     
                   
                   
                   })
                 
                 ############# END TALK TO DB, SUBMIT ACTION ###################
                 

                 
                 }) # Close module server
  
  } # End dossiersEditModuleServer
