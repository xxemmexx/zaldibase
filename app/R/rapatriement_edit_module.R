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
rapatriementEditModuleServer <- function(id, 
                                     modal_title, 
                                     rapatriement_patient, 
                                     modal_trigger) {
    
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                   ############# MODAL TRIGGER #################################
                   observeEvent(modal_trigger(), {
                     
                     hold <- rapatriement_patient()
                     
                     # docs <- user_base %>%
                     #   filter(permissions %in% c('chef', 'resident')) %>%
                     #   transmute(`Médecins` = name)
                     # 
                     # docs <- c(" ", docs)
                     
                     #------------FILL-IN FORM----------------------------------
                     
                     showModal(
                       modalDialog(
                         div(style = "padding: 30px;",
                             fluidRow(
                               
                               HTML(paste0("<h4 style=text-align:center;>Hospitalisation pour M./Mme. ",
                                           hold$prenom, " ", str_to_upper(hold$nom), "</h4>")),
                               tags$br(),
                               tags$br(),
                               column(width = 6,
                                      dateInput(ns("date_rapatriement"),
                                                'Date (AAAA-MM-DD)',
                                                value = ifelse((str_trim(hold$date_rapatriement) == "1970-01-01" | is.na(hold$date_rapatriement)), 
                                                               as.character(today()+7), 
                                                               hold$date_rapatriement),
                                                language = "fr")),
                               column(width = 3,
                                      numericInput(ns("heure_rapatriement"),
                                                   'Heure',
                                                   value = 14,
                                                   min = 0,
                                                   max = 23,
                                                   step = 1,
                                                   width = '100%')),
                               column(width = 3,
                                      numericInput(ns("min_rapatriement"),
                                                   'Minute',
                                                   value = 15,
                                                   min = 0,
                                                   max = 55,
                                                   step = 5,
                                                   width = '100%'))
                               
                             ), # Close fluidrow
                             fluidRow(column(width = 2),
                                      column(width = 8,
                                             textInput(ns("room_id"),
                                                         "Chambre",
                                                         placeholder = "Chambre 2.17")),
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
                     
                     formFields <- reactiveValues(date_rapatriement = 1,
                                                  room_id = 0)
                    
                     
                     observeEvent(input$date_rapatriement, {
                       if (isInvalidDate(input$date_rapatriement)) {
                         formFields$date_rapatriement = 0
                         shinyFeedback::showFeedbackDanger("date_rapatriement",
                                                           text = "Chosissez une date (dans l'avenir)")
                         shinyjs::disable('submit')
                       } else {
                         formFields$date_rapatriement = 1
                         shinyFeedback::hideFeedback("date_rapatriement")
                       }
                     })

                     observeEvent(input$room_id, {
                       if (str_trim(input$room_id) == "") {
                         formFields$room_id = 0
                         shinyFeedback::showFeedbackDanger("room_id",
                                                           text = "Choisissez une chambre")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("room_id")
                         formFields$room_id = 1
                       }
                     })
                     
                     observe({
                       if(formFields$date_rapatriement == 1 &
                          formFields$room_id == 1) {
                         shinyjs::enable('submit')
                       }
                     })
                     
                   }) # Close modal trigger
                 
                 ############# END MODAL TRIGGER ###############################
                 
                 
                 ################# CAPTURE DATA ################################
                 
                 rapatriement_dat <- reactive({
                   
                   hold <- rapatriement_patient()
                   
                   #time_now <- Sys.time() %>% ymd_hms()
                   
                   out <- list(uid = deliverUID(hold),
                               data = list("date_rapatriement" = input$date_rapatriement,
                                           "time_rapatriement" = deliverTimeString(input$heure_rapatriement, input$min_rapatriement),
                                           "room_id" = input$room_id,
                                           "nom" = hold$nom,
                                           "prenom" = hold$prenom,
                                           "email" = hold$email_patient,
                                           "date_naissance" = hold$date_naissance,
                                           "staff_decision" = hold$staff_decision,
                                           "explication" = hold$explication,
                                           "contact_email" = hold$contact_email))
              
                   out
                   })
                 
                 ############# END CAPTURE DATA ################################
                 
                 
                 ############# SUBMIT ACTION: VALIDATE DATA ####################
                 
                 validate_edit <- eventReactive(input$submit, {
                   dat <- rapatriement_dat()
                     
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
                     
                     query <- writeRapatriementDetailsQuery(dat$data$date_rapatriement,
                                                          dat$data$time_rapatriement,
                                                          dat$data$room_id,
                                                          dat$uid)
                     
                     print('Trying to execute query...')
                     
                     dbExecute(conn, query)
                     
                     nomCompletPatient <- paste0(dat$data$prenom, " ", str_to_upper(dat$data$nom))

                     print('Trying to send notification email to contact person...')

                     generateRapatriementEmail(nomCompletPatient,
                                             dat$data$date_naissance,
                                             dat$data$staff_decision,
                                             dat$data$date_rapatriement,
                                             dat$data$time_rapatriement,
                                             dat$data$room_id,
                                             dat$data$explication) %>%
                       smtp_send(
                         to = dat$data$contact_email,
                         from = zaldibase,
                         subject = "Nouvelle notification CHU - équipue du neurochirurgical",
                         credentials = creds_file(credentialsPath)
                       )
                     
                     if(!(dat$data$email == '')) {
                       print('Trying to send notification email to patient...')
                       
                       generateRapatriementEmailForPatient(dat$data$date_rapatriement,
                                                         dat$data$time_rapatriement,
                                                         dat$data$room_id,
                                                         dat$data$explication) %>%
                         smtp_send(
                           to = dat$data$email,
                           from = zaldibase,
                           subject = "Nouvelle notification CHU - équipue du neurochirurgical",
                           credentials = creds_file(credentialsPath)
                         )
                     }
                     
                     session$userData$rapatriement_trigger(session$userData$rapatriement_trigger() + 1)
                     
                     removeModal()
                     
                     showToast("success", message = "Hospitalisation enregistrée")}, 
                     
                     error = function(error) {
                       
                       msg <- paste0("Erreur en faisant un rapatriement")
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
