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
#' @param dossier_to_edit reactive returning a 1 row data frame of the dossier to edit
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
dossiersEditModuleServer <- function(id, 
                                     modal_title, dossier_to_edit, modal_trigger) {
    
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                   ############# MODAL TRIGGER #################################
                   observeEvent(modal_trigger(), {
                     
                     hold <- dossier_to_edit()
                     
                     #------------FILL-IN FORM----------------------------------
                     
                     showModal(
                       modalDialog(
                         div(style = "padding: 30px;",
                             fluidRow(HTML("<h5><em>Données du centre hospitalier d'origine</em></h4>"),
                                      column(width = 6
                                             # selectInput(ns("hopital"),
                                             #             "Centre Hospitalier d'origine",
                                             #             choices = c("CHU", "CHAM"),
                                             #             selected = ifelse(is.null(hold), "", hold$hopital))
                                      ),
                                      column(width = 6
                                             # selectInput(
                                             #   ns('contact'),
                                             #   "Personne de contact",
                                             #   choices = c('Cassandra Gotsi', 'Albert Malfait'),
                                             #   selected = ifelse(is.null(hold), "", hold$contact))
                                      ),
                                      HTML("<h5><em>Données du patient</em></h5>"),
                                      fluidRow(column(width = 6,
                                                      textInput(ns("nom"),
                                                                'Nom',
                                                                value = ifelse(is.null(hold), "", hold$nom)),
                                                      dateInput(ns("date_naissance"),
                                                                'Date de naissance (YYYY-MM-JJ)',
                                                                value = ifelse(is.null(hold), "", hold$date_naissance),
                                                                language = "fr")),
                                               column(width = 6,
                                                      textInput(ns("prenom"),
                                                                'Prénom',
                                                                value = ifelse(is.null(hold), "", hold$prenom)),
                                                      textInput(ns("phone_number_patient"),
                                                                'Numéro de téléphone',
                                                                value = ifelse(is.null(hold), "", hold$phone_number_patient)))
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      selectInput(ns('pathologie'),
                                                                  'Pathologie',
                                                                  choices = pathologies,
                                                                  selected = ifelse(is.null(hold), "", hold$pathologie)),
                                                      conditionalPanel("input.pathologie == 'Autre...'",
                                                                       textAreaInput(ns('description'),
                                                                                     'Description',
                                                                                     placeholder = "Decrivez..."),
                                                                       ns = ns))
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      textAreaInput(ns('histoire'),
                                                                    'Histoire',
                                                                    placeholder = "Décrivez...",
                                                                    width = '740'))
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      selectInput(ns('pre_decision'),
                                                                  "Décision préliminaire",
                                                                  choices = decisions,
                                                                  selected = ifelse(is.null(hold), "", hold$pre_decision)),
                                                      conditionalPanel("input.pre_decision !== ' '",
                                                                       textAreaInput(ns('explanation'),
                                                                                     'Explication',
                                                                                     placeholder = "Expliquez cette décision..."),
                                                                       ns = ns))
                                               ) # Close fluidRow
                                      ) # Close fluidrow
                             ), # Close div
                         title = modal_title,
                         size = 'l',
                         footer = list(modalButton('Annuler'),
                                       actionButton(ns('submit'),
                                                    printButtonLabel(modal_title),
                                                    class = "btn btn-primary mb1 bg-olive",
                                                    style = "color: white")) 
                         ) # Close modal dialog
                       ) # Close showModal
                     
                     #------------END FILL-IN FORM------------------------------
                     
                     #------------FIELD VALIDATION - FEEDBACK-------------------
                     
                     
                     observeEvent(input$contact, {
                       if (input$contact == "") {
                         shinyFeedback::showFeedbackDanger("contact",
                                                           text = "La personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$hopital, {
                       if (input$hopital == "") {
                         shinyFeedback::showFeedbackDanger("hopital",
                                                           text = "L'hôpital d'origine est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("hopital")
                         shinyjs::enable('submit')
                       }
                     })
                     
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
                     
                     observeEvent(input$prenom, {
                       if (input$prenom == "") {
                         shinyFeedback::showFeedbackDanger("prenom",
                                                           text = "Le prénom du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("prenom")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$date_naissance, {
                       if (length(input$date_naissance) < 1) {
                         shinyFeedback::showFeedbackDanger("date_naissance",
                                                           text = "Le date de naissance du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("date_naissance")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$phone_number_patient, {
                       if (input$phone_number_patient == "") {
                         shinyFeedback::showFeedbackDanger("phone_number_patient",
                                                           text = "Le numéro de téléphone est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("phone_number_patient")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$pathologie, {
                       if (input$pathologie == 'Autre...' ) {
                         
                         observeEvent(input$description, {
                           
                           if(input$description == "") {
                             shinyFeedback::showFeedbackDanger("description",
                                                               text = "Une description est obligatoire!")
                             shinyjs::disable('submit')
                           } else {
                             shinyFeedback::hideFeedback("description")
                             shinyjs::enable('submit')
                           }
                         }) # Close description observer
                       } # Close if statement pathologie == 'Autre'
                     }) # Close pathologie observer
                     
                     #------------END FIELD VALIDATION - FEEDBACK---------------
                     
                   }) # Close modal trigger
                 
                 ############# END MODAL TRIGGER ###############################
                 
                 
                 ################# CAPTURE DATA ################################
                 
                 edit_dossier_dat <- reactive({
                   
                   hold <- dossier_to_edit()
                   
                   time_now <- Sys.time() %>% ymd_hms()
                   
                   out <- list(uid = deliverUID(hold),
                               data = list("nom" = input$nom,
                                           "prenom" = input$prenom,
                                           "date_naissance" = writeISODate(input$date_naissance),
                                           "phone_number_patient" = input$phone_number_patient,
                                           "pathologie" = input$pathologie,
                                           "pre_decision" = input$pre_decision,
                                           "created_at" = deliverCreationTime(hold, time_now),
                                           "created_by" = deliverCreator(hold, session$userData$username()),
                                           "modified_at" = time_now,
                                           "modified_by" = session$userData$username()))
              
                   out
                   })
                 
                 ############# END CAPTURE DATA ################################
                 
                 
                 ############# VALIDATE DATA ###################################
                 
                 validate_edit <- eventReactive(input$submit, {
                   dat <- edit_dossier_dat()
                     
                     # Logic to validate inputs...
                     
                     dat
                   })
                 
                 ############# END VALIDATE DATA ###############################
                 
                 
                 ############# TALK TO DB ######################################
                 
                 observeEvent(validate_edit(), {
                   
                   removeModal()
                   
                   dat <- validate_edit()
                   
                   tryCatch({
                     
                     if (is.na(dat$uid)) {
                       
                       uid <- generateIdentifier(dat$data$prenom, dat$data$nom) #uuid::UUIDgenerate()
                       statement = "insert"
                       
                       } else {
                         
                         uid <- dat$uid
                         statement = "update"
                         
                       }
                     
                     thisQuery <- writeQuery(uid, dat$data$nom, dat$data$prenom,
                                             dat$data$date_naissance, dat$data$phone_number_patient,
                                             dat$data$pathologie,
                                             dat$data$pre_decision, dat$data$created_at, 
                                             dat$data$created_by, dat$data$modified_at, 
                                             dat$data$modified_by, aStatement = statement)
                     
                     dbExecute(conn, thisQuery)
                     
                     session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
                     
                     showToast("success", message = printToastMessage(modal_title))}, 
                     
                     error = function(error) {
                       
                       msg <- paste0("Erreur - contactez votre admin")
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
                 
                 ############# END TALK TO DB ##################################
                 
                 }) # Close module server
  
  } # End dossiersEditModuleServer
