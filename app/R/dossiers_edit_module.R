

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
dossiersEditModuleServer <-
  function(id,
           modal_title,
           dossier_to_edit,
           modal_trigger) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   
                   observeEvent(modal_trigger(), {
                     hold <- dossier_to_edit()
                     
                     showModal(modalDialog(
                       div(
                         style = "padding: 30px;",
                       fluidRow(
                         HTML("<h5><em>Données du centre hospitalier d'origine</em></h4>"),
                         column(
                           width = 6,
                           selectInput(ns("hopital"),
                                       "Centre Hospitalier d'origine",
                                       choices = c("CHU", "CHAM"),
                                       selected = ifelse(is.null(hold), "", hold$hopital))
                         ),
                         column(
                           width = 6,
                           selectInput(
                             ns('contact'),
                             "Personne de contact",
                             choices = c('Cassandra Gotsi', 'Albert Malfait'),
                             selected = ifelse(is.null(hold), "", hold$contact))
                         ),
                         HTML("<h5><em>Données du patient</em></h5>"),
                           fluidRow(
                             column(
                             width = 6,
                             textInput(
                               ns("nom"),
                               'Nom',
                               value = ifelse(is.null(hold), "", hold$nom)
                             ),
                            
                             dateInput(
                               ns("date_naissance"),
                               'Date de naissance',
                               value = ifelse(is.null(hold), "", hold$date_naissance),
                               language = "fr"
                             )
                           ),
                         column(
                           width = 6,
                           textInput(
                             ns("prenom"),
                             'Prénom',
                             value = ifelse(is.null(hold), "", hold$prenom)
                           ),
                           textInput(
                             ns("phone_number_patient"),
                             'Numéro de téléphone',
                             value = ifelse(is.null(hold), "", hold$phone_number_patient)
                           )
                           )
                         ),
                         fluidRow(
                           column(
                             width = 6,
                             selectInput(
                               ns('pathologie'),
                               'Pathologie',
                               choices = pathologies,
                               selected = ifelse(is.null(hold), "", hold$pathologie)
                             ),
                             conditionalPanel(
                               "input.pathologie == 'Autre...'",
                               textAreaInput(ns('description'),
                                             'Description',
                                             placeholder = "Decrivez..."),
                               ns = ns
                             )
                           ),
                           column(
                             width = 6,
                             selectInput(
                               ns('pre_decision'),
                               "Décision préliminaire",
                               choices = decisions,
                               selected = ifelse(is.null(hold), "", hold$pre_decision)
                             ),
                             conditionalPanel(
                               "input.pre_decision !== ' '",
                               textAreaInput(ns('explanation'),
                                             'Explication',
                                             placeholder = "Expliquez votre avis..."),
                               ns = ns
                             )
                           )
                         
                         ), # Close fluidRow
                         fluidRow(
                           column(
                             width = 12,
                             textAreaInput(ns('histoire'),
                                           'Histoire',
                                           placeholder = "Décrivez...",
                                           width = '740'),
                           )
                         )
                           
                           
                       ) # Close fluidrow
                       ), # Close div
                       
                       title = modal_title,
                       size = 'l',
                       footer = list(
                         modalButton('Annuler'),
                         actionButton(
                           ns('submit'),
                           printButtonLabel(modal_title),
                           class = "btn btn-primary mb1 bg-olive",
                           style = "color: white")
                       ) # Close list in footer
                     ) # Close modal dialog
                     ) # Close showModal
                   
                           
                        
                     
                     # Observe event for "Nom" text input in Add/Edit Dossier
                     # `shinyFeedback`
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
                     
                   })
                   
                   
                   
                   edit_dossier_dat <- reactive({
                     hold <- dossier_to_edit()
                     
                     out <- list(
                       uid = if (is.null(hold))
                         NA
                       else
                         hold$uid,
                       data = list(
                         "nom" = input$nom,
                         "prenom" = input$prenom,
                         "date_naissance" = format(as.Date(input$date_naissance), '%Y-%m-%d'),
                         "pathologie" = input$pathologie,
                         "pre_decision" = input$pre_decision
                       )
                     )
                     
                     time_now <-
                       as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
                     
                     if (is.null(hold)) {
                       # adding a new dossier
                       
                       out$data$created_at <- time_now
                       out$data$created_by <- session$userData$email
                       
                     } else {
                       # Editing existing dossier
                       
                       out$data$created_at <- as.character(hold$created_at)
                       out$data$created_by <- hold$created_by
                       
                     }
                     
                     out$data$modified_at <- time_now
                     out$data$modified_by <- session$userData$email
                     
                     out
                   })
                   
                   validate_edit <- eventReactive(input$submit, {
                     dat <- edit_dossier_dat()
                     
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
                           "INSERT INTO patients (uid, nom, prenom, date_naissance, 
                           pathologie, pre_decision, created_at, 
                           created_by, modified_at, modified_by) VALUES
                           ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
                           params = c(list(uid),
                                      unname(dat$data))
                         )
                       } else {
                         # editing an existing dossier
                         #print('About to execute')
                         
                         dbExecute(
                           conn,
                           "UPDATE patients SET nom=$1, prenom=$2, date_naissance=$3, 
                           pathologie=$4, pre_decision=$5,
                           created_at=$6, created_by=$7, modified_at=$8, modified_by=$9
                           WHERE uid=$10",
                           params = c(unname(dat$data),
                                      list(dat$uid))
                         )
                       }
                       
                       session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
                       
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
