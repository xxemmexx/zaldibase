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
#' @param externe_patient reactive returning a 1 row data frame of the dossier to edit
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
externesEditModuleServer <- function(id, 
                                     modal_title, 
                                     externe_patient, 
                                     modal_trigger) {
    
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                   ############# MODAL TRIGGER #################################
                   observeEvent(modal_trigger(), {
                     
                     hold <- externe_patient()
                     
                     
                     #------------FILL-IN FORM----------------------------------
                     
                     showModal(
                       modalDialog(
                         div(style = "padding: 30px;",
                             fluidRow(style = "background-color:#DBDFE3;",
                                      
                                      
                                      HTML("<h4 style=text-align:center;><b>Origine de l'appel</b></h4>"),
                                      column(width = 6,
                                             textInput(ns('contact_person'),
                                                       "Personne de contact",
                                                       value = ifelse(is.null(hold), "", hold$contact_person)),
                                             selectInput(ns("hopital"),
                                                         "Centre Hospitalier d'origine",
                                                         choices = getAffiliation(session$userData$username(), user_base))
                                             
                                      ),
                                      column(width = 6,
                                             textInput(ns('contact_phone'),
                                                       "Numéro de téléphone du contact",
                                                       value = ifelse(is.null(hold), "", hold$contact_phone)),
                                             textInput(ns('contact_email'),
                                                       "Email",
                                                       value = ifelse(is.null(hold), "", hold$contact_email)))
                             ), # Close fluidrow
                             tags$br(),
                             fluidRow(HTML("<h4 style=text-align:center;><b>Données du patient</b></h4>"),
                                      fluidRow(column(width = 6, align="center",
                                                      radioButtons(ns('sex'), 
                                                                   HTML('<b>Sexe</b>'), 
                                                                   choices = c("H" = 0, "F" = 1),
                                                                   inline = TRUE,
                                                                   selected = ifelse(is.null(hold), "H", hold$sexe)),
                                                      textInput(ns("nom"),
                                                                'Nom',
                                                                value = ifelse(is.null(hold), "", hold$nom)),
                                                      textInput(ns("prenom"),
                                                                'Prénom',
                                                                value = ifelse(is.null(hold), "", hold$prenom))
                                      ), #close column
                                      column(width = 6,
                                             tags$br(),
                                             tags$br(),
                                             tags$br(),
                                             dateInput(ns("date_naissance"),
                                                       'Date de naissance (AAAA-MM-JJ)',
                                                       value = ifelse(is.null(hold), "", hold$date_naissance),
                                                       language = "fr"),
                                             textInput(ns("phone_number_patient"),
                                                       'Numéro de téléphone du patient',
                                                       value = ifelse(is.null(hold), "", hold$phone_number_patient))
                                      ) # close column
                                      ) # Close fluidRow
                             ), # Close fluid row
                                      fluidRow(HTML("<h4 style=text-align:center;><b>Contexte de la maladie</b></h4>"),
                                               column(width = 12, 
                                                      HTML('<b>Le patient, prend-il des fluidifiants adressant des troubles de coagulation?</b>'),
                                                      radioButtons(ns('add_coagulation'), 
                                                                   "", 
                                                                   choices = c("Non" = 0, "Oui" = 1),
                                                                   inline = FALSE,
                                                                   selected = ifelse(is.null(hold), 0, hold$has_coagulation))
                                               ) # Close column
                                      ), # Close fluidRow
                                      conditionalPanel("input.add_coagulation == 1",
                                                       tags$br(),
                                                       fluidRow(column(width = 12,
                                                                       HTML('<b>Indiquez quels médicaments ont été administrés ainsi que la date de leur dernière prise</b>'))),
                                                       tags$br(),
                                                       fluidRow(
                                                         column(width = 4,
                                                                selectInput(ns('traitement_1'),
                                                                            'Médicament 1',
                                                                            choices = medications,
                                                                            selected = ifelse(is.null(hold), "", hold$treat_coagulant_1)),
                                                                dateInput(ns("date_derniere_prise_1"),
                                                                          'Dernière prise',
                                                                          value = ifelse(is.null(hold), "", hold$date_derniere_prise_1),
                                                                          language = "fr")),
                                                         column(width = 4,
                                                                selectInput(ns('traitement_2'),
                                                                            'Médicament 2',
                                                                            choices = medications,
                                                                            selected = ifelse(is.null(hold), "", hold$treat_coagulant_2)),
                                                                dateInput(ns("date_derniere_prise_2"),
                                                                          'Dernière prise',
                                                                          value = ifelse(is.null(hold), "", hold$date_derniere_prise_2),
                                                                          language = "fr")),
                                                         column(width = 4,
                                                                selectInput(ns('traitement_3'),
                                                                            'Médicament 3',
                                                                            choices = medications,
                                                                            selected = ifelse(is.null(hold), "", hold$treat_coagulant_3)),
                                                                dateInput(ns("date_derniere_prise_3"),
                                                                          'Dernière prise',
                                                                          value = ifelse(is.null(hold), "", hold$date_derniere_prise_3),
                                                                          language = "fr"))
                                                       ), #close fluidRow
                                                       ns = ns),
                                      
                             fluidRow(column(width = 12, align="center",
                                             selectInput(ns('pathologie_1'),
                                                         'Pathologie',
                                                         choices = pathologies,
                                                         selected = ifelse(is.null(hold), "", hold$pathologie_1),
                                                         width = '66%'),
                                             conditionalPanel("input.pathologie_1 == 'Autre...'",
                                                              textAreaInput(ns('description_pathologie_1'),
                                                                            'Description',
                                                                            value = ifelse(is.null(hold), "", hold$pathologie_1),
                                                                            placeholder = "Decrivez...",
                                                                            width = '66%'),
                                                              ns = ns),
                                             tags$br(),
                                             HTML("<b>Le patient, subit-il d'une deuxième pathologie?</b>"),
                                             radioButtons(ns('add_pathologie_2'), 
                                                          "", 
                                                          choices = c("Non" = 0, "Oui" = 1),
                                                          inline = TRUE,
                                                          selected = ifelse(str_trim(hold$pathologie_2) == "", 0, 1)),
                                             conditionalPanel("input.add_pathologie_2 == 1",
                                                              selectInput(ns('pathologie_2'),
                                                                          'Pathologie',
                                                                          choices = pathologies,
                                                                          selected = ifelse(str_trim(hold$pathologie_2) == "", "", hold$pathologie_2),
                                                                          width = '66%'),
                                                              conditionalPanel("input.pathologie_2 == 'Autre...'",
                                                                               textAreaInput(ns('description_pathologie_2'),
                                                                                             'Description',
                                                                                             placeholder = "Decrivez...",
                                                                                             value = ifelse(str_trim(hold$pathologie_2) == "", "", hold$pathologie_2),
                                                                                             width = '66%'),
                                                                               ns = ns),
                                                              HTML("<b>Le patient, subit-il d'une troisième pathologie?</b>"),
                                                              radioButtons(ns('add_pathologie_3'), 
                                                                           "", 
                                                                           choices = c("Non" = 0, "Oui" = 1),
                                                                           inline = TRUE,
                                                                           selected = ifelse(str_trim(hold$pathologie_3) == "", 0, 1)),
                                                              conditionalPanel("input.add_pathologie_3 == 1",
                                                                               selectInput(ns('pathologie_3'),
                                                                                           'Pathologie',
                                                                                           choices = pathologies,
                                                                                           selected = ifelse(str_trim(hold$pathologie_3) == "", "", hold$pathologie_3),
                                                                                           width = '66%'),
                                                                               conditionalPanel("input.pathologie_3 == 'Autre...'",
                                                                                                textAreaInput(ns('description_pathologie_3'),
                                                                                                              'Description',
                                                                                                              placeholder = "Decrivez...",
                                                                                                              value = ifelse(str_trim(hold$pathologie_3) == "", "", hold$pathologie_3),
                                                                                                              width = '66%'),
                                                                                                ns = ns),
                                                                               ns = ns),
                                                              ns = ns)
                             ) # Close column
                             ), # Close fluidRow
                                      tags$br(),
                                      fluidRow(column(width = 12, align="center",
                                                      textAreaInput(ns('description_histoire'),
                                                                    'Histoire de la maladie',
                                                                    placeholder = "Décrivez...",
                                                                    value = ifelse(is.null(hold), "", hold$description_histoire),
                                                                    width = '100%',
                                                                    height = '100px'))
                                      ), # Close fluidRow
                                      fluidRow(column(width = 12, align="center",
                                                      fileInput(ns('photos'),
                                                                "Ajouter des images",
                                                                multiple = TRUE,
                                                                accept = 'image/*',
                                                                buttonLabel = "Parcourir...",
                                                                placeholder = "...ou placez fichier ici",
                                                                width = '66%')
                                                      
                                                      
                                      ) # close column
                                      ) # Close fluid row
                             ), # Close div
                         title = modal_title,
                         size = 'l',
                         footer = list(modalButton('Annuler'),
                                       actionButton(ns('submit'),
                                                    "Enregistrer",
                                                    class = "btn btn-primary mb1 bg-olive")) 
                         ) # Close modal dialog
                       ) # Close showModal
                     
                     #------------END FILL-IN FORM------------------------------
                     
                     #------------FIELD VALIDATION - FEEDBACK-------------------
                     
                     formFields <- reactiveValues(contact_person = 0,
                                                  contact_phone = 0,
                                                  contact_email = 0,
                                                  nom = 0,
                                                  prenom = 0,
                                                  date_naissance = 0,
                                                  phone_number_patient = 0,
                                                  pathologie_1 = 0,
                                                  description_histoire = 0)
                     
                     
                     observeEvent(input$contact_person, {
                       if (input$contact_person == "") {
                         shinyFeedback::showFeedbackDanger("contact_person",
                                                           text = "La personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_person")
                         formFields$contact_person = 1
                       }
                     })
                     
                     observeEvent(input$contact_phone, {
                       if (input$contact_phone == "") {
                         shinyFeedback::showFeedbackDanger("contact_phone",
                                                           text = "Le numéro de téléphone du contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_phone")
                         formFields$contact_phone = 1
                       }
                     })
                     
                     observeEvent(input$contact_email, {
                       if (str_trim(input$contact_email == "")) {
                         shinyFeedback::showFeedbackDanger("contact_email",
                                                           text = "L'email de la personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_email")
                         formFields$contact_email = 1
                       }
                     })
                     
                     observeEvent(input$nom, {
                       if (input$nom == "") {
                         shinyFeedback::showFeedbackDanger("nom",
                                                           text = "Le nom du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("nom")
                         formFields$nom = 1
                       }
                     })
                     
                     observeEvent(input$prenom, {
                       if (input$prenom == "") {
                         shinyFeedback::showFeedbackDanger("prenom",
                                                           text = "Le prénom du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("prenom")
                         formFields$prenom = 1
                       }
                     })
                     
                     observeEvent(input$date_naissance, {
                       if (length(input$date_naissance) < 1) {
                         shinyFeedback::showFeedbackDanger("date_naissance",
                                                           text = "Le date de naissance du patient est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("date_naissance")
                         formFields$date_naissance = 1
                       }
                     })
                     
                     observeEvent(input$phone_number_patient, {
                       if (str_trim(input$phone_number_patient) == "") {
                         shinyFeedback::showFeedbackDanger("phone_number_patient",
                                                           text = "Le numéro de téléphone est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("phone_number_patient")
                         formFields$phone_number_patient = 1
                       }
                     })
                     
                     observeEvent(
                       eventExpr = {
                         input$traitement_1
                         input$date_derniere_prise_1
                       }, 
                       handlerExpr = {
                         if (!(str_trim(input$traitement_1) == "") & length(input$date_derniere_prise_1) < 1) {
                           shinyFeedback::showFeedbackDanger("date_derniere_prise_1",
                                                             text = "Fournissez une date!")
                         } else {
                           shinyFeedback::hideFeedback("date_derniere_prise_1")
                           
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$traitement_2
                         input$date_derniere_prise_2
                       }, 
                       handlerExpr = {
                         if (!(str_trim(input$traitement_2) == "") & length(input$date_derniere_prise_2) < 1) {
                           shinyFeedback::showFeedbackDanger("date_derniere_prise_2",
                                                             text = "Fournissez une date!")
                         } else {
                           shinyFeedback::hideFeedback("date_derniere_prise_2")
                           
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$traitement_3
                         input$date_derniere_prise_3
                       }, 
                       handlerExpr = {
                         if (!(str_trim(input$traitement_3) == "") & length(input$date_derniere_prise_3) < 1) {
                           shinyFeedback::showFeedbackDanger("date_derniere_prise_3",
                                                             text = "Fournissez une date!")
                         } else {
                           shinyFeedback::hideFeedback("date_derniere_prise_3")
                           
                         }
                       }
                     )
                     
                     observeEvent(input$pathologie_1, {
                       if (str_trim(input$pathologie_1) == "") {
                         shinyFeedback::showFeedbackDanger("pathologie_1",
                                                           text = "La pathologie est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("pathologie_1")
                         formFields$pathologie_1 = 1
                       }
                     })
                     
                     observeEvent(input$pathologie_1, {
                       if (input$pathologie_1 == 'Autre...' ) {
                         
                         observeEvent(input$description_pathologie_1, {
                           
                           if(input$description_pathologie_1 == "") {
                             shinyFeedback::showFeedbackDanger("description_pathologie_1",
                                                               text = "Une description est obligatoire!")
                             shinyjs::disable('submit')
                           } else {
                             shinyFeedback::hideFeedback("description_pathologie_1")
                             
                           }
                         }) # Close description observer
                       } # Close if statement pathologie == 'Autre'
                     }) # Close pathologie observer
                     
                     observeEvent(input$description_histoire, {
                       if (str_trim(input$description_histoire) == "") {
                         shinyFeedback::showFeedbackDanger("description_histoire",
                                                           text = "L'histoire de la maladie est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("description_histoire")
                         formFields$description_histoire = 1
                       }
                     })
                     
                     
                     observe({
                       
                       if(formFields$contact_person == 1 &
                          formFields$contact_phone == 1 &
                          formFields$contact_email == 1 &
                          formFields$nom == 1 &
                          formFields$prenom == 1 &
                          formFields$date_naissance == 1 &
                          formFields$phone_number_patient == 1 &
                          formFields$pathologie_1 == 1 & 
                          formFields$description_histoire == 1) {
                         shinyjs::enable('submit')
                       }
                     })
                     
                     
                     #------------END FIELD VALIDATION - FEEDBACK---------------
                     
                   }) # Close modal trigger
                 
                 ############# END MODAL TRIGGER ###############################
                 
                 
                 ################# CAPTURE DATA ################################
                 
                 externes_dat <- reactive({
                   
                   hold <- externe_patient()
                   
                   time_now <- Sys.time() %>% ymd_hms()
                   
                   out <- list(uid = deliverUID(hold),
                               data = list("nom" = input$nom,
                                           "prenom" = input$prenom,
                                           "sexe" = input$sex,
                                           "date_naissance" = writeISODate(input$date_naissance),
                                           "phone_number_patient" = input$phone_number_patient,
                                           "pathologie_1" = deliverStandardOrCustom(input$pathologie_1, input$description_pathologie_1, 1),
                                           "pathologie_2" = deliverStandardOrCustom(input$pathologie_2, input$description_pathologie_2, input$add_pathologie_2),
                                           "pathologie_3" = deliverStandardOrCustom(input$pathologie_3, input$description_pathologie_3, input$add_pathologie_3),
                                           "description_histoire" = input$description_histoire,
                                           "contact_person" = input$contact_person,
                                           "contact_phone" = input$contact_phone,
                                           "contact_email" = input$contact_email,
                                           "hopital" = input$hopital,
                                           "created_at" = deliverCreationTime(hold, time_now),
                                           "created_by" = deliverCreator(hold, session$userData$username()),
                                           "modified_at" = time_now,
                                           "modified_by" = session$userData$username(),
                                           "status" = hold$status
                               )
                   )
                   
                   out
                   })
                 
                 ############# END CAPTURE DATA ################################
                 
                 
                 ############# SUBMIT ACTION: VALIDATE DATA ####################
                 
                 validate_edit <- eventReactive(input$submit, {
                   dat <- externes_dat()
                     
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
                       
                       uid <- generateIdentifier(dat$data$prenom, dat$data$nom) 
                       statement = "insert"
                       
                     } else {
                       
                       uid <- dat$uid
                       statement = "update"
                       
                     }
                     
                     if(!is.null(input$photos)) {
                       files <- nrow(input$photos)
                       
                       if(files>=1) {
                         for(i in 1:files) {
                           thisFile <- input$photos[[i, "datapath"]]
                           ext <- tools::file_ext(thisFile)
                           
                           if(localDB) {
                             
                             print("Saving image to local directory")
                             
                             saveFile(thisFile, uid, i, ext)
                             
                           } else {
                             
                             print("Transfering image to server")
                             
                             transferFile(thisFile,
                                          uid,
                                          dbInfo[[1]][[2]],
                                          i,
                                          ext,
                                          deviceInfo[[1]][[1]],
                                          deviceInfo[[1]][[2]],
                                          TRUE)
                           }
                         }
                       }
                     }
                     
                     query <- writeExterneQuery(uid, 
                                                dat$data$nom, 
                                                dat$data$prenom,
                                                dat$data$sexe,
                                                dat$data$date_naissance, 
                                                dat$data$phone_number_patient,
                                                dat$data$pathologie_1,
                                                dat$data$pathologie_2,
                                                dat$data$pathologie_3,
                                                dat$data$description_histoire,
                                                dat$data$contact_person,
                                                dat$data$contact_phone,
                                                dat$data$contact_email,
                                                dat$data$hopital,
                                                dat$data$created_at, 
                                                dat$data$created_by, 
                                                dat$data$modified_at, 
                                                dat$data$modified_by,
                                                status = dat$data$status,
                                                aStatement = statement)
                     
                     print('Trying to execute query...')
                     print(query)
                     dbExecute(conn, query)
                     
                     session$userData$externes_trigger(session$userData$externes_trigger() + 1)
                     
                     showToast("success", message = "Patient enregistré")}, 
                     
                     error = function(error) {
                       
                       msg <- paste0("Erreur pendant l'enregistrement...")
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
  
  } # End externesEditModuleServer
