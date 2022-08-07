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
                             fluidRow(HTML("<h4>Données du centre hospitalier d'origine</h4>"),
                                      column(width = 6,
                                             textInput(ns('contact_person'),
                                                       "Personne de contact",
                                                       value = ifelse(is.null(hold), "", hold$contact_person)),
                                             selectInput(ns("hopital"),
                                                         "Centre Hospitalier d'origine",
                                                         choices = hopitaux,
                                                         selected = ifelse(is.null(hold), "", hold$hopital)),
                                             conditionalPanel("input.hopital == 'Autre...'",
                                                              textInput(ns('hopital_autre'),
                                                                        'Hôpital/Clinique',
                                                                        placeholder = "Ecrivez le nom de l'hôpital..."),
                                                              ns = ns)
                                      ),
                                      column(width = 6,
                                             textInput(ns('contact_phone'),
                                                       "Numéro de téléphone du contact",
                                                       value = ifelse(is.null(hold), "", hold$contact_phone)),
                                             textInput(ns('contact_email'),
                                                       "Email",
                                                       value = ifelse(is.null(hold), "", hold$contact_email)))
                                      ),
                                      fluidRow(HTML("<h4>Données du patient</em></h4>"),
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
                                                                'Numéro de téléphone du patient',
                                                                value = ifelse(is.null(hold), "", hold$phone_number_patient)))
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      selectInput(ns('pathologie_1'),
                                                                  'Pathologie',
                                                                  choices = pathologies,
                                                                  selected = ifelse(is.null(hold), "", hold$pathologie_1)),
                                                      conditionalPanel("input.pathologie_1 == 'Autre...'",
                                                                       textAreaInput(ns('description_pathologie_1'),
                                                                                     'Description',
                                                                                     placeholder = "Decrivez..."),
                                                                       ns = ns),
                                                      checkboxInput(ns('add_pathologie_2'),
                                                                    'Ajouter une 2ème pathologie',
                                                                    value = FALSE),
                                                      conditionalPanel("input.add_pathologie_2",
                                                                       selectInput(ns('pathologie_2'),
                                                                                   'Pathologie',
                                                                                   choices = pathologies,
                                                                                   selected = ifelse(is.null(hold), "", hold$pathologie_2)),
                                                                       conditionalPanel("input.pathologie_2 == 'Autre...'",
                                                                                        textAreaInput(ns('description_pathologie_2'),
                                                                                                      'Description',
                                                                                                      placeholder = "Decrivez..."),
                                                                                        ns = ns),
                                                                       checkboxInput(ns('add_pathologie_3'),
                                                                                     'Ajouter une 3ème pathologie',
                                                                                     value = FALSE),
                                                                       conditionalPanel("input.add_pathologie_3",
                                                                                        selectInput(ns('pathologie_3'),
                                                                                                    'Pathologie',
                                                                                                    choices = pathologies,
                                                                                                    selected = ifelse(is.null(hold), "", hold$pathologie_3)),
                                                                                        conditionalPanel("input.pathologie_3 == 'Autre...'",
                                                                                                         textAreaInput(ns('description_pathologie_3'),
                                                                                                                       'Description',
                                                                                                                       placeholder = "Decrivez..."),
                                                                                                         ns = ns),
                                                                       ns = ns),
                                                                       ns = ns)
                                                      ) # Close column
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      textAreaInput(ns('description_histoire'),
                                                                    'Histoire',
                                                                    placeholder = "Décrivez...",
                                                                    value = ifelse(is.null(hold), "", hold$description_histoire),
                                                                    width = '740'))
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12,
                                                      selectInput(ns('pre_decision'),
                                                                  "Décision préliminaire",
                                                                  choices = decisions,
                                                                  selected = ifelse(is.null(hold), "", hold$pre_decision)),
                                                      textAreaInput(ns('explanation'),
                                                                    'Explication',
                                                                    placeholder = "Expliquez cette décision..."),
                                                      fileInput(ns('photos'),
                                                                "Ajouter des images",
                                                                multiple = TRUE,
                                                                accept = 'image/*',
                                                                buttonLabel = "Parcourir...",
                                                                placeholder = "Aucun fichier n'a été trouvé"))
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
                     
                     
                     observeEvent(input$contact_person, {
                       if (input$contact_person == "") {
                         shinyFeedback::showFeedbackDanger("contact_person",
                                                           text = "La personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_person")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$hopital, {
                       if (input$hopital == " ") {
                         shinyFeedback::showFeedbackDanger("hopital",
                                                           text = "L'hôpital d'origine est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("hopital")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$hopital, {
                       if (input$hopital == 'Autre...' ) {
                         
                         observeEvent(input$hopital_autre, {
                           
                           if(input$hopital_autre == "") {
                             shinyFeedback::showFeedbackDanger("hopital_autre",
                                                               text = "Hôpital/clinque est obligatoire!")
                             shinyjs::disable('submit')
                           } else {
                             shinyFeedback::hideFeedback("hopital_autre")
                             shinyjs::enable('submit')
                           }
                       }) # Close hopital_autre  observer
                       } # Close if statement hopital == 'Autre'
                     }) # Close hopital observer
                     
                     observeEvent(input$contact_phone, {
                       if (input$contact_phone == "") {
                         shinyFeedback::showFeedbackDanger("contact_phone",
                                                           text = "Le numéro de téléphone du contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_phone")
                         shinyjs::enable('submit')
                       }
                     })
                     
                     observeEvent(input$contact_email, {
                       if (input$contact_email == "") {
                         shinyFeedback::showFeedbackDanger("contact_email",
                                                           text = "L'email de la personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_email")
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
                     
                     observeEvent(input$pathologie_1, {
                       if (input$pathologie_1 == " ") {
                         shinyFeedback::showFeedbackDanger("pathologie_1",
                                                           text = "La pathologie est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("pathologie_1")
                         shinyjs::enable('submit')
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
                             shinyjs::enable('submit')
                           }
                         }) # Close description observer
                       } # Close if statement pathologie == 'Autre'
                     }) # Close pathologie observer
                     
                     observeEvent(input$pre_decision, {
                       if (input$pre_decision == "") {
                         shinyFeedback::showFeedbackDanger("pre_decision",
                                                           text = "La décision préliminaire est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("pre_decision")
                         shinyjs::enable('submit')
                       }
                     })
                     
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
                                           "pathologie_1" = deliverStandardOrCustom(input$pathologie_1, input$description_pathologie_1),
                                           "pathologie_2" = deliverStandardOrCustom(input$pathologie_2, input$description_pathologie_2, input$add_pathologie_2),
                                           "pathologie_3" = deliverStandardOrCustom(input$pathologie_3, input$description_pathologie_3, input$add_pathologie_3),
                                           "description_histoire" = input$description_histoire,
                                           "pre_decision" = input$pre_decision,
                                           "contact_person" = input$contact_person,
                                           "contact_phone" = input$contact_phone,
                                           "contact_email" = input$contact_email,
                                           "hopital" = deliverStandardOrCustom(input$hopital, input$hopital_autre),
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
                           
                           transferFile(thisFile,
                                        uid,
                                        dbInfo[[1]][[2]],
                                        i,
                                        ext,
                                        TRUE)
                         }
                       }
                     }
                     
                     
                     thisQuery <- writeQuery(uid, 
                                             dat$data$nom, 
                                             dat$data$prenom,
                                             dat$data$date_naissance, 
                                             dat$data$phone_number_patient,
                                             dat$data$pathologie_1,
                                             dat$data$pathologie_2,
                                             dat$data$pathologie_3,
                                             dat$data$description_histoire,
                                             dat$data$pre_decision, 
                                             dat$data$contact_person,
                                             dat$data$contact_phone,
                                             dat$data$contact_email,
                                             dat$data$hopital,
                                             dat$data$created_at, 
                                             dat$data$created_by, 
                                             dat$data$modified_at, 
                                             dat$data$modified_by, 
                                             aStatement = statement)
                     
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
