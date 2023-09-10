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
                                     modal_title, 
                                     dossier_to_edit, 
                                     modal_trigger,
                                     permissions) {
    
  moduleServer(id, 
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 output$privilege <- renderText({

                   permissions()

                 })
                 
                 
                 
                   ############# MODAL TRIGGER #################################
                   observeEvent(modal_trigger(), {
                     
                     hold <- dossier_to_edit()

                     
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
                                      ), #Close fluidRow
                                      tags$br(),
                                      fluidRow(HTML("<h4 style=text-align:center;><b>Données du patient</b></h4>"),
                                               tags$br(),
                                               fluidRow(column(width = 12, align = "center",
                                                               checkboxInput(ns('patient_inconnu'),
                                                                             HTML('<b>Patient inconnu</b>'),
                                                                             value = ifelse(is.null(hold), FALSE, hold$patient_inconnu)))),
                                               fluidRow(column(width = 6, align="center",
                                                               radioButtons(ns('sex'), 
                                                                            HTML('<b>Sexe</b>'), 
                                                                            choices = c("H" = 0, "F" = 1),
                                                                            inline = TRUE,
                                                                            selected = ifelse(is.null(hold), 0, hold$sexe)),
                                                               textInput(ns("nom"),
                                                                         'Nom',
                                                                         value = ifelse(is.null(hold), "", hold$nom)),
                                                               textInput(ns("prenom"),
                                                                         'Prénom',
                                                                         value = ifelse(is.null(hold), "", hold$prenom))
                                                      ), #close column
                                               column(width = 6, align = "center",
                                                      dateInput(ns("date_naissance"),
                                                                'Date de naissance (AAAA-MM-JJ)',
                                                                value = ifelse(is.null(hold), "", hold$date_naissance),
                                                                language = "fr"),
                                                      textInput(ns("phone_number_patient"),
                                                                'Numéro de téléphone du patient',
                                                                value = ifelse(is.null(hold), "", hold$phone_number_patient)),
                                                      textInput(ns("email_patient"),
                                                                'Email du patient',
                                                                placeholder = "(facultatif)",
                                                                value = ifelse(is.null(hold), "", hold$email_patient))
                                                      ) # close column
                                               ), # Close fluidRow
                                               tags$br(),
                                               HTML("<h4 style=text-align:center;><b>Contexte de la maladie</b></h4>"),
                                               tags$br(),
                                               HTML("<h5 style=text-align:left;font-family:Garamond><b>PATHOLOGIES</b></h5>"),
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
                                                                            selected = ifelse(is.null(hold) || str_trim(hold$pathologie_2) == "", 0, 1)),
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
                                                                                             selected = ifelse(is.null(hold) || str_trim(hold$pathologie_3) == "", 0, 1)),
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
                                               HTML("<h5 style=text-align:left;font-family:Garamond><b>FLUIDIFIANTS</b></h5>"),
                                               fluidRow(
                                                 column(width = 12, 
                                                        HTML('<b>Le patient prend-il des fluidifiants, ou a-t-il des troubles de coagulation?</b>'),
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
                                               tags$br(),
                                               HTML("<h5 style=text-align:left;font-family:Garamond><b>SYNDROMES</b></h5>"),
                                      fluidRow(column(width = 12, 
                                                      HTML("<b>Le patient présente-t-il un syndrome inflammatoire ou infectieux actif, ou est-il en isolement?</b>"),
                                                      radioButtons(ns('add_syndrome'), 
                                                                   "", 
                                                                   choices = c("Non" = 0, "Oui" = 1),
                                                                   inline = FALSE,
                                                                   selected = ifelse(is.null(hold) || str_trim(hold$syndrome) == "", 0, 1))
                                               ) # Close column
                                      ), # Close fluidRow
                                      conditionalPanel("input.add_syndrome == 1",
                                                       fluidRow(column(width = 12, align = 'center',
                                                                       HTML('<b>Indiquez le syndrome observé</b>'),
                                                                       textInput(ns('syndrome'),
                                                                                 "",
                                                                                 value = ifelse(is.null(hold), "", hold$syndrome),
                                                                                 width = '66%'))),
                                                       ns = ns), # Close conditional panel
                                      
                                      tags$br(),
                                      # HTML("<h5 style=text-align:left;font-family:Garamond><b>COMORBIDITÉS</b></h5>"),
                                      # tags$br(),
                                      # fluidRow(
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Metabolique</b>')),
                                      #   column(width = 8, align = 'left',
                                      #                          radioButtons(ns('comorb_metabolique'),
                                      #                                       "",
                                      #                                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                                       inline = TRUE,
                                      #                                       selected = ifelse(is.null(hold), 0, hold$comorb_metabolique))
                                      #          )
                                      # ),
                                      # fluidRow(style = "background-color:#F1F1F4;",
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Cardiovasculaire</b>')),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_cardiovasculaire'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_cardiovasculaire))
                                      #   )
                                      # ),
                                      # fluidRow(
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Renale</b>')),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_renale'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_renale))
                                      #   )
                                      # ),
                                      # fluidRow(style = "background-color:#F1F1F4;",
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Hepatique</b>')),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_hepatique'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_hepatique))
                                      #   )
                                      # ),
                                      # fluidRow(
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Oncologique/Hematologique</b>')),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_oncologique'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_oncologique))
                                      #   )
                                      # ),
                                      # fluidRow(style = "background-color:#F1F1F4;",
                                      #   column(width = 4, align = 'right',
                                      #          HTML('<br><b>Neurologique</b>')),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_neurologique'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_neurologique))
                                      #   )
                                      # ),
                                      # fluidRow(
                                      #   column(width = 4, align = 'right',
                                      #          textInput(ns('comorbidite_1'),
                                      #                    "",
                                      #                    value = ifelse(is.null(hold) || str_trim(hold$comorbidite_1) == "", "", hold$comorbidite_1),
                                      #                    placeholder = 'Autre...',
                                      #                    width = '80%')
                                      #          ),
                                      #   column(width = 8, align = 'left',
                                      #          radioButtons(ns('comorb_1'),
                                      #                       "",
                                      #                       choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                       inline = TRUE,
                                      #                       selected = ifelse(is.null(hold), 0, hold$comorb_1))
                                      #   )
                                      # ),
                                      # fluidRow(style = "background-color:#F1F1F4;",
                                      #          column(width = 4, align = 'right',
                                      #                 textInput(ns('comorbidite_2'),
                                      #                           "",
                                      #                           value = ifelse(is.null(hold) || str_trim(hold$comorbidite_2) == "", "", hold$comorbidite_2),
                                      #                           placeholder = 'Autre...',
                                      #                           width = '80%')
                                      #          ),
                                      #          column(width = 8, align = 'left',
                                      #                 radioButtons(ns('comorb_2'),
                                      #                              "",
                                      #                              choices = c("Non" = 0, "Modérée" = 1, "Importante" = 2, "Très importante" = 3),
                                      #                              inline = TRUE,
                                      #                              selected = ifelse(is.null(hold), 0, hold$comorb_2))
                                      #          )
                                      # ),
                                      # tags$br(),
                                      HTML("<h5 style=text-align:left;font-family:Garamond><b>HISTOIRE DE LA MALADIE</b></h5>"),
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
                                               ), # Close fluidRow
                                      fluidRow(column(width = 12, align="center",
                                                      conditionalPanel("output.privilege == 'resident'",
                                                                       selectInput(ns('pre_decision'),
                                                                                   "Décision préliminaire",
                                                                                   choices = decisions,
                                                                                   selected = ifelse(is.null(hold), "", hold$pre_decision)),
                                                                       ns = ns
                                                      ),
                                                      conditionalPanel("output.privilege == 'admin' || output.privilege == 'chef'",
                                                                       selectInput(ns('def_decision'),
                                                                                   "Décision définitive",
                                                                                   choices = decisions,
                                                                                   selected = ifelse(is.null(hold), "", hold$def_decision)),
                                                                       ns = ns),
                                                      textAreaInput(ns('explication'),
                                                                    'Explication',
                                                                    placeholder = "Expliquez votre décision...",
                                                                    value = ifelse(is.null(hold), "", hold$explication),
                                                                    width = '100%',
                                                                    height = '100px')
                                      ) # Close column
                                        
                                      )# Close fluidRow
                                      ) # Close fluidrow
                             ), # Close div
                         title = modal_title,
                         size = 'l',
                         footer = list(modalButton('Annuler'),
                                       actionButton(ns('submit'),
                                                    printButtonLabel(modal_title),
                                                    class = "btn btn-primary mb1 bg-olive")) 
                         ) # Close modal dialog
                       ) # Close showModal
                     
                     #------------END FILL-IN FORM------------------------------
                     
                     
                     
                     #------------FIELD VALIDATION - FEEDBACK-------------------
                     
                     formFields <- reactiveValues(contact_person = 0,
                                                  hopital = 0,
                                                  contact_phone = 0,
                                                  contact_email = 0,
                                                  nom = 0,
                                                  prenom = 0,
                                                  date_naissance = 0,
                                                  phone_number_patient = 0,
                                                  pathologie_1 = 0,
                                                  syndrome = 0,
                                                  description_histoire = 0)
                     
                     
                     observeEvent(input$contact_person, {
                       if (input$contact_person == "") {
                         formFields$contact_person = 0
                         shinyFeedback::showFeedbackDanger("contact_person",
                                                           text = "La personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_person")
                         formFields$contact_person = 1
                       }
                     })
                     
                     observeEvent(input$hopital, {
                       if (input$hopital == " ") {
                         formFields$hopital = 0
                         shinyFeedback::showFeedbackDanger("hopital",
                                                           text = "L'hôpital d'origine est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("hopital")
                         formFields$hopital = 1
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
                         formFields$contact_phone = 0
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
                         formFields$contact_email = 0
                         shinyFeedback::showFeedbackDanger("contact_email",
                                                           text = "L'email de la personne de contact est obligatoire!")
                         shinyjs::disable('submit')
                       } else {
                         shinyFeedback::hideFeedback("contact_email")
                         formFields$contact_email = 1
                       }
                     })
                     
                     observeEvent(
                       eventExpr = {
                         input$nom
                         input$patient_inconnu
                       }, 
                       handlerExpr = {
                         if (input$nom == "" & !input$patient_inconnu) {
                           formFields$nom = 0
                           shinyFeedback::showFeedbackDanger("nom",
                                                             text = "Le nom du patient est obligatoire!")
                           shinyjs::disable('submit')
                         } else {
                           shinyFeedback::hideFeedback("nom")
                           formFields$nom = 1
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$prenom
                         input$patient_inconnu
                       }, 
                       handlerExpr = {
                         if (input$prenom == "" & !input$patient_inconnu) {
                           formFields$prenom = 0
                           shinyFeedback::showFeedbackDanger("prenom",
                                                             text = "Le prénom du patient est obligatoire!")
                           shinyjs::disable('submit')
                         } else {
                           shinyFeedback::hideFeedback("prenom")
                           formFields$prenom = 1
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$date_naissance
                         input$patient_inconnu
                       }, 
                       handlerExpr = {
                         if (length(input$date_naissance) < 1 & !input$patient_inconnu) {
                           formFields$date_naissance = 0
                           shinyFeedback::showFeedbackDanger("date_naissance",
                                                             text = "Le date de naissance du patient est obligatoire!")
                           shinyjs::disable('submit')
                         } else {
                           shinyFeedback::hideFeedback("date_naissance")
                           formFields$date_naissance = 1
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$phone_number_patient
                         input$patient_inconnu
                       }, 
                       handlerExpr = {
                         if (str_trim(input$phone_number_patient) == "" & !input$patient_inconnu) {
                           formFields$phone_number_patient = 0
                           shinyFeedback::showFeedbackDanger("phone_number_patient",
                                                             text = "Le numéro de téléphone est obligatoire!")
                           shinyjs::disable('submit')
                         } else {
                           shinyFeedback::hideFeedback("phone_number_patient")
                           formFields$phone_number_patient = 1
                         }
                       }
                     )
                     
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
                         formFields$pathologie_1 = 0
                         shinyFeedback::showFeedbackDanger("pathologie_1",
                                                           text = "Indiquez une pathologie")
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
                     
                     observeEvent(
                       eventExpr = {
                         input$pathologie_2
                         input$add_pathologie_2
                       }, 
                       handlerExpr = {
                         if (str_trim(input$pathologie_2) == "" & input$add_pathologie_2 == 1) {
                           shinyFeedback::showFeedbackDanger("pathologie_2",
                                                             text = "Indiquez une pathologie")
                         } else {
                           shinyFeedback::hideFeedback("pathologie_2")
                           
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$pathologie_3
                         input$add_pathologie_3
                       }, 
                       handlerExpr = {
                         if (str_trim(input$pathologie_3) == "" & input$add_pathologie_3 == 1) {
                           shinyFeedback::showFeedbackDanger("pathologie_3",
                                                             text = "Indiquez une pathologie")
                         } else {
                           shinyFeedback::hideFeedback("pathologie_3")
                           
                         }
                       }
                     )
                     
                     observeEvent(
                       eventExpr = {
                         input$syndrome
                         input$add_syndrome
                       }, 
                       handlerExpr = {
                         if (str_trim(input$syndrome) == "" & input$add_syndrome == 1) {
                           formFields$syndrome = 0
                           shinyFeedback::showFeedbackDanger("syndrome",
                                                             text = "Indiquez le syndrome")
                           shinyjs::disable('submit')
                         } else {
                           formFields$syndrome = 1
                           shinyFeedback::hideFeedback("syndrome")
                         }
                       }
                     )
                     
                     observeEvent(input$description_histoire, {
                       if (str_trim(input$description_histoire) == "") {
                         formFields$description_histoire = 0
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
                          formFields$hopital == 1 &
                          formFields$contact_phone == 1 &
                          formFields$contact_email == 1 &
                          formFields$nom == 1 &
                          formFields$prenom == 1 &
                          formFields$date_naissance == 1 &
                          formFields$phone_number_patient == 1 &
                          formFields$pathologie_1 == 1 & 
                          formFields$syndrome == 1 &
                          formFields$description_histoire == 1) {
                         shinyjs::enable(id ='submit')
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
                               data = list("patient_inconnu" = ifelse(input$patient_inconnu, 1, 0),
                                           "nom" = prepareString(input$nom),
                                           "prenom" = prepareString(input$prenom),
                                           "sexe" = input$sex,
                                           "date_naissance" = writeISODate(input$date_naissance),
                                           "phone_number_patient" = input$phone_number_patient,
                                           "email_patient" = input$email_patient,
                                           "pathologie_1" = deliverStandardOrCustom(input$pathologie_1, input$description_pathologie_1, 1),
                                           "pathologie_2" = deliverStandardOrCustom(input$pathologie_2, input$description_pathologie_2, input$add_pathologie_2),
                                           "pathologie_3" = deliverStandardOrCustom(input$pathologie_3, input$description_pathologie_3, input$add_pathologie_3),
                                           "syndrome" = prepareString(input$syndrome),
                                           "comorb_metabolique" = ifelse(is.null(input$comorb_metabolique), 0, input$comorb_metabolique),
                                           "comorb_cardiovasculaire" = ifelse(is.null(input$comorb_cardiovasculaire), 0, input$comorb_cardiovasculaire),
                                           "comorb_renale" = ifelse(is.null(input$comorb_renale), 0, input$comorb_renale),
                                           "comorb_hepatique" = ifelse(is.null(input$comorb_hepatique), 0, input$comorb_hepatique),
                                           "comorb_oncologique" = ifelse(is.null(input$comorb_oncologique), 0, input$comorb_oncologique),
                                           "comorb_neurologique" = ifelse(is.null(input$comorb_neurologique), 0, input$comorb_neurologique),
                                           "comorbidite_1" = prepareString(input$comorbidite_1),
                                           "comorb_1" = ifelse(is.null(input$comorb_1), 0, input$comorb_1),
                                           "comorbidite_2" = prepareString(input$comorbidite_2),
                                           "comorb_2" = ifelse(is.null(input$comorb_2), 0, input$comorb_2),
                                           "description_histoire" = prepareString(input$description_histoire),
                                           "pre_decision" = input$pre_decision,
                                           "def_decision" = input$def_decision,
                                           "explication" = prepareString(input$explication),
                                           "contact_person" = prepareString(input$contact_person),
                                           "contact_phone" = input$contact_phone,
                                           "contact_email" = input$contact_email,
                                           "hopital" = deliverStandardOrCustom(input$hopital, input$hopital_autre, 1),
                                           "has_coagulation" = input$add_coagulation,
                                           "treat_coagulant_1" = input$traitement_1,
                                           "date_derniere_prise_1" = input$date_derniere_prise_1,
                                           "treat_coagulant_2" = input$traitement_2,
                                           "date_derniere_prise_2" = input$date_derniere_prise_2,
                                           "treat_coagulant_3" = input$traitement_3,
                                           "date_derniere_prise_3" = input$date_derniere_prise_3,
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
                       
                       out <- conn %>%
                         tbl("garde") %>%
                         collect() %>%
                         arrange(desc(garde_id)) %>%
                         top_n(1)
                       
                       gardeId <- out$garde_id
                       
                       } else {
                         
                         uid <- dat$uid
                         statement = "update"
                         gardeId <- 0
                         
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
                     
                     
                     thisQuery <- writeQuery(uid, 
                                             dat$data$patient_inconnu,
                                             dat$data$nom, 
                                             dat$data$prenom,
                                             dat$data$sexe,
                                             dat$data$date_naissance, 
                                             dat$data$phone_number_patient,
                                             dat$data$email_patient,
                                             dat$data$pathologie_1,
                                             dat$data$pathologie_2,
                                             dat$data$pathologie_3,
                                             dat$data$syndrome,
                                             dat$data$comorb_metabolique,
                                             dat$data$comorb_cardiovasculaire,
                                             dat$data$comorb_renale,
                                             dat$data$comorb_hepatique,
                                             dat$data$comorb_oncologique,
                                             dat$data$comorb_neurologique,
                                             dat$data$comorbidite_1,
                                             dat$data$comorb_1,
                                             dat$data$comorbidite_2,
                                             dat$data$comorb_2,
                                             dat$data$description_histoire,
                                             dat$data$pre_decision,
                                             dat$data$def_decision,
                                             dat$data$explication,
                                             dat$data$contact_person,
                                             dat$data$contact_phone,
                                             dat$data$contact_email,
                                             dat$data$hopital,
                                             dat$data$has_coagulation %>% as.integer(),
                                             dat$data$treat_coagulant_1,
                                             dat$data$date_derniere_prise_1,
                                             dat$data$treat_coagulant_2,
                                             dat$data$date_derniere_prise_2,
                                             dat$data$treat_coagulant_3,
                                             dat$data$date_derniere_prise_3,
                                             dat$data$created_at, 
                                             dat$data$created_by, 
                                             dat$data$modified_at, 
                                             dat$data$modified_by,
                                             gardeId,
                                             status = dat$data$status,
                                             aStatement = statement)
                     
                     print(paste0('Trying to execute ', statement, ' query...'))
                     print(thisQuery)
                     
                     dbExecute(conn, thisQuery)
                     
                     session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
                     
                     session$userData$archive_trigger(session$userData$archive_trigger() + 1)
                     
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
                 
                 ############# END TALK TO DB, SUBMIT ACTION ###################
                 
                 # set suspendWhenHidden to FALSE so it renders even without output
                 outputOptions(output, 'privilege', suspendWhenHidden = FALSE) 
                 
                 }) # Close module server
  
  } # End dossiersEditModuleServer
