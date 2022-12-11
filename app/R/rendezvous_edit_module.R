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
                       select(name)
                     
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
                               dateInput(ns("date_rendezvous"),
                                         'Date (AAAA-MM-DD)',
                                         value = ifelse(is.null(hold), as.character(today()+7), hold$date_rendezvous),
                                         language = "fr"),
                               timeInput(ns("time_rendezvous"), 
                                         "Heure (écart de 5 mins)", 
                                         value = Sys.time(), 
                                         minute.steps = 5,
                                         seconds = FALSE),
                               selectInput(ns("rendezvous_avec"),
                                           "Médecin",
                                           choices = docs,
                                           selected = NULL)
                                      
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
                     
                     
                     
                     
                     
                   }) # Close modal trigger
                 
                 ############# END MODAL TRIGGER ###############################
                 
                 
                 ################# CAPTURE DATA ################################
                 
                 rendez_vous_dat <- reactive({
                   
                   hold <- rendezvous_patient()
                   
                   #time_now <- Sys.time() %>% ymd_hms()
                   
                   out <- list(uid = deliverUID(hold),
                               data = list("date_rendezvous" = input$date_rendezvous,
                                           "time_rendezvous" = format(input$time_rendezvous, format = "%H:%M:%S"),
                                           "rendezvous_avec" = input$rendezvous_avec))
              
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
                     
                     query <- paste0("UPDATE patients
                                      SET date_rendezvous = '", dat$data$date_rendezvous,
                                     "', time_rendezvous = '", dat$data$time_rendezvous,
                                     "', rendezvous_avec = '", dat$data$rendezvous_avec,
                                     "' WHERE uid = '", dat$uid, "';"
                     )
                     
                     print('Trying to execute query...')
                     print(query)
                     
                     dbExecute(conn, query)
                     
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
