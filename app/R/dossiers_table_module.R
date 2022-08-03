#' Dossiers Table Module UI
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
dossiersTableModuleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width = 4,
                    tags$br(),
                    tags$br(),
                    actionButton(ns("add_dossier"),
                                 "Nouveau dossier",
                                 class = "btn-success",
                                 style = "color: #fff;",
                                 icon = icon('plus'),
                                 width = '66%'),
                    tags$br(),
                    tags$br())
    ), # Close fluid row
    fluidRow(column(width = 12,
                    title = "Mes dossiers",
                    DTOutput(ns('dossiers_table')) %>% withSpinner(),
                    textOutput(ns('target_uid')))
    ), # Close fluid row
    tags$script(src = "dossiers_table_module.js"),
    tags$script(paste0("dossiers_table_module_js('", ns(''), "')"))
  )
} # End dossiersTableModuleUI


#' Dossiers Table Module Server
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
#' @return a patient uid

dossiersTableModuleServer <- function(id, credentials) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 # trigger to reload data from the "patients" table
                 session$userData$dossiers_trigger <- reactiveVal(0)
                 
                 
                 # Read in "patients" table from the database
                 dossiers <- reactive({
                   req(credentials()$user_auth)
                   
                   session$userData$dossiers_trigger()
                   
                   out <- NULL
                   tryCatch({
                     out <- conn %>%
                       tbl('patients') %>%
                       collect() %>%
                       mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
                              modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
                       arrange(desc(modified_at)) %>%
                       filter(modified_by == credentials()$info[['user']])
                       
                     }, 
                     error = function(err) {
                       msg <- "Could not find what you are looking for!"
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
                 
                 dossiers_table_prep <- reactiveVal(NULL)
                 
                 observeEvent(dossiers(), {
                   
                   out <- dossiers()
                     
                   ids <- out$uid
                   
                   actions <- purrr::map_chr(ids, function(id_) {
                     paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Modifier" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
                     <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Effacer" id = ', id_, ' style="margin: 0"><i class="fa fa-times-circle"></i></button>
                            </div>')
                   })
                   
                   # Select relevant columns for the user
                   out <- out %>%
                     select(nom, prenom, date_naissance, pathologie, pre_decision, def_decision)
                   
                   # Set the Action Buttons row to the first column of the `dossiers` table
                   out <- cbind(tibble(" " = actions),
                                out)
                   
                   if (is.null(dossiers_table_prep())) {
                     # loading data into the table for the first time, so we render the entire table
                     # rather than using a DT proxy
                     dossiers_table_prep(out)
                     
                   } else {
                     # table has already rendered, so use DT proxy to update the data in the
                     # table without reendering the entire table
                     replaceData(dossiers_table_proxy,
                                 out,
                                 resetPaging = FALSE,
                                 rownames = FALSE)
                     }
                 })
                
                 
                 output$dossiers_table <- renderDT({
                   req(credentials()$user_auth, dossiers_table_prep())
                   
                   out <- dossiers_table_prep() 
                   
                   out %>%
                     datatable(rownames = FALSE,
                               colnames = c('Nom', 'Prénom', 'Date de naissance', 
                                            'Pathologie', 'Décision préliminaire', 'Décision finale'),
                               selection = "single",
                               class = "compact stripe row-border nowrap",
                               escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                               options = list(scrollX = TRUE,
                                              dom = 't',
                                              columnDefs = list(list(targets = 0, orderable = FALSE)),
                                              drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
                               ) #%>%
                   # formatDate(
                   #   columns = c("created_at", "modified_at"),
                   #   method = 'toLocaleString')
                   
                 })
                 
                 
                 output$target_uid <- renderText({

                   dossiers()[input$dossiers_table_rows_selected,][[1]]

                   })
                 
                # fichePatientModuleServer("fiche_patient",
                #                          reactive({dossiers()[input$dossiers_table_rows_selected,][[1]]}))
                 
                 
                 # observeEvent(is.null(credentials()$user_auth), {
                 # 
                 #   toggle("add_dossier")
                 # 
                 # })
                 
                 dossiers_table_proxy <- DT::dataTableProxy('dossiers_table')
                 
                 dossiersEditModuleServer("add_dossier",
                                           modal_title = "Registrer un nouveau dossier",
                                           dossier_to_edit = function() NULL,
                                           modal_trigger = reactive({input$add_dossier}))
                 
                 
                 dossier_to_edit <- eventReactive(input$dossier_id_to_edit, {
                   
                   dossiers() %>%
                     filter(uid == input$dossier_id_to_edit)
                   
                 })
                 
                 dossiersEditModuleServer("edit_dossier",
                                          modal_title = "Modification du profil",
                                          dossier_to_edit = dossier_to_edit,
                                          modal_trigger = reactive({input$dossier_id_to_edit}))
                 
                 
                 dossier_to_delete <- eventReactive(input$dossier_id_to_delete, {
                     
                   dossiers() %>%
                       filter(uid == input$dossier_id_to_delete) %>%
                       as.list()
                   
                   })
                 
                 
                 dossiersDeleteModuleServer("delete_dossier",
                                            modal_title = "Effacer profil",
                                            dossier_to_delete = dossier_to_delete,
                                            modal_trigger = reactive({input$dossier_id_to_delete}))
                 
                 

               }) # Close module server
  
} # End dossiersTableModuleServer
