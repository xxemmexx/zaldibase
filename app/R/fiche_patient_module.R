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
fichePatientModuleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width = 4,
                    tags$br(),
                    tags$br(),
                    HTML("<b>Bienvenue!!</b>"),
                    tags$br(),
                    textOutput(ns("patient_uid")),
                    # textInput("patient_id",
                    #             "Identificateur du patient",
                    #             value = placePatientUID(textOutput("patient_uid"))),
                    tags$br(),
                    HTML("<b>Fin du message</b>"),
                    tags$br())
    )
  )
} # End fichePatientModuleUI


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
#' @return None

fichePatientModuleServer <- function(id, target_patient_id) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 output$patient_uid <- renderText({target_patient_id()})
                 
                 

               }) # Close module server
  
} # End fichePatientModuleServer
