#' Patient Delete Module
#'
#' This module is for deleting a row's information from the patients database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param dossier_to_delete string - the patient to be deleted
#' @param modal_trigger reactive trigger to open the modal (Delete button)
#'
#' @return None
#'
dossiersDeleteModuleServer <- function(id,
                                       modal_title, dossier_to_delete, modal_trigger) {
    moduleServer(id,
                 function(input, output, session) {
                   
                   ns <- session$ns
                   
                   # Observes trigger for this module (here, the Delete Button)
                   observeEvent(modal_trigger(), {
                     # Authorize who is able to access particular buttons (here, modules)
                     #req(session$userData$email == 'notification@subvertising.org')
                     
                     printWarningEffacerProfil <- paste0('<p style="color:#FF4500;font-size:60px">&#9888;</p> <br> <h4 style = "line-height: 1.75;">Etes-vous sûr de bien vouloir <br> effacer le dossier de M./Mme. ', dossier_to_delete()$nom, '?</h4>')
                     
                     showModal(modalDialog(
                       div(style = "padding: 30px;", class = "text-center",
                           HTML(printWarningEffacerProfil)),
                       title = modal_title,
                       size = "m",
                       footer = list(modalButton("Annuler"),
                                     actionButton(ns("submit_delete"),
                                                  "Effacer profil",
                                                  class = "btn-danger",
                                                  style = "color: #fff;"))
                       ) # Close modal dialog
                       ) # Close show modal
                     }) # Close observer
                   
                   observeEvent(input$submit_delete, {
                     req(dossier_to_delete())
                     
                     removeModal()
                     
                     tryCatch({
                       
                       uid <- dossier_to_delete()$uid
                       
                       clearCache(uid)
                       
                       dbExecute(conn,
                                 "DELETE FROM patients WHERE uid=$1", params = c(uid))
                       
                       
                       
                       session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
                       
                       showToast("success", printToastMessage(modal_title))
                       
                     }, error = function(error) {
                       msg <- "Erreur pendant la suppression"
                       # print `msg` so that we can find it in the logs
                       print(msg)
                       # print the actual error to log it
                       print(error)
                       # show error `msg` to user.  User can then tell us about error and we can
                       # quickly identify where it cam from based on the value in `msg`
                       showToast("error", msg)
                     })
                   })
                 }) # Close module server
  } # Close dossiersDeleteModuleServer
