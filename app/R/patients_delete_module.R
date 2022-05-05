
#' Patient Delete Module
#'
#' This module is for deleting a row's information from the mtcars database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param car_to_delete string - the model of the car to be deleted
#' @param modal_trigger reactive trigger to open the modal (Delete button)
#'
#' @return None
#'
patients_delete_module <- function(input, output, session, modal_title, patient_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    req(session$userData$email == 'notification@subvertising.org')

    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Etes-vous sûr de vouloir effacer le profil de M./Mme. ',
              patient_to_delete()$nom,
              '?'
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Annuler"),
          actionButton(
            ns("submit_delete"),
            "Effacer profil",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })

  observeEvent(input$submit_delete, {
    req(patient_to_delete())

    removeModal()

    tryCatch({

      uid <- patient_to_delete()$uid

      DBI::dbExecute(
        conn,
        "DELETE FROM patients WHERE uid=$1",
        params = c(uid)
      )

      session$userData$patients_trigger(session$userData$patients_trigger() + 1)
      showToast(printToastMessage(modal_title))
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
}
