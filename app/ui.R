# fluidPage(
#   shinyFeedback::useShinyFeedback(),
#   shinyjs::useShinyjs(),
#   # Application Title
#   titlePanel(
#     h1("PendejAPP", align = 'center'),
#     windowTitle = "Ejemplo de app"
#   ),
#   patients_table_module_ui("patients_table")
# )


tagList(shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
  
        navbarPage(title = "Pendejapp",
                   collapsible = TRUE,
                   windowTitle = "Garde",
                   theme = shinytheme("flatly"), 
                   tabPanel("Mes patients",
                            
                            # add logout button UI
                            div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                            # add login panel UI function
                            shinyauthr::loginUI(id = "login",
                                                title = "ZaldiBase",
                                                user_title = "Identifiant",
                                                pass_title = "Mot de passe",
                                                login_title = "Se connecter",
                                                error_message = "Identifiant ou mot de passe invalides!"),
                            
                            patients_table_module_ui("patients_table")
                            
                            ), # Close tabPanel Mes Patients
                   
                   navbarMenu("Autres services",
                              "----",
                              "Recherche",
                              tabPanel("Archive",
                                       
                              ),
                              "----",
                              "Configuration",
                              tabPanel("Bonjour",
                                       HTML("<h1>Bonjour!</h1>"))
                   )
        )
)

