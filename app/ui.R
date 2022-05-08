
tagList(shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
  
        navbarPage(title = "Pendejapp",
                   id = "tabs",
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
                                       HTML("<h2>Under construction...</h2>")
                                       
                              ),
                              "----",
                              "Configuration",
                              tabPanel("Bonjour",
                                       HTML("<h2>Under construction...</h2>")
                                       )
                   )
        )
)

