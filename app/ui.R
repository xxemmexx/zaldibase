
tagList(shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
  
        navbarPage(title = HTML('<p style="font-family: Garamond">Zal<b>di|b</b>ase</p>'),
                   id = "tabs",
                   collapsible = TRUE,
                   windowTitle = "Garde",
                   theme = shinytheme("flatly"), 
                   tabPanel("Application",
                            
                            # add logout button UI
                            div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                            # add login panel UI function
                            shinyauthr::loginUI(id = "login",
                                                title = "Bienvenue!",
                                                user_title = "Identifiant",
                                                pass_title = "Mot de passe",
                                                login_title = "Se connecter",
                                                error_message = "Identifiant ou mot de passe invalides!"),
                            
                            patients_table_module_ui("patients_table")
                            
                            ), # Close tabPanel Mes Patients
                   
                   tabPanel("About",
                            HTML('<h2 style="font-family: Garamond">Zal<b>di|b</b>ase</h2> <br>
                                 <p>Un projet pour et par pendejos qui veulent une 
                                 administration simple pour les chefs de garde du CHU.<br><br>
                                 Avez-vous des questions? Gardez-les pour vous.</p>')
                            
                   )
        )
)

