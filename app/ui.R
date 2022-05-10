

tagList(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  
  navbarPage(
    title = HTML('<p style="font-family: Garamond">Zal<b>di|b</b>ase</p>'),
    id = "tabs",
    collapsible = TRUE,
    windowTitle = "Garde",
    theme = shinytheme("flatly"),
    
    
    tabPanel(
      "Home",
      
      # add logout button UI
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      # add login panel UI function
      shinyauthr::loginUI(
        id = "login",
        title = "Bienvenue!",
        user_title = "Identifiant",
        pass_title = "Mot de passe",
        login_title = "Se connecter",
        error_message = "Identifiant ou mot de passe invalides!"
      ),
      
      
      conditionalPanel(condition = "output.role == 'admin'",
                       tabsetPanel(
                         tabPanel("Mes dossiers",
                                  patientsTableModuleUI("patients_table")),
                         tabPanel("Garde",
                                  HTML('<h2> Some beautiful content and all power </h2>'))
                       )),
      
      conditionalPanel(condition = "output.role == 'resident'",
                       tabsetPanel(
                         tabPanel("Mes dossiers",
                                  #patients_table_module_ui("patients_table")
                                  ),
                         tabPanel("Garde",
                                  HTML('<h2> Some standard content </h2>'))
                       )),
      
      conditionalPanel(condition = "output.role == 'user'",
                       tabsetPanel(tabPanel(
                         "Mes dossiers",
                         HTML('<h2> Huevos puto! </h2>')
                         
                         
                       )))
      
    ),
    tabPanel(
      "About",
      HTML(
        '<h2 style="font-family: Garamond">Zal<b>di|b</b>ase</h2> <br>
                                 <p>Un projet pour et par pendejos qui veulent une
                                 administration simple pour les chefs de garde du CHU.<br><br>
                                 Avez-vous des questions? Gardez-les pour vous.</p>'
      )
    )
  )
)
