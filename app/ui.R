tagList(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  
  navbarPage(
    title = HTML(printLogo),
    id = "tabs",
    collapsible = TRUE,
    windowTitle = "Garde",
    theme = shinytheme("sandstone"),
    
    
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
                         tabPanel("Mes dossiers", icon = icon("list-ul"), value = "dossiers",
                                  fluidRow(column(width = 4,
                                                  tags$br(),
                                                  tags$br(),
                                                  actionButton("add_dossier",
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
                                                  DTOutput('dossiers_table') %>% withSpinner())
                                  ), # Close fluid row
                                  tags$script(src = "dossiers_table_module.js"),
                                  tags$script(paste0("dossiers_table_module_js('')"))),
                         tabPanel("Fiche patient", icon = icon("eye"), value = "fiche",
                                  tags$br(),
                                  tags$br(),
                                  fluidRow(column(width = 1),
                                           column(width = 10,
                                                  wellPanel(
                                                    h3(textOutput('patient_display_name')),
                                                    h4(textOutput('patient_age')),
                                                    tags$br(),
                                                    tags$br(),
                                                    uiOutput('pathologies'),
                                                    tags$br(),
                                                    uiOutput('description_histoire')
                                                    )),
                                           column(width = 1)
                                           ) # Close fluid row
                                  ),
                         tabPanel("Archive", icon = icon("box-open"), value = "archive",
                                  HTML('<h2> Some beautiful archive content</h2>')),
                         tabPanel("Garde", icon = icon("user-nurse"), value = "garde",
                                  HTML('<h2> Some beautiful content and all power </h2>'))
                         ) # Close tabset panel
                       ) # Close conditional panel admin
      
      
      
    )
  )
)
