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
      "Accueil",
      
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
      
      conditionalPanel(condition = "output.role == 'admin' || output.role == 'chef' || output.role == 'resident'",
                       tabsetPanel(
                         tabPanel("Dossiers en cours", icon = icon("list-ul"), value = "dossiers",
                                  
                                  fluidRow(
                                    tags$br(),
                                    tags$br(),
                                    column(width = 4),
                                    column(width = 4,
                                           
                                           tags$span(actionButton("take_garde", 
                                                        "Je prends la garde",
                                                        icon("share"),
                                                        style="color: #FFF0F5; background-color: #3E3F3A")),
                                           style = "position:absolute;right:2em;"),
                                    column(width = 4,
                                           DTOutput('garde_table')),
                                    column(width = 4)
                                  ),
                                  fluidRow(column(width = 4,
                                                  tags$br(),
                                                  tags$br(),
                                                  actionButton("add_dossier",
                                                               "Nouveau dossier",
                                                               class = "btn-success",
                                                               style = "color: #FFF0F5; background-color: #008000",
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
                                                    tags$span(actionButton("show_contact_details", 
                                                                           "Contact",
                                                                           style="color: #FFF0F5; background-color: #3E3F3A"),
                                                              style = "position:absolute;right:2em;"),
                                                    tags$span(actionButton("archive_show_contact_details", 
                                                                           "Contact",
                                                                           style="color: #FFF0F5; background-color: #3E3F3A"),
                                                              style = "position:absolute;right:2em;"),
                                                    h3(textOutput('patient_display_name')),
                                                    h4(textOutput('patient_age')),
                                                    h3(textOutput('archive_patient_display_name')),
                                                    h4(textOutput('archive_patient_age')),
                                                    uiOutput('info_icons'),
                                                    uiOutput('archive_info_icons'),
                                                    uiOutput('decisions'),
                                                    uiOutput('archive_decisions'),
                                                    tags$br(),
                                                    tags$br(),
                                                    uiOutput('pathologies'),
                                                    uiOutput('archive_pathologies'),
                                                    tags$br(),
                                                    tags$br(),
                                                    uiOutput('coagulation'),
                                                    uiOutput('archive_coagulation'),
                                                    tags$br(),
                                                    uiOutput('description_histoire'),
                                                    uiOutput('archive_description_histoire')
                                                    
                                                  ),
                                                  wellPanel(
                                                    uiOutput('photos_title') %>% withSpinner(),
                                                    tags$br(),
                                                    tags$span(actionButton("refresh_images", 
                                                                           "",
                                                                           icon("refresh"),
                                                                           style="color: #FFF0F5; background-color: #3E3F3A;"),
                                                              style = "position:absolute;right:2em;"),
                                                    tags$span(imageOutput("tiffImage"),
                                                    tags$span(actionButton("decrease_index", 
                                                                           "",
                                                                           icon("arrow-left"),
                                                                           style="color: #FFF0F5; background-color: #3E3F3A"),
                                                              actionButton("expand_image", 
                                                                           "",
                                                                           icon("arrows-alt"),
                                                                           style="color: #FFF0F5; background-color: #3E3F3A"),
                                                              actionButton("increase_index", 
                                                                           "",
                                                                           icon("arrow-right"),
                                                                           style="color: #FFF0F5; background-color: #3E3F3A"),
                                                              
                                                              style = "position:absolute;left:250px;")
                                                    )
                                                           
                                                  )),
                                           column(width = 1)
                                  ) # Close fluid row
                         ),
                         tabPanel("Archive", icon = icon("box-open"), value = "archive",
                                  fluidRow(column(width = 12,
                                                  title = "Mes dossiers",
                                                  tags$br(),
                                                  tags$br(),
                                                  DTOutput('archive_table') %>% withSpinner()))
                                           ),
                         tabPanel("Staff", icon = icon("user-nurse"), value = "staff",
                                 fluidRow(column(width = 8,
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$span(actionButton("staff_meeting", 
                                                                        "Lancer le staff meeting",
                                                                        icon("users"), 
                                                                        style="color: #FFF0F5; background-color: #000080"),
                                                           style = "position:absolute;left:2em;"))
                                          ), # Close Fluid row
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 fluidRow(div(id = "staff_ui_controllers",
                                              style = "padding:10px 5px 10px 50px;background-color:#3E3F3A;",
                                              actionButton("decrease_patient_index", 
                                                           "",
                                                           icon("arrow-left"),
                                                           style="color: #DAA520; background-color: #3E3F3A"),
                                              actionButton("increase_patient_index", 
                                                           "",
                                                           icon("arrow-right"),
                                                           style="color: #DAA520; background-color: #3E3F3A")
                                              ) %>% shinyjs::hidden()
                                   
                                 ),
                                 fluidRow(div(id = "staff_ui",
                                              column(width = 12,
                                                     wellPanel(
                                                       h3(textOutput('staff_patient_display_name')),
                                                       h4(textOutput('staff_patient_age'))
                                                       )
                                                     )
                                              ) %>% shinyjs::hidden()
                                          ) # Close Fluid row
                                 ) # Close tabpanel Garde
                                          
                         ) # Close tabset panel
      ), # Close conditional panel admin
      conditionalPanel(condition = "output.role == 'secretariat'",
        tabsetPanel(
          tabPanel("Rendez-vous à prendre",
                   tags$br(),
                   tags$br(),
                   fluidRow(column(width = 12,
                                   title = "Planification",
                                   DTOutput('rendezvous_table') %>% withSpinner())
                   ), # Close fluid row
                   tags$script(src = "rendezvous_table_module.js"),
                   tags$script(paste0("rendezvous_table_module_js('')"))),
          tabPanel("Rendez-vous accordés")
        )
      )
    ), # Close Accueil
    tabPanel("About",
             div(style = "padding:30px;background-color:#3E3F3A;color:#FFFAFA;",
                 HTML(printAbout))
             )
  )
)


