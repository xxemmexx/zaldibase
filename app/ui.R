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
                       tabsetPanel(id = "main_tab_collection",
                         tabPanel("Dossiers en cours", icon = icon("list-ul"), value = "dossiers",
                                  fluidRow(
                                    tags$br(),
                                    tags$br(),
                                    column(width = 3,
                                           actionButton("add_dossier",
                                                        "Nouveau dossier",
                                                        class = "btn-success",
                                                        style = "color: #FFF0F5; background-color: #008000",
                                                        icon = icon('plus'))),
                                    column(width = 3),
                                    column(width = 3,
                                           tags$div(style="text-align:right;",
                                                    actionButton("take_garde",
                                                                 "Je prends la garde",
                                                                 icon("share"),
                                                                 style="color:#FFF0F5;background-color:#3E3F3A;text-align:center;"))
                                           ),
                                    column(width = 3,
                                           DTOutput('garde_table'))
                                    
                                  ),
                                  tags$hr(),
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
                                                    tags$br(),
                                                    uiOutput('decisions'),
                                                    uiOutput('archive_decisions'),
                                                    tags$br(),
                                                    tags$br(),
                                                    uiOutput('pathologies'),
                                                    uiOutput('archive_pathologies'),
                                                    tags$br(),
                                                    uiOutput('description_histoire'),
                                                    uiOutput('archive_description_histoire')
                                                    
                                                  ),
                                                  wellPanel(
                                                    tags$style(type = 'text/css',
                                                               '.modal-dialog { width: fit-content !important; }'),
                                                    uiOutput('photos_title') %>% withSpinner(),
                                                    uiOutput('archive_photos_title') %>% withSpinner(),
                                                    tags$br(),
                                                    tags$span(
                                                      actionButton("refresh_images", 
                                                                   "",
                                                                   icon("refresh"),
                                                                   style="color: #FFF0F5; background-color: #3E3F3A;"),
                                                      actionButton("archive_refresh_images", 
                                                                   "",
                                                                   icon("refresh"),
                                                                   style="color: #FFF0F5; background-color: #3E3F3A;"),
                                                      style = "position:absolute;right:2em;"),
                                                    tags$div(id = "photo_container",
                                                             style = "text-align: center;",
                                                             imageOutput("tiffImage")),
                                                    tags$div(id = "archive_photo_container",
                                                             style = "text-align: center;",
                                                             imageOutput("archive_tiffImage")),
                                                    tags$div(id = "arrows_container",
                                                             style = "text-align: center;",
                                                             actionButton("decrease_index", 
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
                                                                          style="color: #FFF0F5; background-color: #3E3F3A")
                                                             ), # Close div
                                                    tags$div(id = "archive_arrows_container",
                                                             style = "text-align: center;",
                                                             actionButton("decrease_archive_index", 
                                                                          "",
                                                                          icon("arrow-left"),
                                                                          style="color: #FFF0F5; background-color: #3E3F3A"),
                                                             actionButton("expand_archive_image", 
                                                                          "",
                                                                          icon("arrows-alt"),
                                                                          style="color: #FFF0F5; background-color: #3E3F3A"),
                                                             actionButton("increase_archive_index", 
                                                                          "",
                                                                          icon("arrow-right"),
                                                                          style="color: #FFF0F5; background-color: #3E3F3A")
                                                    ) # Close div
                                                    ) # Close well panel
                                                  ),
                                           column(width = 1)
                                  ) # Close fluid row
                         ),
                         tabPanel("Archive", icon = icon("box-open"), value = "archive",
                                  fluidRow(column(width = 12,
                                                  title = "Archive",
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
                                                                        style="color: #FFF0F5; background-color: #325d88"),
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
                                                           style="color: #DAA520; background-color:#3E3F3A"),
                                              actionButton("increase_patient_index", 
                                                           "",
                                                           icon("arrow-right"),
                                                           style="color: #DAA520; background-color:#3E3F3A"),
                                              actionButton("cloturer_staff_meeting", 
                                                           "Clôturer le staff meeting",
                                                           icon("users"),
                                                           style="color: #FFF0F5;background-color:#d9534f;position:absolute;right:2em;")
                                              ) %>% shinyjs::hidden()
                                   
                                 ),
                                 
                                 fluidRow(div(id = "staff_ui",
                                              wellPanel(
                                                fluidRow(
                                                  column(width = 12,
                                                         h3(textOutput('staff_patient_display_name')),
                                                         h4(textOutput('staff_patient_age')),
                                                         uiOutput('staff_info_icons'),
                                                         uiOutput('staff_pre_def_decisions'),
                                                         tags$br(),
                                                         tags$br(),
                                                         uiOutput('staff_pathologies')
                                                         )
                                                  ) # Close fluid row
                                              ), # Close well panel
                                              wellPanel(
                                                fluidRow(
                                                  column(width = 3),
                                                  column(width = 6,
                                                         tags$br(),
                                                         uiOutput("staff_decisions")),
                                                  column(width = 3)
                                                  ) # Close fluid row
                                                ) # Close well panel
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


