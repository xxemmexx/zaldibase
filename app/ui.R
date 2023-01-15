tagList(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")),
  
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
                                                    tags$span(actionButton("reopen_from_archive", 
                                                                           "Réouvrir",
                                                                           style="color: #FFF0F5; background-color: #008080"),
                                                              actionButton("archive_show_contact_details", 
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
                                                    tags$br(),
                                                    uiOutput('pathologies'),
                                                    uiOutput('archive_pathologies'),
                                                    tags$br(),
                                                    uiOutput('syndrome'),
                                                    uiOutput('archive_syndrome'),
                                                    tags$br(),
                                                    uiOutput('comorbidites'),
                                                    uiOutput('archive_comorbidites'),
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
                                                    ), # Close well panel
                                                  wellPanel(
                                                    tags$div(id = "chat_area",
                                                    HTML('<h4>Correspondence</h4>'),
                                                    textAreaInput('chat_message',
                                                                  '',
                                                                  placeholder = "Ecrivez votre message ici...",
                                                                  value = '',
                                                                  width = '100%',
                                                                  height = '90px'),
                                                    tags$div(style="text-align:right;",
                                                             actionButton("chat_send",
                                                                          "",
                                                                          icon("paper-plane"),
                                                                          style="color: #FFF0F5; background-color: #008080")
                                                                 ),
                                                    tags$br(),
                                                    uiOutput("chat_body")
                                                    ), # Close div chat container
                                                    tags$div(id = "chat_area_archive",
                                                             HTML('<h4>Correspondence</h4>'),
                                                             textAreaInput('chat_message_archive',
                                                                           '',
                                                                           placeholder = "Ecrivez votre message ici...",
                                                                           value = '',
                                                                           width = '100%',
                                                                           height = '90px'),
                                                             tags$div(style="text-align:right;",
                                                                      actionButton("chat_send_archive",
                                                                                   "",
                                                                                   icon("paper-plane"),
                                                                                   style="color: #FFF0F5; background-color: #008080")
                                                             ),
                                                             tags$br(),
                                                             uiOutput("chat_body_archive")
                                                    ) # Close div chat container
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
                                 fluidRow(column(width = 1),
                                          column(width = 8,
                                                 tags$br(),
                                                 tags$br(),
                                                 tags$span(actionButton("staff_meeting", 
                                                                        "Lancer le staff meeting",
                                                                        icon("users"), 
                                                                        style="color: #FFF0F5; background-color: #325d88"),
                                                           style = "position:absolute;left:2em;")),
                                          column(width = 3)
                                          ), # Close Fluid row
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 fluidRow(div(id = "staff_ui_controllers",
                                              style = "padding:10px 5px 10px 800px;background-color:#3E3F3A;",
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
                                                           style="color: #FFF0F5;background-color:#d9534f;position:absolute;right:2em;") %>%
                                                shinyjs::hidden()
                                              ) %>% shinyjs::hidden()
                                   
                                 ),
                                 fluidRow(
                                   column(width = 2,
                                          fluidRow(div(id = "staff_patient_overview",
                                                       wellPanel(style = "border: 1px solid #e3e3e3;border-radius: 4px;",
                                                         tags$br(),
                                                         tags$br(),
                                                         tags$br(),
                                                         DTOutput('patient_overview'),
                                                         tags$br(),
                                                         tags$br(),
                                                         tags$br()
                                                       )
                                                       ) %>% shinyjs::hidden() # Close div
                                                   ) # Close fluid row
                                          ), # Close column
                                   column(width = 10,
                                          fluidRow(div(id = "staff_ui",
                                                       wellPanel(
                                                         fluidRow(
                                                           h3(textOutput('staff_patient_display_name')),
                                                           h4(textOutput('staff_patient_age')),
                                                           uiOutput('staff_info_icons'),
                                                           uiOutput('staff_pre_def_decisions'),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           uiOutput('staff_pathologies'),
                                                           tags$br(),
                                                           uiOutput('staff_syndrome'),
                                                           tags$br(),
                                                           uiOutput('staff_comorbidites'),
                                                           tags$br(),
                                                           uiOutput('staff_description_histoire')
                                                         ) # Close fluid row
                                                       ), # Close well panel
                                                       wellPanel(
                                                         tags$style(type = 'text/css',
                                                                    '.modal-dialog { width: fit-content !important; }'),
                                                         uiOutput('staff_photos_title') %>% withSpinner(),
                                                         tags$br(),
                                                         
                                                         tags$div(id = "staff_photo_container",
                                                                  style = "text-align: center;",
                                                                  imageOutput("staff_tiffImage")),
                                                         tags$div(id = "staff_arrows_container",
                                                                  style = "text-align: center;",
                                                                  actionButton("decrease_index_staff",
                                                                               "",
                                                                               icon("arrow-left"),
                                                                               style="color: #FFF0F5; background-color: #3E3F3A"),
                                                                  actionButton("expand_image_staff", 
                                                                               "",
                                                                               icon("arrows-alt"),
                                                                               style="color: #FFF0F5; background-color: #3E3F3A"),
                                                                  actionButton("increase_index_staff",
                                                                               "",
                                                                               icon("arrow-right"),
                                                                               style="color: #FFF0F5; background-color: #3E3F3A")
                                                         ) # Close div
                                                       ), # Close well panel
                                                       wellPanel(
                                                         fluidRow(
                                                           column(width = 3),
                                                           column(width = 6,
                                                                  tags$br(),
                                                                  uiOutput("staff_decisions")),
                                                           column(width = 3)
                                                         ), # Close fluid row
                                                         fluidRow(
                                                           column(width = 12,
                                                                  uiOutput("staff_decision_explanations"))
                                                         ) # Close fluid row
                                                         
                                                       ) # Close well panel
                                                        #Close well panel
                                          ) %>% shinyjs::hidden()
                                          ) # Close Fluid row
                                          ) # Close column
                                 ), # Close fluidrow
                                 fluidRow(div(id = "staff_ui_controllers_2",
                                              style = "padding:10px 5px 10px 800px;background-color:#3E3F3A;",
                                              actionButton("decrease_patient_index_2", 
                                                           "",
                                                           icon("arrow-left"),
                                                           style="color: #DAA520; background-color:#3E3F3A"),
                                              actionButton("increase_patient_index_2", 
                                                           "",
                                                           icon("arrow-right"),
                                                           style="color: #DAA520; background-color:#3E3F3A")
                                 ) %>% shinyjs::hidden() # Close div 
                                 ), # Close fluid row
                                 fluidRow(
                                   column(width = 2),
                                   column(width = 10,
                                          fluidRow(div(id = "staff_ui_chat",
                                                       wellPanel(
                                                         tags$div(id = "staff_chat_area",
                                                                  HTML('<h4>Correspondence</h4>'),
                                                                  uiOutput("staff_chat_message"),
                                                                  tags$div(style="text-align:right;",
                                                                           actionButton("staff_chat_send",
                                                                                        "",
                                                                                        icon("paper-plane"),
                                                                                        style="color: #FFF0F5; background-color: #008080")
                                                                  ),
                                                                  tags$br(),
                                                                  uiOutput("staff_chat_body")
                                                         )
                                                         )
                                                       ) %>% shinyjs::hidden()
                                                   ) # Close fluid row
                                          ) # Close column
                                 )
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
          tabPanel("Rendez-vous accordés",
                   tags$br(),
                   tags$br(),
                   fluidRow(column(width = 12,
                                   title = "Agenda",
                                   DTOutput('rendezvous_ok_table') %>% withSpinner())
                   ), # Close fluid row
                   tags$script(src = "rendezvous_table_module.js"),
                   tags$script(paste0("rendezvous_table_module_js('')")))
        ) # Close tabset panel
      ), # Close conditional panel secretariat
      conditionalPanel(condition = "output.role == 'externe'",
                       tabsetPanel(
                         tabPanel("Mes dossiers",
                                  tags$br(),
                                  tags$br(),
                                  fluidRow(column(width = 2),
                                           column(width = 3,
                                                  actionButton("add_externe",
                                                               "Nouveau dossier",
                                                               class = "btn-success",
                                                               style = "color: #FFF0F5; background-color: #008000",
                                                               icon = icon('plus'))),
                                           column(width = 5),
                                           column(width = 2)),
                                  tags$br(),
                                  fluidRow(column(width = 12,
                                                  title = "Planification",
                                                  DTOutput('externes_table') %>% withSpinner())
                                  ), # Close fluid row
                                  tags$script(src = "externes_table_module.js"),
                                  tags$script(paste0("externes_table_module_js('')"))
                                  ),
                         tabPanel("Fiche du patient", icon = icon("eye"),
                                  tags$br(),
                                  tags$br(),
                                  fluidRow(column(width = 1),
                                           column(width = 10,
                                                  wellPanel(
                                                    h3(textOutput('patient_display_name_ext')),
                                                    h4(textOutput('patient_age_ext')),
                                                    tags$br(),
                                                    tags$br(),
                                                    uiOutput('pathologies_ext'),
                                                    tags$br(),
                                                    uiOutput('syndrome_ext'),
                                                    tags$br(),
                                                    uiOutput('comorbidites_ext'),
                                                    tags$br(),
                                                    uiOutput('description_histoire_ext')
                                                  ),
                                                  wellPanel(
                                                    tags$style(type = 'text/css',
                                                               '.modal-dialog { width: fit-content !important; }'),
                                                    uiOutput('externes_photos_title') %>% withSpinner(),
                                                    tags$br(),
                                                    
                                                    tags$div(id = "externes_photo_container",
                                                             style = "text-align: center;",
                                                             imageOutput("externes_tiffImage")),
                                                    tags$div(id = "externes_arrows_container",
                                                             style = "text-align: center;",
                                                             actionButton("decrease_index_externes",
                                                                          "",
                                                                          icon("arrow-left"),
                                                                          style="color: #FFF0F5; background-color: #3E3F3A"),
                                                             actionButton("increase_index_externes",
                                                                          "",
                                                                          icon("arrow-right"),
                                                                          style="color: #FFF0F5; background-color: #3E3F3A")
                                                    ) # Close div
                                                  ), # Close well panel
                                                  wellPanel(
                                                    tags$div(id = "chat_area_externes",
                                                             HTML('<h4>Correspondence</h4>'),
                                                             textAreaInput('chat_message_externes',
                                                                           '',
                                                                           placeholder = "Ecrivez votre message ici...",
                                                                           value = '',
                                                                           width = '100%',
                                                                           height = '90px'),
                                                             tags$div(style="text-align:right;",
                                                                      actionButton("chat_send_externes",
                                                                                   "",
                                                                                   icon("paper-plane"),
                                                                                   style="color: #FFF0F5; background-color: #008080")
                                                             ),
                                                             tags$br(),
                                                             uiOutput("chat_body_externes")
                                                    ) # Close div chat container
                                                    
                                                  ) # Close well panel
                                           ),
                                           column(width = 1)
                                  ) # Close fluid row
                                  )
                         
                       )
      ) # Close conditional panel externe
    ), # Close Accueil
    tabPanel("About",
             fluidRow(
               column(width = 3),
               column(width = 6,
                      div(style = "padding:30px;background-color:#C5C5C3;color:#3E3F3A;",
                          HTML(printAbout))),
               column(width = 3)
             ) # Close fluid row
             
             )
  )
)