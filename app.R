#------------------------------------------------------------------------------
# Requirements
#------------------------------------------------------------------------------
require(shiny)
require(shinyjs)
require(shinycssloaders)
library(shinyFeedback)
require(shinythemes)
require(shinyauthr)
require(sodium)
require(dplyr)
library(dbplyr)
require(DT)
require(stringr)
require(lubridate)
library(DBI)
library(RSQLite)

#-------------------------------------------------------------------------------
# javascript functions
#-------------------------------------------------------------------------------
jscHeader <- "function(settings, json) {
$(this.api().table().header()).css({'background-color': '#006400', 'color': '#F8F8FF'});
}"

#------------------------------------------------------------------------------
# Hard-coded data
#------------------------------------------------------------------------------
dateStamp <- stamp("Garde du 26/9/2022")
toDay <- today() %>% ymd() %>% dateStamp()

user_base <- tibble(
  user = c("zaldijn001", "zaldijn002", "zaldipe001"),
  password = c("pendejouw", "pendejouw", "pendejobrouw"),
  permissions = c("admin", "end user", "standard"),
  name = c("Julian Zaldivar", "Julien Zaldivar", "Pierre Zaldivar"))

ward <- tibble(
  Nom = c("Vera", "Rodriguez", "Verdu", "Monge", "Pereda", "Aguilar"),
  `Prénom` = c("Daniel", "Erika", "Anaïs", "Andrea", "Valentín", "Paola"),
  `Sexe` = c("M", "F", "F", "F", "M", "F"),
  `Date de naissance` = c("13/05/1984", "18/05/1985", "10/02/1986", "02/12/1985", "22/09/1984", "02/11/1986"),
  `Décision` = c("Hospitalisé", "Chirurgie", "Hospitalisé", "A rappatrier", "", "Hospitalisé"),
  `Date de registre` = c("09/03/2022", "08/03/2022", "09/03/2022", "09/03/2022", "09/03/2022", "08/03/2022"),
  `Dernière modification` = c("09/03/2022", "08/03/2022", "09/03/2022", "09/03/2022", "09/03/2022", "08/03/2022")) 

# displayMessage <- function(aFile) {
#   
#   ext <- tools::file_ext(aFile$datapath) 
#   
#   Sys.sleep(1.5)
#   
#   paste0('This is a clasic ', ext, ' file!')
# }

#------------------------------------------------------------------------------
# UI
#------------------------------------------------------------------------------

ui <- navbarPage(
  title = "Pendejapp",
  collapsible = TRUE,
  windowTitle = "Garde",
  theme = shinytheme("journal"),        
  
  tabPanel(title = "Home",
           # add logout button UI
           div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
           # add login panel UI function
           shinyauthr::loginUI(id = "login",
                               title = "ZaldiBase",
                               user_title = "Identifiant",
                               pass_title = "Mot de passe",
                               login_title = "Se connecter",
                               error_message = "Identifiant ou mot de passe invalides!"),
           
           htmlOutput("notification") 
  ),
  tabPanel(title = "Garde",
           dateInput("dateDeGarde",
                     "Date de garde",
                     value = today(),
                     format = "dd/mm/yyyy",
                     language = "fr"),
           HTML("<br><br><br>"),
           fileInput("aFile",
                     "patientPic",
                     accept = "image/*",
                     buttonLabel = "Mettre en ligne"),
           HTML("<br><br><br>"),
           DTOutput("tabelleGarde"),
           
           
  )
)

#------------------------------------------------------------------------------
# Server
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(id = "login",
                                         data = user_base,
                                         user_col = user,
                                         pwd_col = password,
                                         log_out = reactive(logout_init()))
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  
  output$notification <- renderUI({
    req(credentials()$user_auth)
    
    #Sys.sleep(1.5)
    
    div(
      class = "bg-success",
      id = "success_basic",
      h5(paste0("One pendejo with ", credentials()$info$permissions, " rights currently logged in!")),
      p(" Have a nice day ", credentials()$info$name ," ! ")
    )
    
  })
  
  output$userInfo <- renderTable({
    req(credentials()$user_auth)
    
    credentials()$info
    
  })
  
  output$tabelleGarde <- renderDT({
    req(credentials()$user_auth)
    
    ward %>%
      filter(dmy(`Dernière modification`) == input$dateDeGarde) %>%
      datatable(caption = tags$caption(dateStamp(input$dateDeGarde), style = "color:#006400"),
                options = list(scrollX = FALSE,
                               dom = 't',
                               ordering = FALSE,
                               initComplete = JS(jscHeader)),
                rownames = FALSE)
    
  })
  
  # thisMessage <- reactive({
  #   file <- input$aFile
  #   
  #   
  #   req(file)
  #   displayMessage(file)
  #   
  # })
  # 
  # 
  # observeEvent(!(thisMessage()==""), {
  #   showModal(modalDialog(
  #     title = "Mededeling",
  #     thisMessage(),
  #     easyClose = TRUE,
  #     footer = modalButton(label = "Check!" , icon = icon('thumbs-up', lib = "font-awesome"))
  #   ))
  # })
  
}





#------------------------------------------------------------------------------
# Run App
#------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)