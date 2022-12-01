function(input, output, session) {

  # Everything related to shinyauthr ------------------------------------------
  credentials <- shinyauthr::loginServer(id = "login",
                                         data = user_base,
                                         user_col = user,
                                         pwd_col = password,
                                         sodium_hashed = TRUE,
                                         reload_on_logout = TRUE,
                                         log_out = reactive(logout_init()))
  
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))

  # shinyauthr ----------------------------------------------------------------
  
  # output$user_table <- renderTable({
  #   credentials()$info
  # })
  
  session$userData$username <- reactive({
    if(is.null(credentials()$user_auth)) {
      username <- 'amas'
    } else {
      username <- credentials()$info[['user']]
    }
    username
    })
  
  session$userData$permissions <- reactive({
    if(is.null(credentials()$user_auth)) {
      privilege <- 'user'
    } else {
      privilege <- credentials()$info[['permissions']]
    }
    privilege
  })
  
  output$role <- renderText({
    req(credentials()$user_auth)
    # use is.null(session$user) so it still works when testing locally
    credentials()$info[['permissions']]
    
  })

  
  # Dossiers table ------------------------------------------------------------

  
  # trigger to reload data from the "patients" table
  session$userData$dossiers_trigger <- reactiveVal(0)
  
  
  # Read in Mes Dossiers table from the database
  dossiers <- reactive({
    req(credentials()$user_auth)
    
    session$userData$dossiers_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(modified_by == credentials()$info[['user']],
               is_closed == 0)
      
    }, 
    error = function(err) {
      msg <- "Could not find what you are looking for!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out 
    
  })
  
  dossiers_table_prep <- reactiveVal(NULL)
  
  observeEvent(dossiers(), {
    
    out <- dossiers()
    
    ids <- out$uid
    
    
    if(credentials()$info[['permissions']] == 'admin' || credentials()$info[['permissions']] == 'chef') {
      actions <- purrr::map_chr(ids, function(id_) {
        paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Modifier" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
                     <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Effacer" id = ', id_, ' style="margin: 0"><i class="fa fa-times-circle"></i></button>
                            </div>')
      })
      } else {
        actions <- purrr::map_chr(ids, function(id_) {
          paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Modifier" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
                            </div>')
        })
    }
    
    
    
    # Select relevant columns for the user
    out <- out %>%
      select(nom, prenom, date_naissance, pathologie_1, pre_decision)
    
    # Set the Action Buttons row to the first column of the `dossiers` table
    out <- cbind(tibble(" " = actions),
                 out)
    
    if (is.null(dossiers_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      dossiers_table_prep(out)
      
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without reendering the entire table
      replaceData(dossiers_table_proxy,
                  out,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  })
  
  
  output$dossiers_table <- renderDT({
    req(credentials()$user_auth, dossiers_table_prep())
    
    out <- dossiers_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Pathologie', 'Décision préliminaire'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'tp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Vous n'avez pas de dossiers actifs",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant')),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
    
  })
  
  
  # Fetch data based on row -------- ------------------------------------------
  
  observe({
    if(is.null(input$dossiers_table_rows_selected)) {
      shinyjs::hide("show_contact_details")
    } else {
      shinyjs::show("show_contact_details")
    }    
    })
  
  observe({
    if(is.null(input$archive_table_rows_selected)) {
      shinyjs::hide("archive_show_contact_details")
    } else {
      shinyjs::show("archive_show_contact_details")
    }    
  })
  
  observe({
    if((is.null(input$dossiers_table_rows_selected) && is.null(input$archive_table_rows_selected)) ||
       dossiers_patient_filenames_count() < 1) {
      shinyjs::hide("decrease_index")
      shinyjs::hide("increase_index")
      shinyjs::hide("expand_image")
    } else {
      shinyjs::show("decrease_index")
      shinyjs::show("increase_index")
      shinyjs::show("expand_image")
    }    
  })
  
  observe({
    if(is.null(input$dossiers_table_rows_selected)) {
      
    } else {
      archive_table_proxy %>% selectRows(NULL)
    }     
  })
  
  observe({
    if(is.null(input$archive_table_rows_selected)) {
      
    } else {
      dossiers_table_proxy %>% selectRows(NULL)
    }     
  })
  
  
  patientUID <- reactive({
    
    dossiers()[input$dossiers_table_rows_selected,][[1]]
    
  })
  
  patient_data <- eventReactive(input$dossiers_table_rows_selected, {
    
    patientRow <- NULL
    
    tryCatch({
      patientRow <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(uid == patientUID())
      
    }, 
    error = function(err) {
      msg <- "Could not find that particular patient!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    patientRow
  })

  session$userData$emptyCache <- reactiveVal(0)
  
  observeEvent(input$refresh_images, {
    req(patientUID())
    
    clearCache(patientUID())
    
    session$userData$emptyCache(session$userData$emptyCache() + 1)
    
  })
  
  dossiers_patient_filenames_count <- reactive({
    req(patientUID())
    
    session$userData$emptyCache()
    
    pathToPatientImages <- paste0(tiffDir, patientUID())
    
    if(!file.exists(pathToPatientImages)) {
      
      thisMode = 'test'
      
      filenames <- fetchFiles(patientUID(), 
                              dbInfo[[1]][[2]], 
                              '22', 
                              deviceInfo[[1]][[1]], 
                              deviceInfo[[1]][[2]],
                              thisMode,
                              aLocalDB = localDB)
      
      filename_count <- filenames %>%
        length()
        
      
      fetchPhotos(patientUID(),
                  dbInfo[[1]][[2]],
                  '22',
                  deviceInfo[[1]][[1]],
                  deviceInfo[[1]][[2]],
                  filenames,
                  thisMode,
                  localDB)
    } else {
      
      filename_split <- list.files(pathToPatientImages, pattern = '.tiff') %>%
        str_split('_')
      
      
      if(length(filename_split) == 0) {
        filename_count <- 0
      } else {
        filename_count <- filename_split[[1]][[2]] %>% strtoi()
      }
    }
    
    return(filename_count)
    
  })
  
  output$photos_title <-renderUI({
    req(dossiers_patient_filenames_count())
    
    if(dossiers_patient_filenames_count() == 0) {
      x <- paste0("<h4> Aucune image n'a été trouvée </h4>")
    } else if (dossiers_patient_filenames_count() == 1) {
      x <- paste0("<h4> 1 image trouvée </h4>")
    } else {
      x <- paste0('<h4> ', dossiers_patient_filenames_count(),
                  ' images trouvées </h4>')
      
    }
    
    HTML(x)
    
  })
  

  imgIdx <- 1
  makeReactiveBinding('imgIdx')
  
  observeEvent(input$increase_index, {
    
    if(imgIdx == dossiers_patient_filenames_count()) {
      imgIdx <<- 1
    } else {
      imgIdx <<- imgIdx + 1
    }
  })
  
  observeEvent(input$decrease_index, {
    
    if(imgIdx == 1) {
      imgIdx <<- dossiers_patient_filenames_count()
    } else {
      imgIdx <<- imgIdx - 1
    }
  })
  
  patientPhotos <- reactive({
    req(dossiers_patient_filenames_count())
    
    targetDir <- paste0(tiffDir, patientUID())
                        
    imageFile <- list.files(path = targetDir, pattern = '.tiff')
    
    image_read(paste0(targetDir, '/', imageFile))
    
  })
  
  
  
  output$tiffImage <- renderImage(
    {
      req(dossiers_patient_filenames_count())
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.png')
      
       #width  <- session$clientData$output_tiffImage_width
       #height <- session$clientData$output_tiffImage_height
      
      # Generate the PNG
      #png(outfile, width = 400, height = 300)
      patientPhotos()[imgIdx] %>%
        image_scale(geometry = "x380") %>%
        image_write(path = outfile, format = "png")
      #dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
    
    }, 
    deleteFile = TRUE)
  
  
  # Patient data from dossiers -------------------------------------------------
  
  output$patient_display_name <- renderText({
    
    paste0(patient_data()$prenom, ' ', str_to_upper(patient_data()$nom, locale = 'fr'))
    
  })
  
  output$patient_age <-renderText({
    
    paste0("âgé(e) de ", deliverAge(patient_data()$date_naissance), " ans")
    
  })
  
  observeEvent(input$show_contact_details, {
    showModal(modalDialog(
      title = paste0(patient_data()$prenom, ' ', str_to_upper(patient_data()$nom, locale = 'fr')),
      buildContactCard(patient_data()$phone_number_patient,
                       patient_data()$contact_person, 
                       patient_data()$contact_phone, 
                       patient_data()$contact_email,
                       patient_data()$hopital, 
                       patient_data()$created_at, 
                       patient_data()$created_by, 
                       user_base) %>% HTML(),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  output$pathologies <-renderUI({
    req(patient_data()$pathologie_1)
    
    x <- buildUnorderedList(list(patient_data()$pathologie_1,
                                 patient_data()$pathologie_2,
                                 patient_data()$pathologie_3),
                            "Pathologie(s)")
    
    
    HTML(x)
    
  })
  
  output$coagulation <- renderUI({
    req(patient_data()$treat_coagulant_1)
    
    x <- buildUnorderedList(list(patient_data()$treat_coagulant_1,
                                 patient_data()$treat_coagulant_2,
                                 patient_data()$treat_coagulant_3),
                            "Traitement")
    
    
    HTML(x)
    
  })
  
  output$info_icons <- renderUI({
    req(patient_data())
  
    x <- ''
    if(patient_data()$has_coagulation) {
      x <- paste0(x, '<img src="blood.jpeg" alt="drop" width="17" height="20"/>')
    } 
    
    HTML(x)
    
  })
  

  output$description_histoire <-renderUI({
    req(patient_data()$description_histoire)
    
    x <- buildParagraph(patient_data()$description_histoire, "Histoire")
    
    HTML(x)
    
  })
  
  output$decisions <-renderUI({
    
    x <- buildDecisionBanner(patient_data()$pre_decision, patient_data()$def_decision)
    
    HTML(x)
    
  })
  
  
  # Edit/Delete modules---------------------------------------------------------
  
  dossiers_table_proxy <- DT::dataTableProxy('dossiers_table')
  
  dossiersEditModuleServer("add_dossier",
                           modal_title = "Registrer un nouveau dossier",
                           dossier_to_edit = function() NULL,
                           modal_trigger = reactive({input$add_dossier}),
                           permissions = session$userData$permissions)
  
  
  dossier_to_edit <- eventReactive(input$dossier_id_to_edit, {
    
    dossiers() %>%
      filter(uid == input$dossier_id_to_edit)
    
  })
  
  dossiersEditModuleServer("edit_dossier",
                           modal_title = "Modification du profil",
                           dossier_to_edit = dossier_to_edit,
                           modal_trigger = reactive({input$dossier_id_to_edit}),
                           permissions = session$userData$permissions)
  
  
  dossier_to_delete <- eventReactive(input$dossier_id_to_delete, {
    
    dossiers() %>%
      filter(uid == input$dossier_id_to_delete) %>%
      as.list()
    
  })
  
  
  dossiersDeleteModuleServer("delete_dossier",
                             modal_title = "Effacer profil",
                             dossier_to_delete = dossier_to_delete,
                             modal_trigger = reactive({input$dossier_id_to_delete}))
  
  
  
  # Archive table ------------------------------------------------------------
  
  # trigger to reload data from the archive table
  session$userData$archive_trigger <- reactiveVal(0)
  
  
  # Read in Archive table from the database
  archive_records <- reactive({
    req(credentials()$user_auth)
    
    session$userData$archive_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        arrange(desc(modified_at)) 
      
    }, 
    error = function(err) {
      msg <- "Could not find the record you are looking for!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out 
    
  })
  
  
  archive_table_prep <- reactiveVal(NULL)
  
  observeEvent(archive_records(), {
    
    out <- archive_records()
    
    # Mutate table 
    out <- out %>%
      transmute(nom,
                prenom,
                date_naissance,
                pathologie_1,
                pathologie_2,
                pathologie_3,
                pre_decision,
                def_decision,
                phone_number_patient,
                contact_person,
                contact_phone,
                contact_email,
                hopital,
                has_coagulation,
                treat_coagulant_1,
                treat_coagulant_2,
                treat_coagulant_3,
                created_at,
                created_by,
                modified_at,
                modified_by,
                description_histoire)
    
    if (is.null(archive_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      archive_table_prep(out)
      
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without reendering the entire table
      replaceData(archive_table_proxy,
                  out,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  })
  
  archive_table_proxy <- DT::dataTableProxy('archive_table')
  
  
  output$archive_table <- renderDT({
    req(credentials()$user_auth, archive_table_prep())
    
    out <- archive_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 
                             'Prénom', 
                             'Date de naissance',
                             '1ère pathologie',
                             '2ème pathologie',
                             '3ème pathologie',
                             'Décision préliminaire',
                             'Décision définitive',
                             'Numéro (patient)',
                             'Personne de contact',
                             'Numéro (contact)',
                             'Email (contact)',
                             'Hôpital/Clinique',
                             'Troubles coagulation',
                             'Médicament 1',
                             'Médicament 2',
                             'Médicament 3',
                             'Enregistré(e) le',
                             'Enregistré(e) par',
                             'Dernière modification',
                             'Modifié par',
                             'Histoire'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'ftp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 25,
                               language = list(emptyTable = "Vous n'avez pas de dossiers actifs",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant'),
                                               search = 'Recherche: '))
      ) 
    
  })
  
  # Patient data from archive--------------------------------------------------
  
  archive_patient_data <- eventReactive(input$archive_table_rows_selected, {
    
    patientUID <- archive_records()[input$archive_table_rows_selected,][[1]]
    
    patientRow <- NULL
    tryCatch({
      patientRow <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(uid == patientUID)
      
    }, 
    error = function(err) {
      msg <- "Could not find that particular patient in archive!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    patientRow
  })
  
  
  # Patient data from archive UI -----------------------------------------------
  
  output$archive_patient_display_name <- renderText({
    
    paste0(archive_patient_data()$prenom, ' ', str_to_upper(archive_patient_data()$nom, locale = 'fr'))
    
  })
  
  output$archive_patient_age <-renderText({
    
    paste0("âgé(e) de ", deliverAge(archive_patient_data()$date_naissance), " ans")
    
  })
  
  observeEvent(input$archive_show_contact_details, {
    showModal(modalDialog(
      title = paste0(archive_patient_data()$prenom, ' ', str_to_upper(archive_patient_data()$nom, locale = 'fr')),
      buildContactCard(archive_patient_data()$phone_number_patient,
                       archive_patient_data()$contact_person, 
                       archive_patient_data()$contact_phone, 
                       archive_patient_data()$contact_email,
                       archive_patient_data()$hopital, 
                       archive_patient_data()$created_at, 
                       archive_patient_data()$created_by, 
                       user_base) %>% HTML(),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  output$archive_pathologies <-renderUI({
    req(archive_patient_data()$pathologie_1)
    
    x <- buildUnorderedList(list(archive_patient_data()$pathologie_1,
                                 archive_patient_data()$pathologie_2,
                                 archive_patient_data()$pathologie_3),
                            "Pathologie(s)")
    
    
    HTML(x)
    
  })
  
  output$archive_coagulation <-renderUI({
    req(archive_patient_data()$treat_coagulant_1)
    
    x <- buildUnorderedList(list(archive_patient_data()$treat_coagulant_1,
                                 archive_patient_data()$treat_coagulant_2,
                                 archive_patient_data()$treat_coagulant_3),
                            "Traitement")
    
    
    HTML(x)
    
  })
  
  output$archive_info_icons <- renderUI({
    req(archive_patient_data())
    
    x <- ''
    if(archive_patient_data()$has_coagulation) {
      x <- paste0(x, '<img src="blood.jpeg" alt="drop" width="17" height="20"/>')
    } 
    
    HTML(x)
    
  })
  
  output$archive_description_histoire <-renderUI({
    req(archive_patient_data()$description_histoire)
    
    x <- buildParagraph(archive_patient_data()$description_histoire, "Histoire")
    
    HTML(x)
    
  })
  
  output$archive_decisions <-renderUI({
    
    x <- buildDecisionBanner(archive_patient_data()$pre_decision, archive_patient_data()$def_decision)
    
    HTML(x)
    
  })
  
  # Garde table ------------------------------------------------------------
  
  # trigger to reload data from the "garde" table
  session$userData$garde_trigger <- reactiveVal(0)
  
  
  # Read in Mes Dossiers table from the database
  garde <- reactive({
    req(credentials()$user_auth)
    
    session$userData$garde_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('garde') %>%
        collect() %>%
        mutate(modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        slice_head(n = 1)
      
    }, 
    error = function(err) {
      msg <- "Could not find the garde you are looking for!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out 
    
  })
  
  garde_table_prep <- reactiveVal(NULL)
  
  observeEvent(garde(), {
    
    out <- garde()
    
    # Select relevant columns for the user
    out <- out %>%
      transmute(`En garde maintenant:` = convertUsernameToDisplayname(modified_by, user_base))
      

    
    if (is.null(garde_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      garde_table_prep(out)
      
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without reendering the entire table
      replaceData(garde_table_proxy,
                  out,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  })
  
  garde_table_proxy <- DT::dataTableProxy('garde_table')
  
  output$garde_table <- renderDT({
    req(garde_table_prep())
    
    out <- garde_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('En garde maintenant:'),
                class = "compact stripe nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = FALSE,
                               dom = 't',
                               columnDefs = list(list(className = 'dt-center', targets = 0)),
                               initComplete = jsHeader,
                               language = list(emptyTable = "Personne n'a pris la garde"))
      ) 
    
  })
  
  observeEvent(input$take_garde, {
    
    tryCatch({
      
      thisQuery <- writeGardeQuery(credentials()$info[['user']],
                                   ymd_hms(Sys.time()))
      
      dbExecute(conn, thisQuery)
      
      session$userData$garde_trigger(session$userData$garde_trigger() + 1)
      
      showToast("success", message = "Vous avez bien pris la garde")}, 
      
      error = function(error) {
        
        msg <- paste0("Erreur - contactez votre admin")
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    ) # Close try-catch
  })

  patient_data_staff <- eventReactive(input$staff_meeting, {

    out <- NULL

    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(is_closed == 0)

    },
    error = function(err) {
      msg <- "Could not find that particular patient!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })

    out
  })
  
  #patient_data_staff()
  
  #names_patients_staff <- composeNameAndAge(patient_data_staff())
  
  example_text <- eventReactive(input$staff_meeting, {
    "Show me!"
  })
  
  observeEvent(input$toggleButton, {
    shinyjs::toggle("main")
  })
  
  output$example_1 <- renderText({example_text()})
 
  # set suspendWhenHidden to FALSE so it renders even without output
  outputOptions(output, 'role', suspendWhenHidden = FALSE) 
  
}
