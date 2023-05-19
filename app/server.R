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
    session$userData$staff_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(is_closed == 0, status < 30)
      
    }, 
    error = function(err) {
      msg <- "Could not find the dossiers you are looking for!"
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
    
    
    if(credentials()$info[['permissions']] == 'admin') {
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
      transmute(nom, prenom, date_naissance, pathologie_1, displayStatusName(status))
    
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
                             'Pathologie', 'Status'),
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
  
  
  # Read in Mes Dossiers table from the database
  notifications <- reactive({
    req(credentials()$user_auth)
    
    dbTimer()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('pending') %>%
        collect() %>%
        distinct(uid, .keep_all = TRUE)
      
    }, 
    error = function(err) {
      msg <- "Could not find pending notifications!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out 
    
    #print(out)
    
  })
  
  observe({
    if(nrow(notifications()) < 1) {
      shinyjs::hide("notifications_container")
    } else {
      shinyjs::show("notifications_container")
    }
  })
  
  
  output$notifications_table <- renderDT({
    req(credentials()$user_auth)
    
    notifications() %>%
      select(name) %>%
      datatable(rownames = FALSE,
                colnames = c(''),
                selection = "none",
                class = "compact row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 't',
                               columnDefs = list(list(targets = 0, orderable = FALSE),
                                                 list(className = 'dt-center', targets = 0)),
                               language = list(emptyTable = "Vous n'avez pas de notifications"),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
  })
  
  # Fetch data based on row -------- ------------------------------------------
  
  observe({
    if(is.null(input$dossiers_table_rows_selected)) {
      shinyjs::hide("show_contact_details")
      shinyjs::hide("arrows_container")
      shinyjs::hide("refresh_images")
    } else {
      archive_table_proxy %>% selectRows(NULL)
      shinyjs::show("photo_container")
      shinyjs::hide("archive_photo_container")
      shinyjs::show("show_contact_details")
      shinyjs::show("refresh_images")
    }    
    })
  
  observe({
    if(is.null(input$archive_table_rows_selected)) {
      shinyjs::hide("archive_show_contact_details")
      shinyjs::hide("archive_arrows_container")
      shinyjs::hide("archive_refresh_images")
    } else {
      dossiers_table_proxy %>% selectRows(NULL)
      shinyjs::show("archive_show_contact_details")
      shinyjs::show("archive_refresh_images")
      shinyjs::hide("photo_container")
      shinyjs::show("archive_photo_container")
    }    
  })
  
  observeEvent(dossiers_patient_filenames_count(), {
    if(dossiers_patient_filenames_count() < 1){
      shinyjs::hide("arrows_container")
    } else {
      shinyjs::show("arrows_container")
    }
  })
  
  observeEvent(archive_patient_filenames_count(), {
    if(archive_patient_filenames_count() < 1) {
      shinyjs::hide("archive_arrows_container")
    } else {
      shinyjs::show("archive_arrows_container")
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
      if(dossiers_patient_filenames_count() == 0) {
        index <- 1
      } else {
        index <- imgIdx
      }
      
      patientPhotos()[index] %>%
        image_scale(geometry = "x380") %>%
        image_write(path = outfile, format = "png")
      
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
    
    }, 
    deleteFile = TRUE)
  
  observeEvent(input$expand_image, {
    
    clearTmpImgs()
    
    timeSuffix <- Sys.time() %>% 
      gsub("^[^\\s]+\\s", "", .) %>%
      str_replace_all(":", "")
    
    patientPhotos()[imgIdx] %>%
      image_scale(geometry = "x780") %>%
      image_write(path = paste0(tmpImg, timeSuffix), format = "png")
    
  
    showModal(
      modalDialog(
        HTML(paste0('<img src="tmpimg', timeSuffix, '">')),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  
  # Patient data from dossiers -------------------------------------------------
  
  output$patient_display_name <- renderText({
    
    if(patient_data()$prenom == '' | patient_data()$nom == '') {
      return("Patient inconnu")
    } else {
      return(writePatientDisplayName(patient_data()$prenom, patient_data()$nom))
    }
  })
  
  output$patient_age <-renderText({
    
    if(patient_data()$date_naissance == '') {
      return("Âge inconnu")
    } else {
      return(paste0("âgé(e) de ", deliverAge(patient_data()$date_naissance,
                                      patient_data()$created_at), " ans"))
    }
  })
  
  
  output$info_icons <- renderUI({
    req(patient_data())
    
    iconCoagulation <- ''
    textCoagulation <- ''
    iconPediatrician <- ''
    textPediatrician <- ''
    iconSyndrome <- ''
    textSyndrome <- ''
    
    if(patient_data()$has_coagulation) {
      iconCoagulation <- '<img src="blood.jpeg" alt="drop" width="27" height="31.5"/>'
      textCoagulation <- buildTreatmentBanner(patient_data()$treat_coagulant_1,
                                              patient_data()$date_derniere_prise_1,
                                              patient_data()$treat_coagulant_2,
                                              patient_data()$date_derniere_prise_2,
                                              patient_data()$treat_coagulant_3,
                                              patient_data()$date_derniere_prise_3)
                  
    } 
    
    if(!(patient_data()$date_naissance == '')) {
      if(deliverAge(patient_data()$date_naissance, patient_data()$created_at) < 18) {
        iconPediatrician <- '<img src="child_icon.jpeg" alt="child" height="30"/>'
        textPediatrician <- "<h4><b>Pédiatrie</b></h4>"
      }
    } 
    
    if(!(patient_data()$syndrome == '')) {
      iconSyndrome <- '<img src="bugs_icon.png" alt="infection" height="30"/>'
      textSyndrome <- "<h4><b>Infection/Syndrome inflammatoire</b></h4>"
    } 
    
    x <- paste0('<table style="width:100%">
      <tr>
      <th></th>
      <th style="width:95%;"></th>
      </tr>
      <tr>
      <td style="text-align:center;">', iconCoagulation, '</td>
      <td style="text-align:left;">', textCoagulation, '</td>
      </tr>
      <tr>
      <td style="text-align:center;">', iconPediatrician, '</td>
      <td style="text-align:left;">', textPediatrician, '</td>
      </tr>
      <td style="text-align:center;">', iconSyndrome, '</td>
      <td style="text-align:left;">', textSyndrome, '</td>
      </tr>
      </table> ')
    
    HTML(x)
    
  })
  
  output$decisions <-renderUI({
    
    x <- buildDecisionBanner(patient_data()$pre_decision, patient_data()$def_decision)
    
    HTML(x)
    
  })
  
  output$dossier_metadata <-renderUI({
    req(patient_data())
    
    tryCatch({
      out <- conn %>%
        tbl('garde') %>%
        collect() %>%
        filter(garde_id == patient_data()$garde_id)
      
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
    
    
    garde <- deliverGardeDisplayName(out$modified_by, out$partner, user_base)
    author <- convertUsernameToDisplayname(patient_data()$created_by, user_base)
    timestamp <- displaySimpleDateTime(patient_data()$created_at)

    x <- buildMetadataBanner(author,
                             patient_data()$hopital,
                             timestamp,
                             garde)

    HTML(x)
    
  })
  
  output$dossier_status <-renderUI({
    req(patient_data())
    
    statusDisplayName <- patient_data()$status %>%
      displayStatusName()
    
    x <- paste0('<b><h4 style="color:#800000;text-align:right">Status : ', statusDisplayName,'</h4></b>')
      
    HTML(x)
  })
  
  observeEvent(input$show_contact_details, {
    showModal(modalDialog(
      title = writePatientDisplayName(patient_data()$prenom, patient_data()$nom),
      buildContactCard(patient_data()$phone_number_patient,
                       patient_data()$email_patient,
                       patient_data()$contact_person, 
                       patient_data()$contact_phone, 
                       patient_data()$contact_email,
                       patient_data()$hopital) %>% HTML(),
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
  
  output$syndrome <-renderUI({
    req(patient_data()$syndrome)
    
    x <- buildUnorderedList(list(patient_data()$syndrome),
                            "Syndrome inflammatoire ou infectieux actif")
    
    HTML(x)
    
  })
  
  output$comorbidites <-renderUI({
    
    patient_data() %>%
      deliverComorbiditeTibble() %>%
      buildComorbiditeTable() %>%
      HTML()
    
  })
  
  
  output$description_histoire <-renderUI({
    req(patient_data()$description_histoire)
    
    x <- buildParagraph(patient_data()$description_histoire, "Histoire")
    
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
  
  archivePatientUID <- reactive({
    
    archive_records()[input$archive_table_rows_selected,][[1]]
    
  })
  
  archive_patient_data <- eventReactive(input$archive_table_rows_selected, {
    
    patientRow <- NULL
    tryCatch({
      patientRow <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(uid == archivePatientUID())
      
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
    
    if(archive_patient_data()$prenom == '' | archive_patient_data()$nom == '') {
      return("Patient inconnu")
    } else {
      return(writePatientDisplayName(archive_patient_data()$prenom, archive_patient_data()$nom))
    }
    
  })
  
  output$archive_patient_age <-renderText({
    
    if(archive_patient_data()$date_naissance == '') {
      return("Âge inconnu")
    } else {
      return(paste0("âgé(e) de ", deliverAge(archive_patient_data()$date_naissance,
                                             archive_patient_data()$created_at), " ans"))
    }
  })
  
  observeEvent(input$archive_show_contact_details, {
    showModal(modalDialog(
      title = writePatientDisplayName(archive_patient_data()$prenom, archive_patient_data()$nom),
      buildContactCard(archive_patient_data()$phone_number_patient,
                       archive_patient_data()$email_patient,
                       archive_patient_data()$contact_person, 
                       archive_patient_data()$contact_phone, 
                       archive_patient_data()$contact_email,
                       archive_patient_data()$hopital) %>% HTML(),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  observe({
    if(identical(patientUID(), character(0))) {
      shinyjs::hide("mark_as_opere")
    } else {
      if(patient_data()$status == 9) {
        shinyjs::show("mark_as_opere")
      } else {
        shinyjs::hide("mark_as_opere")
      }
    }    
  })
  
  observeEvent(input$mark_as_opere, {
    
    tryCatch({
      
      opereQuery <- writeOperationFinieQuery(patient_data()$uid)
      
      dbExecute(conn, opereQuery)
      
      session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
      
      showToast("success", message = "Patient a été opéré. Vous pouvez quitter la fiche...")}, 
      
      error = function(error) {
        
        msg <- paste0("Erreur pendant le changement - contactez votre admin")
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    )
  })
  
  output$archive_metadata <-renderUI({
    req(archive_patient_data())
    
    tryCatch({
      out <- conn %>%
        tbl('garde') %>%
        collect() %>%
        filter(garde_id == archive_patient_data()$garde_id)
      
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
    
    
    garde <- deliverGardeDisplayName(out$modified_by, out$partner, user_base)
    author <- convertUsernameToDisplayname(archive_patient_data()$created_by, user_base)
    timestamp <- displaySimpleDateTime(archive_patient_data()$created_at)
    
    x <- buildMetadataBanner(author,
                             archive_patient_data()$hopital,
                             timestamp,
                             garde)
    
    HTML(x)
    
  })
  
  output$archive_status <-renderUI({
    req(archive_patient_data())
    
    statusDisplayName <- archive_patient_data()$status %>%
      displayStatusName()
    
    x <- paste0('<b><h4 style="color:#800000;text-align:right">Status : ', statusDisplayName,'</h4></b>')
    
    HTML(x)
  })
  
  output$archive_pathologies <-renderUI({
    req(archive_patient_data()$pathologie_1)
    
    x <- buildUnorderedList(list(archive_patient_data()$pathologie_1,
                                 archive_patient_data()$pathologie_2,
                                 archive_patient_data()$pathologie_3),
                            "Pathologie(s)")
    
    
    HTML(x)
    
  })
  
  output$archive_syndrome <-renderUI({
    req(archive_patient_data()$syndrome)
    
    x <- buildUnorderedList(list(archive_patient_data()$syndrome),
                            "Syndrome inflammatoire ou infectieux actif")
    
    HTML(x)
    
  })
  
  output$archive_comorbidites <-renderUI({
    
    archive_patient_data() %>%
      deliverComorbiditeTibble() %>%
      buildComorbiditeTable() %>%
      HTML()
    
  })
  
  
  output$archive_info_icons <- renderUI({
    req(archive_patient_data())
    
    iconCoagulation <- ''
    textCoagulation <- ''
    iconPediatrician <- ''
    textPediatrician <- ''
    iconSyndrome <- ''
    textSyndrome <- ''
    
    if(archive_patient_data()$has_coagulation) {
      iconCoagulation <- '<img src="blood.jpeg" alt="drop" width="27" height="31.5"/>'
      textCoagulation <- buildTreatmentBanner(archive_patient_data()$treat_coagulant_1,
                                              archive_patient_data()$date_derniere_prise_1,
                                              archive_patient_data()$treat_coagulant_2,
                                              archive_patient_data()$date_derniere_prise_2,
                                              archive_patient_data()$treat_coagulant_3,
                                              archive_patient_data()$date_derniere_prise_3)
      
    } 
    
    if(!(archive_patient_data()$date_naissance == '')) {
      if(deliverAge(archive_patient_data()$date_naissance, archive_patient_data()$created_at) < 18) {
        iconPediatrician <- '<img src="child_icon.jpeg" alt="child" height="30"/>'
        textPediatrician <- "<h4><b>Pédiatrie</b></h4>"
      }
    } 
    
    if(!(archive_patient_data()$syndrome == '')) {
      iconSyndrome <- '<img src="bugs_icon.png" alt="infection" height="30"/>'
      textSyndrome <- "<h4><b>Infection/Syndrome inflammatoire</b></h4>"
    } 
    
    x <- paste0('<table style="width:100%">
      <tr>
      <th></th>
      <th style="width:95%;"></th>
      </tr>
      <tr>
      <td style="text-align:center;">', iconCoagulation, '</td>
      <td style="text-align:left;">', textCoagulation, '</td>
      </tr>
      <tr>
      <td style="text-align:center;">', iconPediatrician, '</td>
      <td style="text-align:left;">', textPediatrician, '</td>
      </tr>
      <td style="text-align:center;">', iconSyndrome, '</td>
      <td style="text-align:left;">', textSyndrome, '</td>
      </tr>
      </table> ')
    
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
  
  observeEvent(input$archive_refresh_images, {
    req(archivePatientUID())
    
    clearCache(archivePatientUID())
    
    session$userData$emptyCache(session$userData$emptyCache() + 1)
    
  })
  
  archive_patient_filenames_count <- reactive({
    req(archivePatientUID())
    
    session$userData$emptyCache()
    
    pathToPatientImages <- paste0(tiffDir, archivePatientUID())
    
    if(!file.exists(pathToPatientImages)) {
      
      thisMode = 'test'
      
      filenames <- fetchFiles(archivePatientUID(), 
                              dbInfo[[1]][[2]], 
                              '22', 
                              deviceInfo[[1]][[1]], 
                              deviceInfo[[1]][[2]],
                              thisMode,
                              aLocalDB = localDB)
      
      filename_count <- filenames %>%
        length()
      
      
      fetchPhotos(archivePatientUID(),
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
  
  archivePatientPhotos <- reactive({
    req(archive_patient_filenames_count())
    
    targetDir <- paste0(tiffDir, archivePatientUID())
    
    imageFile <- list.files(path = targetDir, pattern = '.tiff')
    
    image_read(paste0(targetDir, '/', imageFile))
    
  })
  
  output$archive_photos_title <-renderUI({
    req(archive_patient_filenames_count())
    
    if(archive_patient_filenames_count() == 0) {
      x <- paste0("<h4> Aucune image n'a été trouvée </h4>")
    } else if (archive_patient_filenames_count() == 1) {
      x <- paste0("<h4> 1 image trouvée </h4>")
    } else {
      x <- paste0('<h4> ', archive_patient_filenames_count(),
                  ' images trouvées </h4>')
      
    }
    
    HTML(x)
    
  })
  
  archiveImgIdx <- 1
  makeReactiveBinding('archiveImgIdx')
  
  observeEvent(input$increase_archive_index, {
    
    if(archiveImgIdx == archive_patient_filenames_count()) {
      archiveImgIdx <<- 1
    } else {
      archiveImgIdx <<- archiveImgIdx + 1
    }
  })
  
  observeEvent(input$decrease_archive_index, {
    
    if(archiveImgIdx == 1) {
      archiveImgIdx <<- archive_patient_filenames_count()
    } else {
      archiveImgIdx <<- archiveImgIdx - 1
    }
  })
  
  
  output$archive_tiffImage <- renderImage(
    {
      req(archive_patient_filenames_count())
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.png')
      
      # Generate the PNG
      if(archive_patient_filenames_count() == 0) {
        index <- 1
      } else {
        index <- archiveImgIdx
      }
      
      archivePatientPhotos()[index] %>%
        image_scale(geometry = "x380") %>%
        image_write(path = outfile, format = "png")
      #dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
      
    }, 
    deleteFile = TRUE)
  
  observeEvent(input$expand_archive_image, {
    
    clearTmpImgs()
    
    timeSuffix <- Sys.time() %>% 
      gsub("^[^\\s]+\\s", "", .) %>%
      str_replace_all(":", "")
    
    archivePatientPhotos()[archiveImgIdx] %>%
      image_scale(geometry = "x780") %>%
      image_write(path = paste0(tmpImg, timeSuffix), format = "png")
    
    
    showModal(
      modalDialog(
        HTML(paste0('<img src="tmpimg', timeSuffix, '">')),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observe({
    if(identical(archivePatientUID(), character(0))) {
      shinyjs::hide("reopen_from_archive")
    } else {
      if(archive_patient_data()$is_closed == 1) {
        shinyjs::show("reopen_from_archive")
      } else {
        shinyjs::hide("reopen_from_archive")
      }
    }    
  })
  
  observeEvent(input$reopen_from_archive, {
    
    showModal(modalDialog(
      div(style = "padding: 30px;", class = "text-center",
          HTML(printReopenDossierExplanation)),
      easyClose = FALSE,
      size = 'l',
      footer = list(modalButton('Annuler'),
                    actionButton('submit_reopen',
                                 'Réouvrir',
                                 class = "btn btn-primary mb1 bg-olive"))))
    
  }) # Close observe event
  
  observeEvent(input$submit_reopen, {
    
    removeModal()
    
    tryCatch({
      
      reopenQuery <- writeReopenDossierQuery(archive_patient_data(),
                                             session$userData$username())

      dbExecute(conn, reopenQuery)
      
      session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
      
      showToast("success", message = "Le dossier a bien été réouvert")}, 
      
      error = function(error) {
        
        msg <- paste0("Erreur pendant la réouverture - contactez votre admin")
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    )
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
      pivot_longer(!modified_at & !garde_id, names_to = "action", values_to = "garde") %>%
      transmute(Engarde = convertToDisplayName(garde))

    
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
                options = list(scrollX = FALSE,
                               dom = 't',
                               columnDefs = list(list(className = 'dt-center', targets = 0),
                                                 list(orderable = FALSE)),
                               initComplete = jsHeader,
                               language = list(emptyTable = "Personne n'a pris la garde"))
      ) 
    
  })
  
  observeEvent(input$take_garde, {
    
    if(session$userData$permissions() == 'admin') {
      showModal(modalDialog(
        div(style = "padding: 30px;", class = "text-center",
            HTML(printWarningHighPrivileges)),
        title = "Privilège trop élevé",
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    } else {
      
      gardeChoices <- deliverGardeChoices(session$userData$permissions(),
                                          user_base)
      
      gardeChoices <- c(" ", gardeChoices)
      
      showModal(modalDialog(
        div(style = "padding: 30px;", class = "text-center",
            HTML(printTakeGarde),
            selectInput("garde_avec",
                        "Je prends la garde avec : ",
                        choices = gardeChoices,
                        selected = " ")),
        easyClose = FALSE,
        size = 'l',
        footer = list(modalButton('Annuler'),
                      actionButton('submit_garde',
                                   'Soumettre',
                                   class = "btn btn-primary mb1 bg-olive"))))
      
       # Close try-catch
    } # Close else
    
  }) # Close observe event
  
  observeEvent(input$garde_avec, {
    if (input$garde_avec == " ") {
      shinyFeedback::showFeedbackDanger("garde_avec",
                                        text = "Choisissez un collègue!")
      shinyjs::disable('submit_garde')
    } else {
      shinyFeedback::hideFeedback("garde_avec")
      shinyjs::enable('submit_garde')
    }
  })
  
  observeEvent(input$submit_garde, {
    
    removeModal()
    
    tryCatch({
      
      thisQuery <- writeGardeQuery(credentials()$info[['user']],
                                   input$garde_avec,
                                   ymd_hms(Sys.time()))
      
      dbExecute(conn, thisQuery)
      
      session$userData$garde_trigger(session$userData$garde_trigger() + 1)
      
      showToast("success", message = "Vous avez bien pris la garde")
      
      
      # output$report <- downloadHandler(
      #   
      #   filename = "report.pdf",
      #   content = function(file) {
      #     
      #     tempReport <- file.path(tmpDir)
      #     file.copy(list.files(getwd()), tempReport, overwrite = TRUE, recursive = TRUE)
      #     
      #     params <- list(species = input$garde_avec,
      #                    island = input$garde_avec,
      #                    sex = input$garde_avec
      #     )
      #     quarto::quarto_render("data/Report.qmd",
      #                           output_file = file,
      #                           execute_params = params
      #     )
      #   }
      # )
      
      }, 
      
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
    )
  })

  # Staff meeting ------------------------------------------------------------
  
  session$userData$staff_trigger <- reactiveVal(0)
  
  patient_data_staff <- eventReactive(input$staff_meeting, {

    session$userData$staff_trigger()
    session$userData$dossiers_trigger()
    
    out <- NULL

    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(is_closed == 0 | is_viewed == 0) %>%
        filter(status < 30)

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
  
  patient_data_staff_count <- reactive({
    
    nrow(patient_data_staff())
    
  })
  
  observe({
    if(nrow(dossiers()) == 0 || patient_data_staff_count() == 0) {
      shinyjs::hide("staff_meeting")
    } else {
      shinyjs::show("staff_meeting")
    }    
  })
  
  patientIdx <- 1
  makeReactiveBinding('patientIdx')
  
  observeEvent(input$increase_patient_index, {
    
    if(patientIdx == patient_data_staff_count()) {
      patientIdx <<- 1
    } else {
      patientIdx <<- patientIdx + 1
    }
  })
  
  observeEvent(input$decrease_patient_index, {
    
    if(patientIdx == 1) {
      patientIdx <<- patient_data_staff_count()
    } else {
      patientIdx <<- patientIdx - 1
    }
  })
  
  observeEvent(input$increase_patient_index_2, {
    
    if(patientIdx == patient_data_staff_count()) {
      patientIdx <<- 1
    } else {
      patientIdx <<- patientIdx + 1
    }
    
    shinyjs::runjs("window.scrollTo(0,0)")
  })
  
  observeEvent(input$decrease_patient_index_2, {
    
    if(patientIdx == 1) {
      patientIdx <<- patient_data_staff_count()
    } else {
      patientIdx <<- patientIdx - 1
    }
    
    shinyjs::runjs("window.scrollTo(0,0)")
  })
  
  staff_decision_names <- reactive(paste0("staff_decision_", seq_len(patient_data_staff_count())))
  
  output$staff_decisions <- renderUI({
    req(patient_data_staff())
    
    decision_input_controllers <- map(staff_decision_names(), ~ selectInput(.x, 
                                              "Décision du staff",
                                              selected = isolate(ifelse(is.null(input[[.x]]), 
                                                                        patient_data_staff()$staff_decision[[patientIdx]], 
                                                                        input[[.x]])),
                                              choices = decisions_dev))
      decision_input_controllers[[patientIdx]]
  })
  
  staff_explanation_names <- reactive(paste0("staff_explanation_", seq_len(patient_data_staff_count())))
  
  output$staff_decision_explanations <- renderUI({
    req(patient_data_staff())
    
    explanation_input_controllers <- map(staff_explanation_names(), ~ textAreaInput(.x, 
                                                                                    "Explication",
                                                                                    value = isolate(ifelse(is.null(input[[.x]]), 
                                                                                                           patient_data_staff()$explication[[patientIdx]], 
                                                                                                           input[[.x]])),
                                                                                    width = '100%',
                                                                                    height = '140px'))
    explanation_input_controllers[[patientIdx]]
  })
  
  checkVectorDecisions <- reactive({
    map(staff_decision_names(), ~ input[[.x]]) %>%
      convertToAdjustedVector(patient_data_staff_count())

  })
  
  checkVectorExplanations <- reactive({
    map(staff_explanation_names(), ~ input[[.x]]) %>%
      convertToAdjustedVector(patient_data_staff_count())
    
  })
  
  overviewTable <- reactive({
    req(checkVectorDecisions(), patient_data_staff())
    
    patient_data_staff() %>%
      transmute(displayName = if_else(prenom == '' | nom == '', '(Inconnu)', writePatientDisplayName(prenom, nom)),
                dec = decisionIsValid(checkVectorDecisions(), patient_data_staff_count()),
                expl = explanationIsValid(checkVectorDecisions(), checkVectorExplanations(), patient_data_staff()$status),
                valid = dossierIsReviewed(dec, expl),
                tickedName = tickName(as.logical(valid), displayName))
    
  })
  

  output$patient_overview <- renderDT({
    req(overviewTable())
    
    overviewTable() %>%
      select(tickedName) %>%
      datatable(rownames = FALSE,
                colnames = c('Aperçu patients'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,
                options = list(scrollX = TRUE,
                               dom = 't',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Aucun dossier ouvert"),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      )
    
  })

  observe({
    if(!is.null(input$patient_overview_rows_selected)) {
      patientIdx <<- input$patient_overview_rows_selected
    }
  }) 
  
  output$staff_patient_display_name <- renderText({
    req(patient_data_staff())
    
    if(patient_data_staff()$prenom[[patientIdx]] == '' | patient_data_staff()$nom[[patientIdx]] == '') {
      return("Patient inconnu")
    } else {
      return(writePatientDisplayName(patient_data_staff()$prenom[[patientIdx]],
                                     patient_data_staff()$nom[[patientIdx]]))
    }
  })
  
  output$staff_patient_age <-renderText({
    req(patient_data_staff())
    
    if(patient_data_staff()$date_naissance[[patientIdx]] == '') {
      return("Âge inconnu")
    } else {
      return(paste0("âgé(e) de ", deliverAge(patient_data_staff()$date_naissance[[patientIdx]],
                                             patient_data_staff()$created_at[[patientIdx]]), " ans"))
    }
  })
  
  output$staff_info_icons <- renderUI({
    req(patient_data_staff())
    
    iconCoagulation <- ''
    textCoagulation <- ''
    iconPediatrician <- ''
    textPediatrician <- ''
    iconSyndrome <- ''
    textSyndrome <- ''
    
    if(patient_data_staff()$has_coagulation[[patientIdx]]) {
      iconCoagulation <- '<img src="blood.jpeg" alt="drop" width="27" height="31.5"/>'
      textCoagulation <- buildTreatmentBanner(patient_data_staff()$treat_coagulant_1[[patientIdx]],
                                              patient_data_staff()$date_derniere_prise_1[[patientIdx]],
                                              patient_data_staff()$treat_coagulant_2[[patientIdx]],
                                              patient_data_staff()$date_derniere_prise_2[[patientIdx]],
                                              patient_data_staff()$treat_coagulant_3[[patientIdx]],
                                              patient_data_staff()$date_derniere_prise_3[[patientIdx]])
      
    } 
    
    if(!(patient_data_staff()$date_naissance[[patientIdx]] == '')) {
      if(deliverAge(patient_data_staff()$date_naissance[[patientIdx]], 
                    patient_data_staff()$created_at[[patientIdx]]) < 16) {
        iconPediatrician <- '<img src="child_icon.jpeg" alt="child" height="30"/>'
        textPediatrician <- "<h4><b>Pédiatrie</b></h4>"
      }
    }
    
    if(!(patient_data_staff()$syndrome[[patientIdx]] == '')) {
      iconSyndrome <- '<img src="bugs_icon.png" alt="infection" height="30"/>'
      textSyndrome <- "<h4><b>Infection/Syndrome inflammatoire</b></h4>"
    } 
    
    x <- paste0('<table style="width:100%">
      <tr>
      <th></th>
      <th style="width:95%;"></th>
      </tr>
      <tr>
      <td style="text-align:center;">', iconCoagulation, '</td>
      <td style="text-align:left;">', textCoagulation, '</td>
      </tr>
      <tr>
      <td style="text-align:center;">', iconPediatrician, '</td>
      <td style="text-align:left;">', textPediatrician, '</td>
      </tr>
      <td style="text-align:center;">', iconSyndrome, '</td>
      <td style="text-align:left;">', textSyndrome, '</td>
      </tr>
      </table> ')
    
    HTML(x)
    
  })
  
  output$staff_metadata <-renderUI({
    req(patient_data_staff())
    
    tryCatch({
      out <- conn %>%
        tbl('garde') %>%
        collect() %>%
        filter(garde_id == patient_data_staff()$garde_id[[patientIdx]])
      
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
    
    
    garde <- deliverGardeDisplayName(out$modified_by, out$partner, user_base)
    author <- convertUsernameToDisplayname(patient_data_staff()$created_by[[patientIdx]], user_base)
    timestamp <- displaySimpleDateTime(patient_data_staff()$created_at[[patientIdx]])
    
    x <- buildMetadataBanner(author,
                             patient_data_staff()$hopital[[patientIdx]],
                             timestamp,
                             garde)
    
    HTML(x)
    
  })
  
  output$staff_status <-renderUI({
    req(patient_data_staff())
    
    statusDisplayName <- patient_data_staff()$status[[patientIdx]] %>%
      displayStatusName()
    
    x <- paste0('<b><h4 style="color:#800000;text-align:right">Status : ', statusDisplayName,'</h4></b>')
    
    HTML(x)
  })
  
  output$staff_pre_def_decisions <-renderUI({
    
    x <- buildDecisionBanner(patient_data_staff()$pre_decision[[patientIdx]], 
                             patient_data_staff()$def_decision[[patientIdx]],
                             pullRight = FALSE)
    
    HTML(x)
    
  })
  
  output$staff_pathologies <-renderUI({
    req(patient_data_staff()$pathologie_1)
    
    x <- buildUnorderedList(list(patient_data_staff()$pathologie_1[[patientIdx]],
                                 patient_data_staff()$pathologie_2[[patientIdx]],
                                 patient_data_staff()$pathologie_3[[patientIdx]]),
                            "Pathologie(s)")
    
    
    HTML(x)
    
  })
  
  output$staff_syndrome <-renderUI({
    req(patient_data_staff()$syndrome)
    
    x <- buildUnorderedList(list(patient_data_staff()$syndrome[[patientIdx]]),
                            "Syndrome inflammatoire ou infectieux actif")
    
    HTML(x)
    
  })
  
  output$staff_comorbidites <-renderUI({
    
    patient_data_staff()[patientIdx,] %>%
      deliverComorbiditeTibble() %>%
      buildComorbiditeTable() %>%
      HTML()
    
  })
  
  output$staff_description_histoire <-renderUI({
    req(patient_data_staff()$description_histoire)
    
    x <- buildParagraph(patient_data_staff()$description_histoire[[patientIdx]], "Histoire")
    
    HTML(x)
    
  })
  
  staff_patient_filenames_count <- reactive({
    req(patient_data_staff()$uid)
    
    session$userData$emptyCache()
    
    pathToPatientImages <- paste0(tiffDir, patient_data_staff()$uid[[patientIdx]])
    
    if(!file.exists(pathToPatientImages)) {
      
      thisMode = 'test'
      
      filenames <- fetchFiles(patient_data_staff()$uid[[patientIdx]], 
                              dbInfo[[1]][[2]], 
                              '22', 
                              deviceInfo[[1]][[1]], 
                              deviceInfo[[1]][[2]],
                              thisMode,
                              aLocalDB = localDB)
      
      filename_count <- filenames %>%
        length()
      
      
      fetchPhotos(patient_data_staff()$uid[[patientIdx]],
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
  
  output$staff_photos_title <-renderUI({
    req(staff_patient_filenames_count())
    
    if(staff_patient_filenames_count() == 0) {
      x <- paste0("<h4> Aucune image n'a été trouvée </h4>")
    } else if (staff_patient_filenames_count() == 1) {
      x <- paste0("<h4> 1 image trouvée </h4>")
    } else {
      x <- paste0('<h4> ', staff_patient_filenames_count(),
                  ' images trouvées </h4>')
    }
    
    HTML(x)
    
  })
  
  observeEvent(staff_patient_filenames_count(), {
    if(staff_patient_filenames_count() < 1){
      shinyjs::hide("staff_arrows_container")
    } else {
      shinyjs::show("staff_arrows_container")
    }
  })
  
  imgIdxStaff <- 1
  makeReactiveBinding('imgIdxStaff')
  
  observeEvent(input$increase_index_staff, {
    
    if(imgIdxStaff == staff_patient_filenames_count()) {
      imgIdxStaff <<- 1
    } else {
      imgIdxStaff <<- imgIdxStaff + 1
    }
  })
  
  observeEvent(input$decrease_index_staff, {
    
    if(imgIdxStaff == 1) {
      imgIdxStaff <<- staff_patient_filenames_count()
    } else {
      imgIdxStaff <<- imgIdxStaff - 1
    }
  })
  
  staffPatientPhotos <- reactive({
    req(staff_patient_filenames_count())
    
    targetDir <- paste0(tiffDir, patient_data_staff()$uid[[patientIdx]])
    
    imageFile <- list.files(path = targetDir, pattern = '.tiff')
    
    image_read(paste0(targetDir, '/', imageFile))
    
  })
  
  
  output$staff_tiffImage <- renderImage(
    {
      req(staff_patient_filenames_count())
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.png')

      
      # Generate the PNG
      if(staff_patient_filenames_count() == 0) {
        index <- 1
      } else {
        index <- imgIdxStaff
      }
      
      staffPatientPhotos()[index] %>%
        image_scale(geometry = "x380") %>%
        image_write(path = outfile, format = "png")
      #dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
      
    }, 
    deleteFile = TRUE)
  
  observeEvent(input$expand_image_staff, {
    
    clearTmpImgs()
    
    timeSuffix <- Sys.time() %>% 
      gsub("^[^\\s]+\\s", "", .) %>%
      str_replace_all(":", "")
    
    staffPatientPhotos()[imgIdxStaff] %>%
      image_scale(geometry = "x780") %>%
      image_write(path = paste0(tmpImg, timeSuffix), format = "png")
    
    
    showModal(
      modalDialog(
        HTML(paste0('<img src="tmpimg', timeSuffix, '">')),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  session$userData$staff_meting_overview <- reactiveVal(0)
  
  observeEvent(input$staff_meeting, {
    shinyjs::show("staff_ui_controllers")
    shinyjs::show("staff_patient_overview")
    shinyjs::show("staff_ui")
    shinyjs::show("staff_ui_controllers_2")
    shinyjs::show("staff_ui_chat")
    shinyjs::show("annuler_staff_meeting")
    shinyjs::show("cloturer_staff_meeting")
    shinyjs::disable("staff_meeting")
  })
  
  observeEvent(input$annuler_staff_meeting, {
    shinyjs::hide("staff_ui_controllers")
    shinyjs::hide("staff_patient_overview")
    shinyjs::hide("staff_ui")
    shinyjs::hide("staff_ui_controllers_2")
    shinyjs::hide("staff_ui_chat")
    shinyjs::hide("annuler_staff_meeting")
    shinyjs::hide("cloturer_staff_meeting")
    shinyjs::enable("staff_meeting")
  })
  
  
  observeEvent(input$cloturer_staff_meeting, {
    
    summary <- overviewTable() %>%
      summarise(readiness = sum(valid))

    if (summary$readiness == patient_data_staff_count()) {
      
      tryCatch({
        
        showModal(patientezDialog)
        
        uids <- patient_data_staff()$uid
        
        for(i in 1:patient_data_staff_count()) {
          
          staffDecision <- input[[staff_decision_names()[[i]]]]
          staffExplanation <- input[[staff_explanation_names()[[i]]]]
          
          thisQuery <- writeStaffDecisionQuery(staffDecision, 
                                               prepareString(staffExplanation),
                                               uids[[i]])
          
          dbExecute(conn, thisQuery)
          
          if(staffDecision == 'Complément d`examen à faire') {
            
            nomCompletPatient <- writePatientDisplayName(patient_data_staff()$prenom[[i]], patient_data_staff()$nom[[i]])
            
            generateInfoSupplementaireEmail(nomCompletPatient,
                                            patient_data_staff()$date_naissance[[i]],
                                            staffDecision,
                                            staffExplanation) %>%
              smtp_send(
                to = patient_data_staff()$contact_email[[i]],
                from = zaldibase,
                subject = printStandardEmailTitle,
                credentials = creds_file(credentialsPath)
              )
          }
        }
        
        session$userData$staff_trigger(session$userData$staff_trigger() + 1)
        session$userData$dossiers_trigger(session$userData$dossiers_trigger() + 1)
        
        shinyjs::hide("staff_ui")
        shinyjs::hide("staff_ui_controllers")
        shinyjs::hide("staff_ui_controllers_2")
        shinyjs::hide("staff_patient_overview")
        shinyjs::hide("staff_ui_chat")
        shinyjs::hide("annuler_staff_meeting")
        shinyjs::hide("cloturer_staff_meeting")
        shinyjs::enable("staff_meeting")
        
        patientIdx <<- 1
        
        removeModal()
        
        showToast("success", message = "Staff meeting terminée correctement")}, 
        
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
    } else {
      
      showModal(
        modalDialog(
        div(style = "padding: 30px;", class = "text-center",
            HTML(printWarningIncompleteStaffMeeting)),
        easyClose = TRUE,
        footer = modalButton("Fermer"))
      )
    }
  })
  
  # Rendez-vous table ------------------------------------------------------------
  
  # trigger to reload data from the "rendez-vous" table
  session$userData$rendezvous_trigger <- reactiveVal(0)
  
  
  # Read in Rendez-vous table from the database
  rendezvous <- reactive({
    req(credentials()$user_auth)
    
    session$userData$rendezvous_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC"),
               date_rendezvous = as.Date(date_rendezvous)) %>%
        arrange(desc(modified_at)) %>%
        filter(isRelevantForSchedule(needs_rendezvous, has_rendezvous, date_rendezvous))
      
    }, 
    error = function(err) {
      msg <- "Could not find the rendez-vous you are looking for!"
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
  
  rendezvous_table_prep <- reactiveVal(NULL)
  rendezvous_table_proxy <- DT::dataTableProxy('rendezvous_table')
  rendezvous_ok_table_prep <- reactiveVal(NULL)
  rendezvous_ok_table_proxy <- DT::dataTableProxy('rendezvous_ok_table')
  
  observeEvent(rendezvous(), {
    
    needsRendezVousTibb <- rendezvous() %>%
      filter(needs_rendezvous == 1)
    
    ids <- needsRendezVousTibb$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Planifier" id = ', id_, ' style="margin: 0; background:teal"><i class="fa fa-calendar"></i></button>
                            </div>')
    })
    
    needsRendezVousTibb <- needsRendezVousTibb %>%
      transmute(nom, prenom, date_naissance, displayStatusName(status))
      
    needsRendezVousTibb <- cbind(tibble(" " = actions), needsRendezVousTibb)
    
    if (is.null(rendezvous_table_prep())) {
      
      rendezvous_table_prep(needsRendezVousTibb)
    } else {
      replaceData(rendezvous_table_proxy,
                  needsRendezVousTibb,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
    
    okRendezVousTibb <- rendezvous() %>%
      filter(has_rendezvous == 1)
    
    ids <- okRendezVousTibb$uid
    
    actions2 <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Planifier" id = ', id_, ' style="margin: 0; background:teal"><i class="fa fa-calendar"></i></button>
                            </div>')
    })
    
    okRendezVousTibb <- okRendezVousTibb %>%
      transmute(nom, prenom, date_naissance, displayStatusName(status))
    
    okRendezVousTibb <- cbind(tibble(" " = actions2), okRendezVousTibb)
    
    rendezvous_ok_table_prep(okRendezVousTibb)
    
  })
  
  output$rendezvous_table <- renderDT({
    req(credentials()$user_auth, rendezvous_table_prep())
    
    out <- rendezvous_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Status'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'tp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Aucun rendez-vous à prendre",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant')),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
  })
  
  output$rendezvous_ok_table <- renderDT({
    req(credentials()$user_auth, rendezvous_ok_table_prep())
    
    out <- rendezvous_ok_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Status'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'tp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Pas de rendez-vous prévus",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant')),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
    
  })
  
  rendezvous_patient <- eventReactive(input$rendezvous_patient_id, {
    
    rendezvous() %>%
      filter(uid == input$rendezvous_patient_id)
    
  })
  
  rendezvousEditModuleServer("add_rendezvous",
                           modal_title = "Planification de rendez-vous",
                           rendezvous_patient = rendezvous_patient,
                           modal_trigger = reactive({input$rendezvous_patient_id}))
  
  # Rapatriement table ------------------------------------------------------------
  
  # trigger to reload data from the "rendez-vous" table
  session$userData$rapatriement_trigger <- reactiveVal(0)
  
  
  # Read in Rendez-vous table from the database
  rapatriement <- reactive({
    req(credentials()$user_auth)
    
    session$userData$rapatriement_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC"),
               date_rapatriement = as.Date(date_rapatriement)) %>%
        arrange(desc(modified_at)) %>%
        filter(isRelevantForSchedule(needs_room, has_room, date_rapatriement))
      
    }, 
    error = function(err) {
      msg <- "Could not find the rapatriement you are looking for!"
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
  
  rapatriement_table_prep <- reactiveVal(NULL)
  rapatriement_table_proxy <- DT::dataTableProxy('rapatriement_table')
  rapatriement_ok_table_prep <- reactiveVal(NULL)
  rapatriement_ok_table_proxy <- DT::dataTableProxy('rapatriement_ok_table')
  
  observeEvent(rapatriement(), {
    
    needsRapatriementTibb <- rapatriement() %>%
      filter(needs_room == 1)
    
    ids <- needsRapatriementTibb$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Planifier" id = ', id_, ' style="margin: 0; background:#8B008B"><i class="fa fa-hospital"></i></button>
                            </div>')
    })
    
    needsRapatriementTibb <- needsRapatriementTibb %>%
      transmute(nom, prenom, date_naissance, displayStatusName(status))
    
    needsRapatriementTibb <- cbind(tibble(" " = actions), needsRapatriementTibb)
    
    if (is.null(rapatriement_table_prep())) {
      
      rapatriement_table_prep(needsRapatriementTibb)
    } else {
      replaceData(rapatriement_table_proxy,
                  needsRapatriementTibb,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
    
    okRapatriementTibb <- rapatriement() %>%
      filter(has_room == 1)
    
    ids <- okRapatriementTibb$uid
    
    actions2 <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Planifier" id = ', id_, ' style="margin: 0; background:#8B008B"><i class="fa fa-hospital"></i></button>
                            </div>')
    })
    
    okRapatriementTibb <- okRapatriementTibb %>%
      transmute(nom, prenom, date_naissance, displayStatusName(status))
    
    okRapatriementTibb <- cbind(tibble(" " = actions2), okRapatriementTibb)
    
    rapatriement_ok_table_prep(okRapatriementTibb)
    
  })
  
  output$rapatriement_table <- renderDT({
    req(credentials()$user_auth, rapatriement_table_prep())
    
    out <- rapatriement_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Status'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'tp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Pas de patients à hospiatliser",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant')),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
  })
  
  output$rapatriement_ok_table <- renderDT({
    req(credentials()$user_auth, rapatriement_ok_table_prep())
    
    out <- rapatriement_ok_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Status'),
                selection = "single",
                class = "compact stripe row-border nowrap",
                escape = -1,  # Escape the HTML in all except 1st column (which has the buttons)
                options = list(scrollX = TRUE,
                               dom = 'tp',
                               columnDefs = list(list(targets = 0, orderable = FALSE)),
                               pageLength = 10,
                               language = list(emptyTable = "Pas d'hospitalisations prévues",
                                               paginate = list(`next` = 'Suivant',
                                                               previous = 'Précédant')),
                               drawCallback = JS("function(settings) {
                                              // removes any lingering tooltips
                                              $('.tooltip').remove()}"))
      ) 
    
  })
  
  rapatriement_patient <- eventReactive(input$rapatriement_patient_id, {
    
    rapatriement() %>%
      filter(uid == input$rapatriement_patient_id)
    
  })
  
  rapatriementEditModuleServer("add_rapatriement",
                             modal_title = "Planification des hospitalisations",
                             rapatriement_patient = rapatriement_patient,
                             modal_trigger = reactive({input$rapatriement_patient_id}))
  
  # Externes table ------------------------------------------------------------
  
  
  # trigger to reload data from the "patients" table
  session$userData$externes_trigger <- reactiveVal(0)
  
  
  # Read in Mes Dossiers table from the database
  externes <- reactive({
    req(credentials()$user_auth)
    
    session$userData$externes_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(created_by == session$userData$username())
      
    }, 
    error = function(err) {
      msg <- "Could not find the externe dossier you are looking for!"
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
  
  externes_table_prep <- reactiveVal(NULL)
  
  observeEvent(externes(), {
    
    out <- externes()
    
    ids <- out$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0('<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
                     <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Modifier" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
                            </div>')
    })
    
    # Select relevant columns for the user
    out <- out %>%
      transmute(nom, prenom, date_naissance, pathologie_1, displayStatusName(status))
    
    # Set the Action Buttons row to the first column of the `dossiers` table
    out <- cbind(tibble(" " = actions),
                 out)
    
    if (is.null(externes_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      externes_table_prep(out)
      
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without reendering the entire table
      replaceData(externes_table_proxy,
                  out,
                  resetPaging = FALSE,
                  rownames = FALSE)
    }
  })
  
  
  output$externes_table <- renderDT({
    req(credentials()$user_auth, externes_table_prep())
    
    out <- externes_table_prep() 
    
    out %>%
      datatable(rownames = FALSE,
                colnames = c('Nom', 'Prénom', 'Date de naissance', 
                             'Pathologie', 'Status'),
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
  
  # Edit/Delete modules---------------------------------------------------------
  
  externes_table_proxy <- DT::dataTableProxy('externes_table')
  
  externesEditModuleServer("add_externe",
                           modal_title = "Enregistrer un nouveau dossier",
                           externe_patient = function() NULL,
                           modal_trigger = reactive({input$add_externe}))
  
  
  externe_to_edit <- eventReactive(input$externe_id_to_edit, {
    
    externes() %>%
      filter(uid == input$externe_id_to_edit)
    
  })
  
  externesEditModuleServer("edit_externe",
                           modal_title = "Modification du profil",
                           externe_patient = externe_to_edit,
                           modal_trigger = reactive({input$externe_id_to_edit}))
  
  
  # Patient data from archive--------------------------------------------------
  
  extPatientUID <- reactive({
    
    externes()[input$externes_table_rows_selected,][[1]]
    
  })
  
  externes_patient_data <- eventReactive(input$externes_table_rows_selected, {
    
    patientRow <- NULL
    tryCatch({
      patientRow <- conn %>%
        tbl('patients') %>%
        collect() %>%
        mutate(created_at = as.POSIXct(created_at, tz = "UTC"),
               modified_at = as.POSIXct(modified_at, tz = "UTC")) %>%
        arrange(desc(modified_at)) %>%
        filter(uid == extPatientUID())
      
    }, 
    error = function(err) {
      msg <- "Could not find that particular patient in externes!"
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
  
  # Patient data for externes---------------------------------------------------
  
  output$patient_display_name_ext <- renderText({
    
    if(externes_patient_data()$prenom == '' | externes_patient_data()$nom == '') {
      return("Patient inconnu")
    } else {
      return(writePatientDisplayName(externes_patient_data()$prenom, externes_patient_data()$nom))
    }
  })
  
  output$patient_age_ext <-renderText({
    
    paste0("âgé(e) de ", deliverAge(externes_patient_data()$date_naissance,
                                    externes_patient_data()$created_at), " ans")
    
  })
  
  externes_patient_filenames_count <- reactive({
    req(extPatientUID())
    
    session$userData$emptyCache()
    
    pathToPatientImages <- paste0(tiffDir, extPatientUID())
    
    if(!file.exists(pathToPatientImages)) {
      
      thisMode = 'test'
      
      filenames <- fetchFiles(extPatientUID(), 
                              dbInfo[[1]][[2]], 
                              '22', 
                              deviceInfo[[1]][[1]], 
                              deviceInfo[[1]][[2]],
                              thisMode,
                              aLocalDB = localDB)
      
      filename_count <- filenames %>%
        length()
      
      
      fetchPhotos(extPatientUID(),
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
  
  output$externes_photos_title <-renderUI({
    req(externes_patient_filenames_count())
    
    if(externes_patient_filenames_count() == 0) {
      x <- paste0("<h4> Aucune image n'a été transférée </h4>")
    } else if (externes_patient_filenames_count() == 1) {
      x <- paste0("<h4> 1 image transférée </h4>")
    } else {
      x <- paste0('<h4> ', externes_patient_filenames_count(),
                  ' images transférées </h4>')
    }
    
    HTML(x)
    
  })
  
  
  imgIdxExt <- 1
  makeReactiveBinding('imgIdxExt')
  
  observeEvent(input$increase_index_externes, {
    
    if(imgIdxExt == externes_patient_filenames_count()) {
      imgIdxExt <<- 1
    } else {
      imgIdxExt <<- imgIdxExt + 1
    }
  })
  
  observeEvent(input$decrease_index_externes, {
    
    if(imgIdxExt == 1) {
      imgIdxExt <<- externes_patient_filenames_count()
    } else {
      imgIdxExt <<- imgIdxExt - 1
    }
  })
  
  externesPatientPhotos <- reactive({
    req(externes_patient_filenames_count())
    
    targetDir <- paste0(tiffDir, extPatientUID())
    
    imageFile <- list.files(path = targetDir, pattern = '.tiff')
    
    image_read(paste0(targetDir, '/', imageFile))
    
  })
  
  
  output$externes_tiffImage <- renderImage(
    {
      req(externes_patient_filenames_count())
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.png')
      
      #width  <- session$clientData$output_tiffImage_width
      #height <- session$clientData$output_tiffImage_height
      
      # Generate the PNG
      #png(outfile, width = 400, height = 300)
      externesPatientPhotos()[imgIdxExt] %>%
        image_scale(geometry = "x380") %>%
        image_write(path = outfile, format = "png")
      #dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
      
    }, 
    deleteFile = TRUE)
  
  
  # output$info_icons <- renderUI({
  #   req(patient_data())
  #   
  #   iconCoagulation <- ''
  #   textCoagulation <- ''
  #   iconPediatrician <- ''
  #   textPediatrician <- ''
  #   
  #   if(patient_data()$has_coagulation) {
  #     iconCoagulation <- '<img src="blood.jpeg" alt="drop" width="27" height="31.5"/>'
  #     textCoagulation <- buildTreatmentBanner(patient_data()$treat_coagulant_1,
  #                                             patient_data()$date_derniere_prise_1,
  #                                             patient_data()$treat_coagulant_2,
  #                                             patient_data()$date_derniere_prise_2,
  #                                             patient_data()$treat_coagulant_3,
  #                                             patient_data()$date_derniere_prise_3)
  #     
  #   } 
  #   
  #   if(deliverAge(patient_data()$date_naissance, patient_data()$created_at) < 18) {
  #     iconPediatrician <- '<img src="child_icon.jpeg" alt="child" height="30"/>'
  #     textPediatrician <- "<h4><b>Pédiatrie</b></h4>"
  #     
  #   }
  #   
  #   x <- paste0('<table style="width:100%">
  #     <tr>
  #     <th></th>
  #     <th style="width:95%;"></th>
  #     </tr>
  #     <tr>
  #     <td style="text-align:center;">', iconCoagulation, '</td>
  #     <td style="text-align:left;">', textCoagulation, '</td>
  #     </tr>
  #     <tr>
  #     <td style="text-align:center;">', iconPediatrician, '</td>
  #     <td style="text-align:left;">', textPediatrician, '</td>
  #     </tr>
  #     </table> ')
  #   
  #   HTML(x)
  #   
  # })
  # 
  # observeEvent(input$show_contact_details, {
  #   showModal(modalDialog(
  #     title = paste0(patient_data()$prenom, ' ', str_to_upper(patient_data()$nom, locale = 'fr')),
  #     buildContactCard(patient_data()$phone_number_patient,
  #                      patient_data()$contact_person, 
  #                      patient_data()$contact_phone, 
  #                      patient_data()$contact_email,
  #                      patient_data()$hopital, 
  #                      patient_data()$created_at, 
  #                      patient_data()$created_by, 
  #                      user_base) %>% HTML(),
  #     easyClose = TRUE,
  #     footer = modalButton("Fermer")
  #   ))
  # })
  
  output$pathologies_ext <-renderUI({
    req(externes_patient_data()$pathologie_1)
    
    x <- buildUnorderedList(list(externes_patient_data()$pathologie_1,
                                 externes_patient_data()$pathologie_2,
                                 externes_patient_data()$pathologie_3),
                            "Pathologie(s)")
    
    
    HTML(x)
    
  })
  
  output$syndrome_ext <-renderUI({
    req(externes_patient_data()$syndrome)
    
    x <- buildUnorderedList(list(externes_patient_data()$syndrome),
                            "Syndrome inflammatoire ou infectieux actif")
    
    HTML(x)
    
  })
  
  output$comorbidites_ext <-renderUI({
    
    externes_patient_data() %>%
      deliverComorbiditeTibble() %>%
      buildComorbiditeTable() %>%
      HTML()
    
  })
  
  
  output$description_histoire_ext <-renderUI({
    req(externes_patient_data()$description_histoire)
    
    x <- buildParagraph(externes_patient_data()$description_histoire, "Histoire")
    
    HTML(x)
    
  })
  
  # Chat ----------------------------------------------------------------------
  messages_db <- reactiveValues(messages = fetchMessages(conn))
  
  dbTimer <- reactiveTimer(intervalMs = 5000)
  
  observe({
    dbTimer()
    
    messages_db$messages <- fetchMessages(conn)
  })
  
  observeEvent(input$chat_send, {
    
    # only do anything if there's a message
    if (!(input$chat_message == "" | is.null(input$chat_message))) {
      
      messageTimestamp <- Sys.time() %>%
        as.character()
      
      chatQuery <- writeChatQuery(messageTimestamp,
                                  patientUID(),
                                  session$userData$username(),
                                  prepareString(input$chat_message))
      
      dbExecute(conn, chatQuery)
      
      messages_db$messages <- fetchMessages(conn, patientUID())
      
      shiny::updateTextInput(inputId = "chat_message", value = "")
      
      deleteNotificationsQuery <- writeDeleteNotificationsQuery(patientUID())
      
      dbExecute(conn, deleteNotificationsQuery)
    }
  })
  
  output$chat_body <- renderUI({
    req(patientUID())
    
    messages <- messages_db$messages %>%
      filter(uid == patientUID(),
             !(str_trim(message) == ""))
    
    renderChatMessages(messages, session$userData$username())
  })
  
  observe({
    if(identical(patientUID(), character(0))) {
      shinyjs::hide("chat_area")
    } else {
      shinyjs::show("chat_area")
    }    
  })
  
  observeEvent(input$chat_send_externes, {
    
    # only do anything if there's a message
    if (!(input$chat_message_externes == "" | is.null(input$chat_message_externes))) {
      
      messageTimestamp <- Sys.time() %>%
        as.character()
      
      chatQuery <- writeChatQuery(messageTimestamp,
                                  extPatientUID(),
                                  session$userData$username(),
                                  prepareString(input$chat_message_externes))
      
      dbExecute(conn, chatQuery)
      
      messages_db$messages <- fetchMessages(conn, extPatientUID())
      
      shiny::updateTextInput(inputId = "chat_message_externes", value = "")
      
      notificationQuery <- writeNotificationQuery(extPatientUID(),
                                                  writePatientDisplayName(externes_patient_data()$prenom,
                                                                          externes_patient_data()$nom))

      dbExecute(conn, notificationQuery)
    }
  })
  
  output$chat_body_externes <- renderUI({
    req(extPatientUID())
    
    messages <- messages_db$messages %>%
      filter(uid == extPatientUID(),
             !(str_trim(message) == ""))
    
    renderChatMessages(messages, session$userData$username())
  })
  
  observe({
    if(identical(extPatientUID(), character(0))) {
      shinyjs::hide("chat_area_externes")
    } else {
      shinyjs::show("chat_area_externes")
    }    
  })
  
  observeEvent(input$chat_send_archive, {
    
    # only do anything if there's a message
    if (!(input$chat_message_archive == "" | is.null(input$chat_message_archive))) {
      
      messageTimestamp <- Sys.time() %>%
        as.character()
      
      chatQuery <- writeChatQuery(messageTimestamp,
                                  archivePatientUID(),
                                  session$userData$username(),
                                  prepareString(input$chat_message_archive))
      
      dbExecute(conn, chatQuery)
      
      messages_db$messages <- fetchMessages(conn, archivePatientUID())
      
      shiny::updateTextInput(inputId = "chat_message_archive", value = "")
      
      deleteNotificationsQuery <- writeDeleteNotificationsQuery(archivePatientUID())
      
      dbExecute(conn, deleteNotificationsQuery)
    }
  })
  
  output$chat_body_archive <- renderUI({
    req(archivePatientUID())
    
    messages <- messages_db$messages %>%
      filter(uid == archivePatientUID(),
             !(str_trim(message) == ""))
    
    renderChatMessages(messages, session$userData$username())
  })
  
  observe({
    if(identical(archivePatientUID(), character(0))) {
      shinyjs::hide("chat_area_archive")
    } else {
      shinyjs::show("chat_area_archive")
    }    
  })
  
  
  staff_chat_names <- reactive(paste0("chat_message_", seq_len(patient_data_staff_count())))
  
  output$staff_chat_message <- renderUI({
    req(patient_data_staff())
    
    chat_input_controllers <- map(staff_chat_names(), ~ textAreaInput(.x, 
                                                                      "",
                                                                      value = isolate(input[[.x]]),
                                                                      width = '100%',
                                                                      height = '140px'))
    chat_input_controllers[[patientIdx]]
  })
  
  observeEvent(input$staff_chat_send, {
    
    # only do anything if there's a message
    if (!(input[[staff_decision_names()[[patientIdx]]]] == "" | is.null(input[[staff_decision_names()[[patientIdx]]]]))) {
      
      messageTimestamp <- Sys.time() %>%
        as.character()
      
      message <- input[[staff_chat_names()[[patientIdx]]]] %>%
        prepareString()
      
      chatQuery <- writeChatQuery(messageTimestamp,
                                  patient_data_staff()$uid[[patientIdx]],
                                  session$userData$username(),
                                  message)
      
      print(chatQuery)
      dbExecute(conn, chatQuery)
      
      messages_db$messages <- fetchMessages(conn, patient_data_staff()$uid[[patientIdx]])
      
      shiny::updateTextInput(inputId = staff_chat_names()[[patientIdx]], value = "")
      
      deleteNotificationsQuery <- writeDeleteNotificationsQuery(patient_data_staff()$uid[[patientIdx]])
      
      dbExecute(conn, deleteNotificationsQuery)
    }
  })
  
  output$staff_chat_body <- renderUI({
    req(patient_data_staff()$uid)
    
    messages <- messages_db$messages %>%
      filter(uid == patient_data_staff()$uid[[patientIdx]],
             !(str_trim(message) == ""))
    
    renderChatMessages(messages, session$userData$username())
  })
  
  # Analyse ---------------------------------------------------------------------
  # Read in Mes Dossiers table from the database
  
  closedDossiers <- reactive({
    req(credentials()$user_auth)
    
    session$userData$dossiers_trigger()
    session$userData$staff_trigger()
    
    out <- NULL
    tryCatch({
      out <- conn %>%
        tbl('patients') %>%
        collect() %>%
        filter(is_closed == 1)
      
    }, 
    error = function(err) {
      msg <- "Could not find the closed dossiers you are looking for!"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    print(str(out))
    print("Print hospital...")
    print(out$hopital[[1]])
    out 
    
  })
  
  output$chart_cases_per_origin <- renderPlot({
    
    closedDossiers() %>%
      filter(between(as.Date(ymd_hms(created_at)), 
                     ymd(input$interval_of_interest[1]), 
                     ymd(input$interval_of_interest[2]))) %>%
      mutate(hopital = factor(hopital, levels = hopitaux)) %>%
      count(hopital) %>%
      mutate(hopital = reorder(hopital, n)) %>%
      ggplot(aes(x = hopital, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste0("Nombre de patients rapportés par hôpitaux régionaux \n entre le ",
                     sd(ymd(input$interval_of_interest[1])),
                     " et le ",
                     sd(ymd(input$interval_of_interest[2])))) +
      xlab("") + ylab("# cas rapportés au CHUGA") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
      theme(axis.title.x = element_text(size = 16, face = "bold")) +
      theme(axis.title.y = element_text(size = 16, face = "bold")) +
      theme(axis.text.x= element_text(face = "bold", size = 12)) +
      theme(axis.text.y= element_text(face = "bold", size = 12, angle = 30)) +
      coord_flip()
    
    
  })
  
  summaryPathologies <- reactive({
    
    summary <- closedDossiers() %>%
      filter(between(as.Date(ymd_hms(created_at)), 
                     ymd(input$interval_of_interest[1]), 
                     ymd(input$interval_of_interest[2]))) %>%
      filter(hopital == input$centre_hopitalier) %>%
      select(pathologie_1, pathologie_2, pathologie_3) %>%
      pivot_longer(cols = starts_with('pathologie_'), names_to = 'label') %>%
      filter(!value == "") %>%
      mutate(pathologie = as.factor(value)) %>%
      group_by(pathologie) %>%
      summarise(cas = n()) %>%
      ungroup()
    
    if(nrow(summary) == 0) {
      return(NULL)
    } else {
      return(summary)
    }
  })
  
  output$pie_pathologies <- renderPlot({
    req(summaryPathologies())
    
    totalCases <- sum(summaryPathologies()$cas)
    
    summaryPathologies() %>%
      mutate(perc = cas/totalCases) %>%
      ggplot(aes(x = "", y = perc, fill = pathologie)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "legend",
           x = NULL,
           y = NULL,
           title = paste0("Disque par pathologies entre le ", 
                          sd(ymd(input$interval_of_interest[1])),
                          " et le ",
                          sd(ymd(input$interval_of_interest[2]))),
           caption = paste0("Centre hôpitalier : ", input$centre_hopitalier)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.caption = element_text(size = 12, hjust = 0, color = "#696969"),
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 14)) + 
      scale_fill_brewer(palette = "Set1")
    
  })
  
  summaryAges <- reactive({
    
    summary <- closedDossiers() %>%
      filter(between(as.Date(ymd_hms(created_at)), 
                     ymd(input$interval_of_interest[1]), 
                     ymd(input$interval_of_interest[2]))) %>%
      filter(hopital == input$centre_hopitalier) %>%
      select(date_naissance, created_at) %>%
      mutate(age = deliverAge(date_naissance, created_at),
             classAge = classifyAge(age)) %>%
      pivot_longer(cols = starts_with('class'), names_to = 'label') %>%
      filter(!value == "") %>%
      mutate(class = as.factor(value)) %>%
      group_by(class) %>%
      summarise(cas = n()) %>%
      ungroup()
    
    if(nrow(summary) == 0) {
      return(NULL)
    } else {
      return(summary)
    }
  })
  
  output$pie_ages <- renderPlot({
    req(summaryAges())
    
    totalCases <- sum(summaryAges()$cas)
    
    summaryAges() %>%
      mutate(perc = cas/totalCases) %>%
      ggplot(aes(x = "", y = perc, fill = class)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "legend",
           x = NULL,
           y = NULL,
           title = paste0("Disque par âges entre le ",
                          sd(ymd(input$interval_of_interest[1])),
                          " et le ",
                          sd(ymd(input$interval_of_interest[2]))),
           caption = paste0("Centre hôpitalier : ", input$centre_hopitalier)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.caption = element_text(size = 12, hjust = 0, color = "#696969"),
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 14)) +
      scale_fill_brewer(palette = "Set2")
    
  })
  
  # output$table_ages <- renderTable({
  #   req(summaryAges())
  #   
  #   summaryAges()
  # })
  
  # sequencedTibb <- seq(ymd('2012-04-07'), ymd('2013-03-22'), by = 'months') %>%
  #   as_tibble() %>%
  #   mutate(creationMonth = month(value),
  #          month = deliverMonthName(as.integer(creationMonth)),
  #          year = year(value),
  #          monthLabel = as.factor(paste0(month, " ", year)),
  #          counter = 0) %>%
  #   select(value, monthLabel, counter)
  # 
  # testTibb <- tribble(~monthLabel, ~counter,
  #                     'Avril 2012', 4,
  #                     'Janvier 2013', 7) %>%
  #   mutate(monthLabel = as.factor(monthLabel))
  # 
  # 
  # 
  # right_join(testTibb, sequencedTibb, c('monthLabel')) %>%
  #   arrange(value) %>%
  #   mutate(cas = if_else(is.na(counter.x), counter.y, counter.x))
    
    
  summaryInfections <- reactive({
    
    compactSummary <- closedDossiers() %>%
      filter(between(as.Date(ymd_hms(created_at)), 
                     ymd(input$interval_of_interest[1]), 
                     ymd(input$interval_of_interest[2]))) %>%
      filter(pathologie_1 == 'Complications / Infections post-op' | pathologie_2 == 'Complications / Infections post-op' | pathologie_3 == 'Complications / Infections post-op') %>%
      select(created_at) %>%
      mutate(creationDate = ymd_hms(created_at),
             creationMonth = month(creationDate),
             month = deliverShortMonthName(as.integer(creationMonth)),
             year = year(creationDate),
             monthLabel = paste0(month, " ", year)) %>%
      select(creationDate, monthLabel) %>%
      mutate(monthLabel = as.factor(monthLabel)) %>%
      group_by(monthLabel) %>%
      summarise(cas = n()) %>%
      ungroup()
    
    sequencedTibb <- seq(ymd(input$interval_of_interest[1]),ymd(input$interval_of_interest[2]), by = 'months') %>%
      as_tibble() %>%
      mutate(creationMonth = month(value),
             month = deliverShortMonthName(as.integer(creationMonth)),
             year = year(value),
             monthLabel = as.factor(paste0(month, " ", year)),
             cas = 0) %>%
      select(value, monthLabel, cas)
    
    summary <- right_join(compactSummary, sequencedTibb, c('monthLabel')) %>%
      arrange(value) %>%
      mutate(cas_1 = as.numeric(cas.x),
             cas_2 = as.numeric(cas.y),
             casTotaux = if_else(is.na(cas_1), cas_2, cas_1))
    
    summary$monthLabel <- factor(summary$monthLabel, levels = summary$monthLabel)
    
    if(nrow(summary) == 0) {
      return(NULL)
    } else {
      return(summary)
    }
  })
  
  #as.numeric(NA)
  
  output$chart_infect <- renderPlot({
    req(summaryInfections())

    summaryInfections() %>%
      ggplot(aes(x = monthLabel, y = casTotaux)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste0("Nombre de cas de complications ou infections entre le ",
                     sd(ymd(input$interval_of_interest[1])),
                     " et le ",
                     sd(ymd(input$interval_of_interest[2])))) +
      xlab("") + ylab("Nombre de cas") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
      theme(axis.title.x = element_text(size = 16, face = "bold")) +
      theme(axis.title.y = element_text(size = 16, face = "bold")) +
      theme(axis.text.x= element_text(face = "bold", size = 12)) +
      theme(axis.text.y= element_text(face = "bold", size = 12)) 
  })
 
  # set suspendWhenHidden to FALSE so it renders even without output
  outputOptions(output, 'role', suspendWhenHidden = FALSE) 

}
