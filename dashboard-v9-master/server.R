# Nombre de nouveaux infirmiers sur une période pour avoir une idée du turnover du staff
# Analyser les durées négatives pour faire ressortir les problèmes d'heure sur les tablettes
# nombre de formulaire par csps
# Temps moyen de saisie du formulaire de consulation ou traitement en fonction du nombre de classifications trouvées
# Temps moyen de saise du formulaire (consult ou traitement) en fonction du nombre de classifications graves
# nombre de consultations par agent par CSPS

function(input, output, session) {
  
  #########################
  ## Filter the datasets ##
  #########################
  
  enroll_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d") + days(1)
    d <- filter(enroll_data, started_on >= s, started_on < e)
    if(input$district != "Tous") {
      d <- filter(d, district == input$district)
    }
    d$mobile_user <- factor(d$mobile_user)
    d$site_code <- factor(d$site_code)
    d$district <- factor(d$district)
    d
  })
  
  visit_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d") + days(1)
    d <- filter(visit_data, started_on >= s, started_on < e)
    if(input$district != "Tous") { 
      d <- filter(d, district == input$district )
    } else {
      d <- filter(visit_data, started_on >= s, started_on < e)
    }
    d$health_worker_id <- factor(d$health_worker_id)
    d$case_id <- factor(d$case_id)
    d$mobile_user <- factor(d$mobile_user)
    d$site_code <- factor(d$site_code)
    d$district <- factor(d$district)
    d$qualification <- factor(d$qualification)
    d 
  })
  
  treatment_data_f <- reactive({
    s <- as.POSIXct(input$period[1], format = "%Y-%m-%d")
    e <- as.POSIXct(input$period[2], format = "%Y-%m-%d") + days(1)
    d <- filter(treatment_data, started_on >= s, started_on < e)
    if(input$district != "Tous") {
      d <- filter(d, district == input$district)
    } else {
      d <- filter(visit_data, started_on >= s, started_on < e)
    }
    d$mobile_user <- factor(d$mobile_user)
    d$site_code <- factor(d$site_code)
    d$district <- factor(d$district)
    d
  })
  
  #########
  ## Map ##
  #########
  
  output$map <- renderLeaflet({
      leaflet( data = geo_data()) %>% addTiles() %>% setView(1-1.53, 12.36, zoom = 7, options = list(animate = T)) %>% addMarkers(~longitude, ~latitude, popup = ~popup, layerId = ~mobile_user, group = ~district)
  })
  
  observe({
    data <- geo_data()
    m <- leafletProxy("map", data = data) %>% clearMarkers() %>% clearShapes()
    if (input$geo_data == "position") {
      m %>% addMarkers(~longitude, ~latitude, popup = ~popup, layerId = ~mobile_user, group = ~district)
    } else if (input$geo_data == "n_consults") {
      radius <- data$n_consults
      m %>% addCircles(~longitude, ~latitude, radius = radius * 5, layerId = ~mobile_user, group = ~district, stroke = F, fill = T, fillOpacity = 0.4)
    } else {
      radius <- data$sync_lag
      m %>% addCircles(~longitude, ~latitude, radius = radius * 150, layerId = ~mobile_user, group = ~district, stroke = F, fill = T, fillOpacity = 0.4)
    }
  })

  geo_data <- reactive({
    
    data <- visit_data_f()
    
    # Sync lag
    d <- data.frame(sync_lag(data, "mobile_user"))
    d <- cbind(rownames(d), d)
    colnames(d) <- c("mobile_user", "sync_lag")
    d$sync_lag <- as.numeric(d$sync_lag)
    d <- arrange(d, mobile_user)
    
    # Consults
    consults <- data.frame(table(data$mobile_user))
    colnames(consults) <- c("mobile_user", "n_consults")
    consults <- arrange(consults, mobile_user)
    d <- mutate(d, n_consults = consults$n_consults)
    
    # Health Workers data (total & consulting on the period)
    hw_c <- data.frame(table(consulting_health_workers(visit_data_f())$mobile_user))
    colnames(hw_c) <- c("mobile_user", "hw_consulting")
    hw_c <- arrange(hw_c, mobile_user)
    hw_t <- data.frame(table(tablet_users_data$mobile_user))
    colnames(hw_t) <- c("mobile_user", "hw_total")
    hw_t <- arrange(hw_t, mobile_user)
    
    # Coordinates
    d <- merge(d, mobile_users_data, by = "mobile_user")
    d <- merge(d, hw_c, by = "mobile_user")
    d <- merge(d, hw_t, by = "mobile_user")
    d <- filter(d, !is.na(latitude))
    d$popup <- map_popup(d)
    d
   })
  
   map_popup <- function(data) {
     s <- paste("<h4>")
     s <- paste(s, "CSPS de ", data$name, "<br/>")
     s <- paste(s, "<small>District de ", CA(data$district), "</small>")
     s <- paste(s, "</h4>")
     s <- paste(s, "<p>")
     s <- paste(s, "<b># consultations : </b>", data$n_consults, "<br/>")
     s <- paste(s, "<b># agents actifs : </b>", data$hw_consulting, "/", data$hw_total, "(", percent(data$hw_consulting, data$hw_total, 2), "%) <br/>")
     s <- paste(s, "<b>Délai de synchro : </b>", data$sync_lag, " (j)")
     s <- paste(s, "</p>")
     s
   }

  ###################
  ## Consultations ##
  ###################
  
  n_consults_csps <- reactive({
    f <- factor(visit_data_f()$mobile_user)
    table(f)
  })
  
  output$consults_n_consults_bar <- renderGvis({
    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Mois de l\\\'année'}",
                    vAxis = "{title:'Nombre de consultations'}")
    
    if (input$district != "Tous") {
      data <- table(visit_data_f()$started_year_month)
      data <- data.frame(data)
      colnames(data) <- c("month", input$district)
      options$seriesType = "bars"
      gvisColumnChart(data, options = options)
    } else {
      data <- as.data.frame.matrix(t(table(visit_data_f()$district, visit_data_f()$started_year_month)))
      names <- rownames(data)
      data <- mutate(data, total = apply(data, 1, sum))
      data <- mutate(data, total.annotation = as.character(total))
      data <- cbind(names, data)
      colnames(data)[1] <- "month"
      yvar <- colnames(data)[2:ncol(data)]
      options$series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = "")
      options$seriesType = "bars"
      options$isStacked = T
      gvisComboChart(data, xvar = "month", yvar = yvar, options = options)
    }
  })
  
  output$consults_n_consults_n <- renderValueBox({
    data <- format_number(nrow(visit_data_f()))
    valueBox(data, "Total sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_average <- renderValueBox ({
    data <- trunc(mean(n_consults_csps()))
    valueBox(data, "Moyenne par CSPS sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_min <- renderValueBox ({
    data <- min(n_consults_csps())
    valueBox(data, "Minimum sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_max <- renderValueBox ({
    data <- max(n_consults_csps())
    valueBox(data, "Maximum sur la période", icon = icon("info-circle"), color = "aqua")
  })
  
  output$consults_n_consults_tree <- renderGvis({
    data <- summarise(group_by(visit_data_f(), mobile_user, district), n_consults = length(age))
    data$color <- data$n_consults
    data$mobile_user <- as.character(data$mobile_user)    
    data$district <- as.character(data$district)
    data$district <- paste("District", data$district) # avoid duplicates with mobile_user (ex. seguenega)
    
    u <- unique(data$district)
    ds <- data.frame(cbind(u, rep("Burkina Faso", length(u)), rep(0, length(u)), rep(0, length(u))))
    colnames(ds) <- colnames(data)
    ds$n_consults <- as.numeric(levels(ds$n_consults))[ds$n_consults] # convert from factor
    ds$color <- ds$n_consults
    ds$mobile_user <- as.character(ds$mobile_user)
    ds$district <- as.character(ds$district)
    ##Erreur de transformation du vecteur en data frame
    data<-data.frame(data)
    data <- rbind(ds, data)
    data <- rbind(c("Burkina Faso", NA, 0, 0), data)
    
    options <- list(height = GL_chart_h,
                    showScale = T,
                    minColor = "#fc8d59",
                    midColor = "#ffffbf", 
                    maxColor = "#2b83ba")
    
    gvisTreeMap(data, idvar = "mobile_user", parentvar = "district",
                sizevar = "n_consults", colorvar = "color", options = options)
  })
  
  output$consults_age_sex_range_bar <- renderGvis ({
    data <- as.data.frame.matrix(t(table(visit_data_f()$sex, visit_data_f()$age_range)))
    names <- rownames(data)
    data <- mutate(data, total = apply(data, 1, sum))
    data <- mutate(data, total.annotation = as.character(total))
    data <- cbind(names, data)
    colnames(data)[1] <- "range"
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Tranche d\\\'âge (mois)'}",
                    vAxis = "{title:'Nombre de consultations'}",
                    series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
                    seriesType = "bars",
                    isStacked = T)
    
    gvisComboChart(data, xvar = "range", yvar = yvar, options = options)
  })
  
  output$consults_n_consults_range_pie <- renderGvis ({
    data <- data.frame(table(visit_data_f()$age_range))
    
    options <- list(height = GL_chart_h,
                    legend = "{position:'bottom'}")
    
    gvisPieChart(data, options = options)
  })
  
  output$consults_age_sex_age_bar <- renderGvis({
    data <- as.data.frame.matrix(t(table(visit_data_f()$sex, visit_data_f()$age)))
    data <- cbind(rownames(data), data)
    
    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Age (mois)'}",
                    vAxis = "{title:'Nombre de consultations'}",
                    isStacked = T)
    
    gvisColumnChart(data, options = options)
  })
  
  ##########################
  ## Epidemiological Data ##
  ##########################
  
  output$epi_profile_classifications_table <- renderDataTable({
    clean <- F
    if (input$class_freq == "illness") { clean <- T }
    
    classifications_frequency(visit_data_f(), clean)
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  
  output$epi_profile_combinations_table <- renderDataTable({
    clean <- F
    if (input$class_comb == "illness") { clean <- T }
    
    classifications_combinations(visit_data_f(), clean)
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10, scrollX = T))
  
  ###############
  ## REC Usage ##
  ###############
  
  output$rec_usage_who_line <- renderGvis({
    qual <- as.data.frame.matrix(t(table(visit_data_f()$qualification, visit_data_f()$started_year_month)))
    qual <- cbind(rownames(qual), qual)
    options <- list(height = GL_chart_h,hAxis ="{title:'Mois de l\\\'année'}",vAxis = "{title:'Nombre de consultations'}")
    gvisLineChart(qual, options = options)
  })
  
  output$rec_usage_who_pie <- renderGvis({  
    all_qual <- as.data.frame(table(visit_data_f()$qualification))
    options <- list(height = GL_chart_h)
    gvisPieChart(all_qual, options = options)
  })
  
  output$rec_usage_who_table <- renderDataTable({
    data <- consulting_health_workers(visit_data_f())
    data <- table(data$qualification)
    data <- data.frame(data)
  }, options = list(paging = F, searching = F, columns = list(list(title = "Qualification"), list(title = "Nombre"))))
  
  sync_lag_data <- reactive({ sync_lag(visit_data_f(), "mobile_user") })
  sync_lag_summary <- reactive({ summary(sync_lag_data()) })
  sum_data_entry <- reactive({sum(visit_data_f()$duration) + sum(treatment_data_f()$duration) + sum(enroll_data_f()$duration)})
  
  output$rec_usage_profile_hm <- renderPlot({
    data <- form_activity(visit_data_f(), "started")
    p <- ggplot(data, aes(x = hour, y = weekday, fill = count))
    p + geom_tile() + labs(x = "Heure de la journée", y = "Jour de la semaine", fill = "Qté")
  })
  
  output$rec_usage_sync_lag_bar <- renderGvis({
    summary <- sync_lag_summary()
    data <- data.frame(sync_lag_data())
    data <- cbind(rownames(data), data)
    colnames(data) <- c("csps", "delay")
    summary <- data.matrix(summary)
    data <- mutate(data, average = as.numeric(summary[4]))
    data <- mutate(data, delay = round(as.numeric(delay), 2))
    options <- list(height = GL_chart_h,
                    hAxis = "{title:'Centre de santé'}",
                    vAxis = "{title:'Délai de synchronisation (jours)'}",
                    legend = "{position:'none'}",
                    seriesType = "bars", 
                    series = "{1:{type:'line'}}")
    gvisComboChart(data, options = options)
  })
  
  output$rec_usage_sync_lag_pie <- renderGvis({
    summary <- sync_lag_summary()
    data <- round(sync_lag_data(), 2)
    data <- table(data > summary[4])
    names(data) <- c("Faux", "Vrai")
    data <- data.frame(data) 
    options = list(height = GL_chart_h, legend = "{position:'bottom'}")
    gvisPieChart(data, options = options)
  })
  
  output$rec_usage_sync_lag_avg <- renderValueBox({
    data <- sync_lag_summary()[4]
    valueBox(data, "Moyenne (jours)", icon = icon("info-circle"), color = "aqua")
  })
  
  output$rec_usage_sync_lag_n_above_avg <- renderValueBox({
    data <- sum(sync_lag_data() > sync_lag_summary()[4])
    valueBox(data, "Nb.CSPS avec un délai > moyenne", icon = icon("exclamation-circle"), color = "red")
  })
  
  output$rec_usage_sync_lag_median <- renderValueBox({
    data <- sync_lag_summary()[3]
    valueBox(data, "Médianne (jours)", icon = icon("info-circle"), color = "aqua")
  })
  
  output$data_entry_average <- renderGvis({
    e <- enroll_data_f()
    v <- visit_data_f()
    t <- treatment_data_f()
    data <- data.frame(c(mean(e$duration), mean(v$duration), mean(t$duration)))
    data <- cbind(c("Enregistrement", "Evaluation", "Traitement"), data)
    colnames(data) <- c("form", "average")
    data$average <- round(data$average, 2)
    
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    options <- list(height = GL_chart_h,
                    legend = "{position:'none'}",
                    hAxis = "{title:'Formulaire'}",
                    vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:0}",
                    series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
                    seriesType = "bars")
    
    gvisComboChart(data, xvar = "form", yvar = yvar, options = options)
  })
  
  output$data_entry_qualification <- renderGvis({
    data <- data.frame(tapply(visit_data_f()$duration, visit_data_f()$qualification, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("qualification", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
                    title = "Formulaire d'évaluation",
                    legend = "{position:'none'}",
                    hAxis = "{title:'Qualification'}",
                    vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:1}",
                    series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
                    seriesType = "bars")
    
    gvisColumnChart(data, xvar = "qualification", yvar = yvar, options = options)
  })
  
  output$data_entry_n_classifications_eval <- renderGvis({
    data <- data.frame(tapply(visit_data_f()$duration, visit_data_f()$n_classifications, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("n_classifications", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
                    title = "Formulaire d'évaluation",
                    legend = "{position:'none'}",
                    hAxis = "{title:'Nombre de classifications'}",
                    vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:1}",
                    series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
                    seriesType = "bars")
    
    gvisColumnChart(data, xvar = "n_classifications", yvar = yvar, options = options)
  })
  
  output$data_entry_n_classifications_treat <- renderGvis({
    data <- data.frame(tapply(treatment_data_f()$duration, treatment_data_f()$n_classifications, mean))
    data <- cbind(rownames(data), data)
    colnames(data) <- c("n_classifications", "average")
    data$average <- round(data$average, 2)
    data$average <- as.numeric(data$average)
    data <- mutate(data, total = average)
    data <- mutate(data, total.annotation = as.character(total))
    yvar <- colnames(data)[2:ncol(data)]
    
    options <- list(height = GL_chart_h,
                    title = "Formulaire de traitement",
                    legend = "{position:'none'}",
                    hAxis = "{title:'Nombre de classifications'}",
                    vAxis = "{title:'Durée moyenne de saisie (minutes)', minValue:0}",
                    series = paste("{", ncol(data) - 3,": {type: 'line', lineWidth: 0, visibleInLegend: 'false'}}", sep = ""),
                    seriesType = "bars")
    
    gvisColumnChart(data, xvar = "n_classifications", yvar = yvar, options = options)
  })
  
  output$data_entry_n_minutes <- renderValueBox ({
    data <- format_number(sum_data_entry())
    valueBox(data, "Nombre de minutes de saisie", icon = icon("clock-o"), color = "light-blue")
  })
  
  output$data_entry_n_hours <- renderValueBox ({
    data <- format_number(trunc(sum_data_entry() / 60))
    valueBox(data, "Nombre d'heures de saisie", icon = icon("clock-o"), color = "green")
  })
  
  output$data_entry_n_days <- renderValueBox ({
    data <- format_number(trunc(sum_data_entry() / 60 / 24))
    valueBox(data, "Nombre de jours de saisie", icon = icon("clock-o"), color = "aqua")
  })
  
  output$data_entry_average_agent <- renderValueBox ({
    n <- length(unique(visit_data_f()$health_worker_id))
    data <- format_number(trunc(sum_data_entry() / 60 / n))
    valueBox(data, "Moyenne par agent (heures)", icon = icon("clock-o"), color = "yellow")
  })
}