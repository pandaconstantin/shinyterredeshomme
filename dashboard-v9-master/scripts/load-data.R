#######################
## Load tablet users ##
#######################
tablet_users_load_data <- function(filename) {
  users <- as.data.frame(fread(filename))
  users <- select(users, info.case_id, qualification, user_type, info.opened_by_username)
  users <- dplyr::rename(users, health_worker_id = info.case_id, mobile_user = info.opened_by_username)
  users <- filter(users, qualification != "ibe")
  users$qualification <- factor(users$qualification)
  users$qualification <- revalue(users$qualification, c("ab" = "AB", "accoucheuse" = "Accoucheuse", "ais" = "AIS", "fe_me" = "Sage femme", "ib" = "IB", "ide" = "IDE", "other" = "Autre"))
  users$user_type <- revalue(users$user_type, c("admin" = "ICP", "regular" = "Agent"))
  users <- filter(users, !grepl("helpdesk|test|formation|demo|lshtm", mobile_user))
  users$mobile_user <- factor(users$mobile_user)
  users
}

#######################
## Load mobile users ##
#######################
mobile_users_load_data <- function(filename, locations) {
  users <- as.data.frame(fread(filename))
  colnames(users) <- c("mobile_user", "site_code", "district")
  users <- select(users, mobile_user, site_code) # TODO: Update file to exclude district column
  users <- filter(users, !grepl("helpdesk|test|formation|demo|lshtm", mobile_user))
  users <- merge(users, locations, by.x = "site_code", by.y = "site_code")
  users$district <- factor(users$district)
  users$mobile_user <- factor(users$mobile_user)
  users$site_code <- factor(users$site_code)
  users
}

####################
## Load locations ##

####################
locations_load_data <- function(filename) {
  locations <- as.data.frame(fread(filename))
  colnames(locations) <- c("site_code", "name", "district", "latitude", "longitude")
  locations
}

##################################
## Load Form Data: Enroll Child ##
##################################
enroll_child_load_data <- function(filename, mobile_users) {
  data <- as.data.frame(fread(filename))
  colnames(data) <- c("started_on", "completed_on", "received_on", "dob_known", "mobile_user", "sex")
  data <- filter(data, !grepl("helpdesk|test|formation|demo|lshtm", mobile_user))
  data$mobile_user <- factor(data$mobile_user)
  data <- merge(data, mobile_users, by.x = "mobile_user", by.y = "mobile_user")
  data$district <- factor(data$district)
  data$sex <- mapvalues(data$sex, from = c("female", "male"), to = c("Fille", "Garçon"))
  data <- date_time_duration_columns(data)
  data <- filter(data, started_on >= ieda_start_date)
  data
}

##################################
## Load Form Data: Child Visit ##
##################################
child_visit_load_data <- function(filename, tablet_users, mobile_users) {
  data <- as.data.frame(fread(filename))
  data[which(is.na(data) == TRUE, arr.ind=TRUE)] <- 0
  colnames(data) <- c("started_on", "completed_on", "received_on", "health_worker_id", "mobile_user", "case_id",
                      "height", "weight", "temperature", "muac",
                      "classification_oreille", "classification_diahree", "classification_vih", "classification_paludisme",
                      "classification_deshydratation", "classification_pneumonie", "classification_malnutrition",
                      "classification_rougeole", "classification_anemie", "classification_dysenterie",
                      "visit_date", "dob", "sex", "visit_type",
                      "l_wfa", "m_wfa", "s_wfa", "zscore_wfa",
                      "l_wfh", "m_wfh", "s_wfh", "zscore_wfh",
                      "l_hfa", "m_hfa", "s_hfa", "zscore_hfa")
  
  data <- merge(data, mobile_users, by = "mobile_user")
  data <- merge(data, select(tablet_users, -mobile_user), by.x = "health_worker_id", by.y = "health_worker_id")
  data <- filter(data, !grepl("helpdesk|test|formation|demo|lshtm", mobile_user))
  data$mobile_user <- factor(data$mobile_user)  
  data$health_worker_id <- factor(data$health_worker_id)  
  data$district <- factor(data$district)
  data$site_code <- factor(data$site_code)
  data$visit_date <- as.POSIXct(data$visit_date, format = "%Y-%m-%d")
  data$dob <- as.POSIXct(data$dob, format = "%Y-%m-%d")
  data$muac <- as.numeric(data$muac)
  data$muac <- as.numeric(levels(data$muac))[data$muac] # Convert MUAC to numeric
  data$sex <- mapvalues(data$sex, from = c("female", "male"), to = c("Fille", "Garçon"))
  data$classification_oreille <- revalue_classifications(data$classification_oreille, "oreille")
  data$classification_diahree <- revalue_classifications(data$classification_diahree, "diahree")
  data$classification_vih <- revalue_classifications(data$classification_vih, "vih")
  data$classification_paludisme <- revalue_classifications(data$classification_paludisme, "paludisme")
  data$classification_deshydratation <- revalue_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_pneumonie <- revalue_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_malnutrition <- revalue_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_rougeole <- revalue_classifications(data$classification_rougeole, "rougeole")
  data$classification_anemie <- revalue_classifications(data$classification_anemie, "anemie")
  data$classification_dysenterie <- revalue_classifications(data$classification_dysenterie, "dysenterie")
  data$n_classifications <- apply(data, 1, count_classifications)
  #oreille, diahree, anemie, dysenterie - nothing to clean
  data$classification_clean_oreille <- data$classification_oreille
  data$classification_clean_diahree <- data$classification_diahree
  data$classification_clean_vih <- clean_classifications(data$classification_vih, "vih")
  data$classification_clean_paludisme <- clean_classifications(data$classification_paludisme, "paludisme")
  data$classification_clean_deshydratation <- clean_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_clean_pneumonie <- clean_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_clean_malnutrition <- clean_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_clean_rougeole <- clean_classifications(data$classification_rougeole, "rougeole")
  data$classification_clean_anemie <- data$classification_anemie
  data$classification_clean_dysenterie <- data$classification_dysenterie
  data$n_clean_classifications <- apply(data, 1, count_clean_classifications)
  data <- date_time_duration_columns(data)  
  # Add age related columns in months
  data$age <- floor(as.numeric(difftime(data$started_on, data$dob, units = "weeks")) / 4) # in months
  data$age_range <- to_age_range_month(data$age)
  # Remove data dating from before the beginning of the project
  data <- filter(data, started_on >= ieda_start_date)
  data
}

###############################
## Load Form Data: Treatment ##
###############################
child_treatment_load_data <- function(filename, mobile_users) {
  data <- as.data.frame(fread(filename))
  colnames(data) <- c("started_on", "completed_on", "received_on", "mobile_user", "child_case_id",
                      "treatment_pneumonie_grave", "treatment_deshydratation_severe_grave",
                      "treatment_diahree_persistante_severe_grave", "treatment_paludisme_grave",
                      "med_perfusion_p1_a", "med_perfusion_p1_b", "med_perfusion_p2_a", "med_perfusion_p2_b",
                      "med_deparasitage", "med_artemether", "med_vitamine_a", "med_antibio",
                      "classification_oreille", "classification_diahree", "classification_vih", "classification_paludisme",
                      "classification_deshydratation", "classification_pneumonie", "classification_malnutrition",
                      "classification_rougeole", "classification_anemie", "classification_dysenterie",
                      "age", "weight")

  data <- filter(data, !grepl("test|formation|demo", mobile_user))
  data$mobile_user <- factor(data$mobile_user)
  data <- merge(data, mobile_users, by.x = "mobile_user", by.y = "mobile_user")
  data$district <- factor(data$district)
  data$classification_oreille <- revalue_classifications(data$classification_oreille, "oreille")
  data$classification_diahree <- revalue_classifications(data$classification_diahree, "diahree")
  data$classification_vih <- revalue_classifications(data$classification_vih, "vih")
  data$classification_paludisme <- revalue_classifications(data$classification_paludisme, "paludisme")
  data$classification_deshydratation <- revalue_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_pneumonie <- revalue_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_malnutrition <- revalue_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_rougeole <- revalue_classifications(data$classification_rougeole, "rougeole")
  data$classification_anemie <- revalue_classifications(data$classification_anemie, "anemie")
  data$classification_dysenterie <- revalue_classifications(data$classification_dysenterie, "dysenterie")
  data$n_classifications <- apply(data, 1, count_classifications)
  # oreille, diahree, anemie, dysenterie - nothing to clean
  data$classification_clean_oreille <- data$classification_oreille
  data$classification_clean_diahree <- data$classification_diahree
  data$classification_clean_vih <- clean_classifications(data$classification_vih, "vih")
  data$classification_clean_paludisme <- clean_classifications(data$classification_paludisme, "paludisme")
  data$classification_clean_deshydratation <- clean_classifications(data$classification_deshydratation, "deshydratation")
  data$classification_clean_pneumonie <- clean_classifications(data$classification_pneumonie, "pneumonie")
  data$classification_clean_malnutrition <- clean_classifications(data$classification_malnutrition, "malnutrition")
  data$classification_clean_rougeole <- clean_classifications(data$classification_rougeole, "rougeole")
  data$classification_clean_anemie <- data$classification_anemie
  data$classification_clean_dysenterie <- data$classification_dysenterie
  data$n_clean_classifications <- apply(data, 1, count_clean_classifications)
  data <- date_time_duration_columns(data)
  # Remove data dating from before the beginning of the project
  data <- filter(data, started_on >= ieda_start_date)
  data
}

#######################
## Utility functions ##
#######################

date_time_duration_columns <- function(data) {
  data$started_on <- as.POSIXct(data$started_on, format = "%Y-%m-%d %H:%M")
  data$completed_on <- as.POSIXct(data$completed_on, format = "%Y-%m-%d %H:%M")
  data$received_on <- as.POSIXct(data$received_on, format = "%Y-%m-%d %H:%M")
  
  # Add weekday & time columns
  data$started_hour <- factor(hour(data$started_on))
  data$started_weekday <- ordered(weekdays(data$started_on), levels = weekdays_vector())
  data$started_month <- lubridate::month(data$started_on, label = T, abbr = F)
  data$started_year <- year(data$started_on)
  data$started_year_month <- strftime(data$started_on, format = "%Y-%m")
  
  data$completed_hour <- factor(hour(data$completed_on))
  data$completed_weekday <- ordered(weekdays(data$completed_on), levels = weekdays_vector())
  data$completed_month <- lubridate::month(data$completed_on, label = T, abbr = F)
  data$completed_year <- year(data$completed_on)
  data$completed_year_month <- strftime(data$completed_on, format = "%Y-%m")
  
  data$received_hour <- factor(hour(data$received_on))
  data$received_weekday <- ordered(weekdays(data$received_on), levels = weekdays_vector())
  data$received_month <- lubridate::month(data$received_on, label = T, abbr = F)
  data$received_year <- year(data$received_on)
  data$received_year_month <- strftime(data$received_on, format = "%Y-%m")
  
  # Add duration colummn
  data$duration <- as.numeric(difftime(data$completed_on, data$started_on, units = "mins"))
  data$sync_lag <- as.numeric(difftime(data$received_on, data$completed_on, units = "mins"))
  
  data
}

weekdays_vector <- function() {
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
}

revalue_classifications <- function(var, classification) {
  if (classification == "oreille") {
    var <- mapvalues(var,
            from = c("infection_aigue_oreille", "infection_chronique_oreille", "mastoidite", ""),
            to = c("Infection aigüe", "Infection chronique", "Mastoïdite", "---"))
  } else if (classification == "diahree") {
    var <- mapvalues(var,
            from = c("diahree_persistante", ""),
            to = c("Diahree persistente", "---"))
  } else if (classification == "vih") {
    var <- mapvalues(var,
            from = c("vih_confirmee", "vih_pas", "vih_peu_probable", "vih_possible", "vih_symp_confirmee", "vih_symp_probable", "vih_symp_suspecte", ""),
            to = c("VIH confirmé", "Pas de VIH", "VIH peu probable", "VIH possible", "VIH symp. comfirmé", "VIH symp. probable", "VIH symp. suspecte", "---"))    
  } else if (classification == "paludisme") {
    var <- mapvalues(var,
            from = c("paludisme_grave", "paludisme_grave_sans_tdr", "paludisme_simple", "pas_paludisme", ""),
            to = c("Paludisme Grave", "Paludisme grave - TDR-", "Paludisme simple", "Pas de paludisme", "---"))
  } else if (classification == "deshydratation") {
    var <- mapvalues(var,
            from = c("deshydratation_severe_grave", "pas_deshydratation", "signes_deshydratation", ""),
            to = c("Deshy. grave", "Pas de deshy.", "Signes évidents de deshy.", "---"))
  } else if (classification == "pneumonie") {
    var <- mapvalues(var,
            from = c("pas_pneumonie", "pneumonie", "pneumonie_grave", ""),
            to = c("Pas de pneumonie", "Pneumonie", "Pneumonie grave", "---"))
  } else if (classification == "malnutrition") {
    var <- mapvalues(var,
            from = c("mam", "masc", "mass", "pas_malnutrition", ""),
            to = c("MAM", "MASc", "MASs", "Pas de malnutrition", "---"))
  } else if (classification == "rougeole") {
    var <- mapvalues(var,
            from = c("antecedent_rougeole", "rougeole", "rougeole_complications", "rougeole_compliquee", ""),
            to = c("Antécédents", "Rougeole", "Complications yeux/bouche", "Rougeole compliquée", "---"))
  } else if (classification == "anemie") {
    var <- mapvalues(var,
            from = c("anemie", "anemie_grave", ""),
            to = c("Anémie", "Anémie grave", "---"))
  } else if (classification == "dysenterie") {
    var <- mapvalues(var,
            from = c("dysenterie", ""),
            to = c("Dysenterie", "---"))
  }
  var
}

count_classifications <- function (consult) {
  (consult["classification_oreille"] != "---") + (consult["classification_diahree"] != "---") +
    (consult["classification_vih"] != "---") + (consult["classification_paludisme"] != "---") +
    (consult["classification_deshydratation"] != "---") + (consult["classification_pneumonie"] != "---") +
    (consult["classification_malnutrition"] != "---") + (consult["classification_rougeole"] != "---") +
    (consult["classification_anemie"] != "---") + (consult["classification_dysenterie"] != "---")
}

count_clean_classifications <- function (consult) {
  (consult["classification_clean_oreille"] != "---") + (consult["classification_clean_diahree"] != "---") +
    (consult["classification_clean_vih"] != "---") + (consult["classification_clean_paludisme"] != "---") +
    (consult["classification_clean_deshydratation"] != "---") + (consult["classification_clean_pneumonie"] != "---") +
    (consult["classification_clean_malnutrition"] != "---") + (consult["classification_clean_rougeole"] != "---") +
    (consult["classification_clean_anemie"] != "---") + (consult["classification_clean_dysenterie"] != "---")
}

clean_classifications <- function(var, classification) {
  # oreille, diahree, anemie, dysenterie - nothing to clean
  if (classification == "vih") {
    var <- "---"
  } else if (classification == "paludisme") {
    var[var == "Pas de paludisme"] <- "---"
  } else if (classification == "deshydratation") {
    var[var == "Pas de deshy."] <- "---"
  } else if (classification == "pneumonie") {
    var[var == "Pas de pneumonie"] <- "---"
  } else if (classification == "malnutrition") {
    var[var == "Pas de malnutrition"] <- "---"
  } else if (classification == "rougeole") {
    var[var == "Antécédents"] <- "Rougeole"
  }
  var <- factor(var)
  var
}

to_age_range_month <- function (data) {
  data <- findInterval(data, c(0, 2, 6, 12, 24, 36, 48, 60, 100))
  data <- factor(data)
  data <- mapvalues(data,
                   from = c(1, 2, 3, 4, 5, 6, 7, 8),
                   to = c("0-2", "2-6", "6-12", "12-24", "24-36", "36-48", "48-60", ">=60" ), warn_missing = FALSE)
  data
}