sidebar <- dashboardSidebar(
  selectInput(inputId = "district", label = "District", choices = c("Tous",levels(visit_data$district))),

  dateRangeInput("period", label = "Période", separator = "à", language = "fr", start = GL_fist_consult_date, end = GL_last_consult_date),

  sidebarMenu(
    menuItem("Aperçu", tabName = "overview", icon = icon("dashboard")),
    
    menuItem("Carte", tabName = "geo", icon = icon("globe")),
    
    menuItem("Consultations", icon = icon("stethoscope"),
      menuSubItem("Nombre de consultations", tabName = "consults_n_consults"),
      menuSubItem("Age et sexe des consultants", tabName = "consults_age_sex")
    ),
    
    menuItem("Profil epidémio.", icon = icon("area-chart"),
      menuSubItem("Fréquence des classifications", tabName = "epi_profile_classifications"),
      menuSubItem("Fréquence des combinaisons", tabName = "epi_profile_combinations")
    ),
    
    menuItem("Utilisation du REC", icon = icon("tablet"),
      menuSubItem("Qui utilise le REC ?", tabName = "rec_usage_who"),
      menuSubItem("Heure d'utilisation", tabName = "rec_usage_profile"),
      menuSubItem("Délai de synchronisation", tabName = "rec_usage_sync_lag")
    ),
    
    menuItem("Durée de saisie", icon = icon("clock-o"), tabName = "data_entry")
  )
)