library(reports)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(shinydashboard)
library(googleVis)
library(rdrop2)
library(dtplyr)
library(data.table)

source("scripts/load-data.R")
source("scripts/transforms.R")

ieda_start_date <- ymd("2014-12-15")


locations_data <- locations_load_data("data/locations.csv")
locations_data[which(is.na(locations_data) == TRUE, arr.ind=TRUE)] <- 0
tablet_users_data <- tablet_users_load_data("data/tablet-users.csv")
mobile_users_data <- mobile_users_load_data("data/mobile-users.csv", locations_data)
enroll_data <- enroll_child_load_data("data/enroll-child.csv", mobile_users_data)
mobile_users_data[which(is.na(mobile_users_data) == TRUE, arr.ind=TRUE)] <- 0
tablet_users_data[which(is.na(tablet_users_data) == TRUE, arr.ind=TRUE)] <- 0
visit_data <- child_visit_load_data("data/child-visit.csv",tablet_users_data, mobile_users_data)
visit_data[which(is.na(visit_data) == TRUE, arr.ind=TRUE)] <- 0
treatment_data <- child_treatment_load_data("data/child-treatment.csv", mobile_users_data)

GL_n_consults <- format_number(nrow(visit_data))
GL_n_patients <- format_number(length(unique(visit_data$case_id)))
GL_n_districts <- as.character(length(levels(visit_data$district)))
GL_n_mobile_users <- as.character(length(levels(visit_data$mobile_user)))
GL_n_health_workers <- as.character(nrow(consulting_health_workers(visit_data)))

GL_chart_w = "100%"
GL_chart_h = 400
GL_fist_consult_date = min(visit_data$started_on)
GL_last_consult_date = max(visit_data$started_on)
