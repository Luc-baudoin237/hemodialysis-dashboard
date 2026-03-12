library(shiny)
library(bs4Dash)
library(tidyverse)
library(jsonlite)
library(plotly)
library(lubridate)
library(DT)
library(reactable)
library(janitor)
library(readr)

source("utils/parse_events_json.R")
source("utils/build_dictionary.R")
source("utils/clean_tracked_entities.R")
source("utils/merge_tracker_data.R")
source("utils/quality_checks.R")
source("utils/helpers.R")

data_path <- "data"
output_path <- "output"

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

events_file <- file.path(data_path, "events.json")
metadata_file <- file.path(data_path, "metadata.json")
tei_file <- file.path(data_path, "trackedEntities.csv")

events_long <- parse_events_json(events_file)
dictionary <- build_dictionary(metadata_file)
tei <- clean_tracked_entities(tei_file)

merged <- merge_tracker_data(events_long, dictionary, tei)

events_long_labeled <- merged$events_long
events_wide_clean <- merged$events_wide
final_db <- merged$final_db

readr::write_csv(dictionary, file.path(output_path, "dictionary_data_elements.csv"))
readr::write_csv(events_long_labeled, file.path(output_path, "events_long_labeled.csv"))
readr::write_csv(events_wide_clean, file.path(output_path, "events_wide_clean.csv"))
readr::write_csv(tei, file.path(output_path, "tracked_entities_wide_clean.csv"))
readr::write_csv(final_db, file.path(output_path, "final_merged_tracker_database.csv"))

safe_dates <- safe_as_date(events_wide_clean$occurredAt)
min_date <- suppressWarnings(min(safe_dates, na.rm = TRUE))
max_date <- suppressWarnings(max(safe_dates, na.rm = TRUE))

if (is.infinite(min_date) || is.na(min_date)) min_date <- Sys.Date() - 30
if (is.infinite(max_date) || is.na(max_date)) max_date <- Sys.Date()

orgunit_choices <- c("All", sort(unique(stats::na.omit(events_wide_clean$orgUnit))))
stage_choices <- c("All", sort(unique(stats::na.omit(events_wide_clean$programStage))))
user_choices <- c("All", sort(unique(stats::na.omit(events_wide_clean$storedBy))))
status_choices <- c("All", sort(unique(stats::na.omit(events_wide_clean$status))))
followup_choices <- c("All", sort(unique(as.character(stats::na.omit(events_wide_clean$followUp)))))