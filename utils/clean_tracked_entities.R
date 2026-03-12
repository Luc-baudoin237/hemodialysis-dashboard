library(dplyr)
library(tidyr)
library(readr)
library(janitor)

clean_tracked_entities <- function(tei_file) {
  tei <- readr::read_csv(tei_file, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  if (!"tracked_entity" %in% names(tei)) {
    stop("tracked_entity column not found in tracked entities file.")
  }
  
  attr_name_col <- dplyr::case_when(
    "display_name" %in% names(tei) ~ "display_name",
    "attribute" %in% names(tei) ~ "attribute",
    TRUE ~ NA_character_
  )
  
  if (is.na(attr_name_col)) {
    stop("No attribute name column found (expected display_name or attribute).")
  }
  
  if (!"value" %in% names(tei)) {
    stop("value column not found in tracked entities file.")
  }
  
  tei_clean <- tei %>%
    mutate(
      attribute_label = .data[[attr_name_col]],
      attribute_label = ifelse(is.na(attribute_label) | attribute_label == "", "unknown_attribute", attribute_label),
      attribute_label = janitor::make_clean_names(attribute_label),
      value = as.character(value)
    ) %>%
    select(tracked_entity, attribute_label, value) %>%
    distinct()
  
  tei_wide <- tei_clean %>%
    pivot_wider(
      names_from = attribute_label,
      values_from = value,
      values_fn = \(x) paste(unique(na.omit(x)), collapse = " | ")
    ) %>%
    rename(trackedEntity = tracked_entity)
  
  tei_wide
}