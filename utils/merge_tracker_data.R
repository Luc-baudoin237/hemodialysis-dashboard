library(dplyr)
library(tidyr)

merge_tracker_data <- function(events_long, dictionary, tei){
  
  if(!"dataElement" %in% names(events_long)){
    stop("Column 'dataElement' not found in events_long")
  }
  
  if(!"dataElement" %in% names(dictionary)){
    stop("Column 'dataElement' not found in dictionary")
  }
  
  events_long_labeled <- events_long %>%
    left_join(dictionary, by = "dataElement")
  
  events_wide <- events_long_labeled %>%
    mutate(
      shortName = ifelse(is.na(shortName) | shortName == "", paste0("de_", dataElement), shortName)
    ) %>%
    select(
      event, trackedEntity, enrollment, program, programStage,
      orgUnit, occurredAt, scheduledAt, createdAt, updatedAt,
      storedBy, status, followUp, deleted, completedAt,
      shortName, value
    ) %>%
    pivot_wider(
      names_from = shortName,
      values_from = value,
      values_fn = \(x) paste(unique(na.omit(x)), collapse = " | ")
    )
  
  final_db <- events_wide %>%
    left_join(tei, by = "trackedEntity")
  
  return(list(
    events_long = events_long_labeled,
    events_wide = events_wide,
    final_db = final_db
  ))
}