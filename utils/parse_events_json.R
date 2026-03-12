library(jsonlite)
library(dplyr)

parse_events_json <- function(events_file){
  
  events_json <- fromJSON(events_file, simplifyVector = FALSE)
  
  if (is.null(events_json$events)) {
    stop("No 'events' object found in events.json")
  }
  
  events <- events_json$events
  rows <- list()
  
  for(ev in events){
    
    base <- data.frame(
      event = ifelse(is.null(ev$event), NA, ev$event),
      trackedEntity = ifelse(is.null(ev$trackedEntity), NA, ev$trackedEntity),
      enrollment = ifelse(is.null(ev$enrollment), NA, ev$enrollment),
      program = ifelse(is.null(ev$program), NA, ev$program),
      programStage = ifelse(is.null(ev$programStage), NA, ev$programStage),
      orgUnit = ifelse(is.null(ev$orgUnit), NA, ev$orgUnit),
      occurredAt = ifelse(is.null(ev$occurredAt), NA, ev$occurredAt),
      scheduledAt = ifelse(is.null(ev$scheduledAt), NA, ev$scheduledAt),
      createdAt = ifelse(is.null(ev$createdAt), NA, ev$createdAt),
      updatedAt = ifelse(is.null(ev$updatedAt), NA, ev$updatedAt),
      storedBy = ifelse(is.null(ev$storedBy), NA, ev$storedBy),
      status = ifelse(is.null(ev$status), NA, ev$status),
      followUp = ifelse(is.null(ev$followUp), NA, ev$followUp),
      deleted = ifelse(is.null(ev$deleted), NA, ev$deleted),
      completedAt = ifelse(is.null(ev$completedAt), NA, ev$completedAt),
      stringsAsFactors = FALSE
    )
    
    dvs <- ev$dataValues
    
    if(!is.null(dvs) && length(dvs) > 0){
      
      for(i in seq_along(dvs)){
        
        row <- base
        row$dataElement <- ifelse(is.null(dvs[[i]]$dataElement), NA, dvs[[i]]$dataElement)
        row$value <- ifelse(is.null(dvs[[i]]$value), NA, as.character(dvs[[i]]$value))
        
        rows[[length(rows) + 1]] <- row
      }
      
    } else {
      
      row <- base
      row$dataElement <- NA
      row$value <- NA
      
      rows[[length(rows) + 1]] <- row
    }
  }
  
  events_long <- bind_rows(rows)
  
  return(events_long)
}