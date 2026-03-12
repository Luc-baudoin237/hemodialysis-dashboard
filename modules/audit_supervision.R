auditUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(
    tabName = "audit",
    
    fluidRow(
      
      box(
        width = 6,
        title = "Duplicate Events",
        status = "warning",
        solidHeader = TRUE,
        DTOutput(ns("duplicate_events"))
      ),
      
      box(
        width = 6,
        title = "Duplicate Patients",
        status = "danger",
        solidHeader = TRUE,
        DTOutput(ns("duplicate_patients"))
      )
      
    )
    
  )
}



auditServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    
    output$duplicate_events <- renderDT({
      
      df <- data()
      req("event" %in% names(df))
      
      dup <- df %>%
        group_by(event) %>%
        filter(n() > 1)
      
      datatable(dup)
      
    })
    
    
    output$duplicate_patients <- renderDT({
      
      df <- data()
      req("trackedEntity" %in% names(df))
      
      dup <- df %>%
        group_by(trackedEntity) %>%
        filter(n() > 1)
      
      datatable(dup)
      
    })
    
    
  })
  
}