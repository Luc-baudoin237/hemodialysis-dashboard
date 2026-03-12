patientUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "patient",
    
    fluidRow(
      box(
        width = 4,
        title = "Patient / Tracked Entity Search",
        status = "primary",
        solidHeader = TRUE,
        selectizeInput(
          ns("patient_id"),
          "Tracked Entity",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Select a tracked entity")
        )
      ),
      box(
        width = 8,
        title = "Patient Summary",
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("patient_summary"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "All Records for Selected Patient",
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("patient_data"))
      )
    )
  )
}

patientServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df <- data()
      req(is.data.frame(df))
      req("trackedEntity" %in% names(df))
      
      choices <- sort(unique(df$trackedEntity[!(is.na(df$trackedEntity) | df$trackedEntity == "")]))
      
      updateSelectizeInput(
        session,
        "patient_id",
        choices = choices,
        server = TRUE
      )
    })
    
    patient_df <- reactive({
      df <- data()
      req(is.data.frame(df))
      req("trackedEntity" %in% names(df))
      req(input$patient_id)
      
      df %>%
        filter(trackedEntity == input$patient_id)
    })
    
    output$patient_summary <- renderDT({
      df <- patient_df()
      req(nrow(df) > 0)
      
      summary_df <- tibble(
        metric = c(
          "Tracked Entity",
          "Number of records",
          "Number of events",
          "Number of org units",
          "Date coverage"
        ),
        value = c(
          input$patient_id,
          nrow(df),
          if ("event" %in% names(df)) dplyr::n_distinct(df$event) else NA,
          if ("orgUnit" %in% names(df)) dplyr::n_distinct(df$orgUnit) else NA,
          if ("occurredAt" %in% names(df)) {
            dd <- safe_as_date(df$occurredAt)
            dd <- dd[!is.na(dd)]
            if (length(dd) == 0) NA else paste0(min(dd), " to ", max(dd))
          } else {
            NA
          }
        )
      )
      
      datatable(
        summary_df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)
      )
    })
    
    output$patient_data <- renderDT({
      datatable(
        patient_df(),
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })
  })
}