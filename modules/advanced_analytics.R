analyticsUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(
    tabName = "analytics",
    
    fluidRow(
      
      box(
        width = 6,
        title = "Monthly Event Volume",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("monthly_events"), height = 350)
      ),
      
      box(
        width = 6,
        title = "Facility Clustering",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("cluster_plot"), height = 350)
      )
      
    ),
    
    fluidRow(
      
      box(
        width = 12,
        title = "Cluster Table",
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("cluster_table"))
      )
      
    )
  )
}


analyticsServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    monthly_data <- reactive({
      
      df <- data()
      req("occurredAt" %in% names(df))
      
      df %>%
        mutate(
          event_date = safe_as_date(occurredAt),
          month = lubridate::floor_date(event_date, "month")
        ) %>%
        filter(!is.na(month)) %>%
        count(month, name = "n") %>%
        arrange(month)
      
    })
    
    
    output$monthly_events <- renderPlotly({
      
      df <- monthly_data()
      req(nrow(df) > 0)
      
      p <- ggplot(df, aes(month, n)) +
        geom_line() +
        geom_point() +
        labs(x = "Month", y = "Events")
      
      ggplotly(p)
      
    })
    
    
    cluster_data <- reactive({
      
      df <- data()
      req(all(c("orgUnit", "event", "trackedEntity") %in% names(df)))
      
      tmp <- df %>%
        group_by(orgUnit) %>%
        summarise(
          total_events = n_distinct(event),
          total_patients = n_distinct(trackedEntity),
          .groups = "drop"
        )
      
      req(nrow(tmp) >= 3)
      
      features <- tmp %>%
        select(total_events, total_patients) %>%
        scale()
      
      km <- kmeans(features, centers = 3)
      
      tmp$cluster <- as.factor(km$cluster)
      
      tmp
      
    })
    
    
    output$cluster_plot <- renderPlotly({
      
      df <- cluster_data()
      
      p <- ggplot(
        df,
        aes(
          total_events,
          total_patients,
          color = cluster,
          text = orgUnit
        )
      ) +
        geom_point(size = 4)
      
      ggplotly(p, tooltip = c("text", "x", "y"))
      
    })
    
    
    output$cluster_table <- renderDT({
      
      datatable(
        cluster_data(),
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