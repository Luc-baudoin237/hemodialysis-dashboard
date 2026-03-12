overviewUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "overview",
    
    fluidRow(
      valueBoxOutput(ns("patients")),
      valueBoxOutput(ns("events")),
      valueBoxOutput(ns("variables")),
      valueBoxOutput(ns("completeness"))
    ),
    
    fluidRow(
      valueBoxOutput(ns("facilities")),
      valueBoxOutput(ns("avg_events_per_patient")),
      valueBoxOutput(ns("date_coverage")),
      valueBoxOutput(ns("active_users"))
    ),
    
    fluidRow(
      box(
        width = 8,
        title = "Event Trend Over Time",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("events_trend"), height = 350)
      ),
      box(
        width = 4,
        title = "Quick Quality Snapshot",
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("quality_snapshot"))
      )
    )
  )
}

overviewServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$patients <- renderValueBox({
      df <- data()
      bs4Dash::valueBox(
        value = if ("trackedEntity" %in% names(df)) dplyr::n_distinct(df$trackedEntity) else 0,
        subtitle = "Tracked Entities",
        icon = icon("users"),
        color = "primary"
      )
    })
    
    output$events <- renderValueBox({
      df <- data()
      bs4Dash::valueBox(
        value = if ("event" %in% names(df)) dplyr::n_distinct(df$event) else 0,
        subtitle = "Events",
        icon = icon("calendar"),
        color = "success"
      )
    })
    
    output$variables <- renderValueBox({
      df <- data()
      bs4Dash::valueBox(
        value = ncol(df),
        subtitle = "Variables",
        icon = icon("table"),
        color = "warning"
      )
    })
    
    output$completeness <- renderValueBox({
      df <- data()
      score <- if (nrow(df) == 0) 0 else dataset_completion_score(df)
      bs4Dash::valueBox(
        value = paste0(score, "%"),
        subtitle = "Completeness Score",
        icon = icon("percent"),
        color = "info"
      )
    })
    
    output$facilities <- renderValueBox({
      df <- data()
      bs4Dash::valueBox(
        value = if ("orgUnit" %in% names(df)) dplyr::n_distinct(df$orgUnit) else 0,
        subtitle = "Facilities / Org Units",
        icon = icon("hospital"),
        color = "primary"
      )
    })
    
    output$avg_events_per_patient <- renderValueBox({
      df <- data()
      ev <- if ("event" %in% names(df)) dplyr::n_distinct(df$event) else 0
      pt <- if ("trackedEntity" %in% names(df)) dplyr::n_distinct(df$trackedEntity) else 0
      bs4Dash::valueBox(
        value = ifelse(pt == 0, 0, round(ev / pt, 2)),
        subtitle = "Average Events per Patient",
        icon = icon("chart-line"),
        color = "success"
      )
    })
    
    output$date_coverage <- renderValueBox({
      df <- data()
      dd <- if ("occurredAt" %in% names(df)) safe_as_date(df$occurredAt) else as.Date(character())
      dd <- dd[!is.na(dd)]
      txt <- if (length(dd) == 0) "Unavailable" else paste0(min(dd), " to ", max(dd))
      bs4Dash::valueBox(
        value = txt,
        subtitle = "Date Coverage",
        icon = icon("clock"),
        color = "warning"
      )
    })
    
    output$active_users <- renderValueBox({
      df <- data()
      val <- if ("storedBy" %in% names(df)) dplyr::n_distinct(df$storedBy[!(is.na(df$storedBy) | df$storedBy == "")]) else 0
      bs4Dash::valueBox(
        value = val,
        subtitle = "Active Data Collectors",
        icon = icon("user"),
        color = "danger"
      )
    })
    
    output$events_trend <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"occurredAt" %in% names(df)) return(empty_plotly("No event trend available"))
      
      plot_df <- data.frame(
        event_date = safe_as_date(df$occurredAt)
      )
      plot_df <- plot_df[!is.na(plot_df$event_date), , drop = FALSE]
      if (nrow(plot_df) == 0) return(empty_plotly("No event trend available"))
      
      plot_df <- dplyr::count(plot_df, event_date, name = "n")
      plot_df <- plot_df[order(plot_df$event_date), , drop = FALSE]
      
      plotly::plot_ly(
        data = plot_df,
        x = ~event_date,
        y = ~n,
        type = "scatter",
        mode = "lines+markers"
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Events")
        )
    })
    
    output$quality_snapshot <- renderDT({
      DT::datatable(
        quick_quality_snapshot(data()),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)
      )
    })
  })
}