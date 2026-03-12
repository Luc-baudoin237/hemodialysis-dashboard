orgunitUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "orgunits",
    
    fluidRow(
      box(
        width = 6,
        title = "Events by Facility / Org Unit",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("events_by_orgunit"), height = 350)
      ),
      box(
        width = 6,
        title = "Timeliness by Facility",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("timeliness_by_orgunit"), height = 350)
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Facility Performance Table",
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("orgunit_table"))
      )
    )
  )
}

orgunitServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    org_data <- reactive({
      df <- data()
      if (!is.data.frame(df) || !"orgUnit" %in% names(df)) return(data.frame())
      
      df$orgUnit <- ifelse(is.na(df$orgUnit) | df$orgUnit == "", "Unknown", as.character(df$orgUnit))
      df$occurred_date <- if ("occurredAt" %in% names(df)) safe_as_date(df$occurredAt) else as.Date(NA)
      df$created_date <- if ("createdAt" %in% names(df)) safe_as_date(df$createdAt) else as.Date(NA)
      df$entry_delay_days <- as.numeric(df$created_date - df$occurred_date)
      
      out <- df |>
        dplyr::group_by(orgUnit) |>
        dplyr::summarise(
          total_events = if ("event" %in% names(df)) dplyr::n_distinct(event) else dplyr::n(),
          total_patients = if ("trackedEntity" %in% names(df)) dplyr::n_distinct(trackedEntity) else NA,
          avg_delay_days = mean(entry_delay_days, na.rm = TRUE),
          median_delay_days = median(entry_delay_days, na.rm = TRUE),
          .groups = "drop"
        )
      
      out$avg_delay_days[is.nan(out$avg_delay_days)] <- NA
      out$median_delay_days[is.infinite(out$median_delay_days)] <- NA
      out[order(-out$total_events), , drop = FALSE]
    })
    
    output$events_by_orgunit <- renderPlotly({
      df <- org_data()
      if (nrow(df) == 0) return(empty_plotly("No facility data available"))
      
      plotly::plot_ly(
        data = df,
        x = ~total_events,
        y = ~reorder(orgUnit, total_events),
        type = "bar",
        orientation = "h"
      ) |>
        plotly::layout(
          xaxis = list(title = "Events"),
          yaxis = list(title = "Org Unit")
        )
    })
    
    output$timeliness_by_orgunit <- renderPlotly({
      df <- org_data()
      df <- df[!is.na(df$avg_delay_days), , drop = FALSE]
      if (nrow(df) == 0) return(empty_plotly("No timeliness data available"))
      
      plotly::plot_ly(
        data = df,
        x = ~avg_delay_days,
        y = ~reorder(orgUnit, avg_delay_days),
        type = "bar",
        orientation = "h"
      ) |>
        plotly::layout(
          xaxis = list(title = "Average Delay (days)"),
          yaxis = list(title = "Org Unit")
        )
    })
    
    output$orgunit_table <- renderDT({
      DT::datatable(
        org_data(),
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