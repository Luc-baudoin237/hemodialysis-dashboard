performanceUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "performance",
    
    fluidRow(
      box(
        width = 6,
        title = "Entries by User",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("entries_user"), height = 350)
      ),
      box(
        width = 6,
        title = "Entries by Month",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("entries_month"), height = 350)
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "User Performance Table",
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("performance_table"))
      )
    )
  )
}

performanceServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    perf_df <- reactive({
      df <- data()
      if (!is.data.frame(df) || !"storedBy" %in% names(df)) return(data.frame())
      
      df$storedBy <- ifelse(is.na(df$storedBy) | df$storedBy == "", "Unknown", as.character(df$storedBy))
      df$occurred_date <- if ("occurredAt" %in% names(df)) safe_as_date(df$occurredAt) else as.Date(NA)
      df$created_date <- if ("createdAt" %in% names(df)) safe_as_date(df$createdAt) else as.Date(NA)
      df$delay_days <- as.numeric(df$created_date - df$occurred_date)
      
      out <- df |>
        dplyr::group_by(storedBy) |>
        dplyr::summarise(
          total_records = dplyr::n(),
          unique_events = if ("event" %in% names(df)) dplyr::n_distinct(event) else NA,
          avg_delay_days = mean(delay_days, na.rm = TRUE),
          median_delay_days = median(delay_days, na.rm = TRUE),
          .groups = "drop"
        )
      
      out$avg_delay_days[is.nan(out$avg_delay_days)] <- NA
      out$median_delay_days[is.infinite(out$median_delay_days)] <- NA
      out[order(-out$total_records), , drop = FALSE]
    })
    
    output$entries_user <- renderPlotly({
      df <- perf_df()
      
      if (nrow(df) == 0) return(empty_plotly("No user performance data available"))
      
      plotly::plot_ly(
        data = df,
        x = ~total_records,
        y = ~reorder(storedBy, total_records),
        type = "bar",
        orientation = "h"
      ) |>
        plotly::layout(
          xaxis = list(title = "Records"),
          yaxis = list(title = "User")
        )
    })
    
    output$entries_month <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"createdAt" %in% names(df)) return(empty_plotly("CreatedAt column not available"))
      
      plot_df <- data.frame(
        created_date = safe_as_date(df$createdAt)
      )
      
      plot_df <- plot_df[!is.na(plot_df$created_date), , drop = FALSE]
      if (nrow(plot_df) == 0) return(empty_plotly("No monthly performance data available"))
      
      plot_df$month <- lubridate::floor_date(plot_df$created_date, "month")
      plot_df <- dplyr::count(plot_df, month, name = "n")
      plot_df <- plot_df[order(plot_df$month), , drop = FALSE]
      
      plotly::plot_ly(
        data = plot_df,
        x = ~month,
        y = ~n,
        type = "scatter",
        mode = "lines+markers"
      ) |>
        plotly::layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Entries")
        )
    })
    
    output$performance_table <- renderDT({
      df <- perf_df()
      DT::datatable(
        df,
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