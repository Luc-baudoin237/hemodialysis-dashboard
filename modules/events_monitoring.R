eventsUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "events",
    
    fluidRow(
      box(
        width = 8,
        title = "Events by Date",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("events_by_date"), height = 380)
      ),
      box(
        width = 4,
        title = "Events by Status",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("status_plot"), height = 380)
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Events by Program Stage",
        status = "success",
        solidHeader = TRUE,
        plotlyOutput(ns("stage_plot"), height = 320)
      ),
      box(
        width = 6,
        title = "Events by User",
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput(ns("user_plot"), height = 320)
      )
    )
  )
}

eventsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$events_by_date <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"occurredAt" %in% names(df)) {
        return(empty_plotly("OccurredAt column not available"))
      }
      
      plot_df <- data.frame(
        event_date = safe_as_date(df$occurredAt)
      )
      
      plot_df <- plot_df[!is.na(plot_df$event_date), , drop = FALSE]
      if (nrow(plot_df) == 0) {
        return(empty_plotly("No event dates available for the selected filters"))
      }
      
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
    
    output$status_plot <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"status" %in% names(df)) {
        return(empty_plotly("Status column not available"))
      }
      
      plot_df <- data.frame(
        status = ifelse(is.na(df$status) | df$status == "", "Unknown", as.character(df$status))
      )
      
      plot_df <- dplyr::count(plot_df, status, name = "n")
      if (nrow(plot_df) == 0) {
        return(empty_plotly("No status data available"))
      }
      
      plotly::plot_ly(
        data = plot_df,
        x = ~status,
        y = ~n,
        type = "bar"
      ) |>
        plotly::layout(
          xaxis = list(title = "Status"),
          yaxis = list(title = "Events")
        )
    })
    
    output$stage_plot <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"programStage" %in% names(df)) {
        return(empty_plotly("ProgramStage column not available"))
      }
      
      plot_df <- data.frame(
        programStage = ifelse(
          is.na(df$programStage) | df$programStage == "",
          "Unknown",
          as.character(df$programStage)
        )
      )
      
      plot_df <- dplyr::count(plot_df, programStage, name = "n")
      plot_df <- plot_df[order(-plot_df$n), , drop = FALSE]
      plot_df <- head(plot_df, 15)
      
      if (nrow(plot_df) == 0) {
        return(empty_plotly("No program stage data available"))
      }
      
      plotly::plot_ly(
        data = plot_df,
        x = ~n,
        y = ~reorder(programStage, n),
        type = "bar",
        orientation = "h"
      ) |>
        plotly::layout(
          xaxis = list(title = "Events"),
          yaxis = list(title = "Program Stage")
        )
    })
    
    output$user_plot <- renderPlotly({
      df <- data()
      if (!is.data.frame(df) || !"storedBy" %in% names(df)) {
        return(empty_plotly("StoredBy column not available"))
      }
      
      plot_df <- data.frame(
        storedBy = ifelse(
          is.na(df$storedBy) | df$storedBy == "",
          "Unknown",
          as.character(df$storedBy)
        )
      )
      
      plot_df <- dplyr::count(plot_df, storedBy, name = "n")
      plot_df <- plot_df[order(-plot_df$n), , drop = FALSE]
      plot_df <- head(plot_df, 15)
      
      if (nrow(plot_df) == 0) {
        return(empty_plotly("No user data available"))
      }
      
      plotly::plot_ly(
        data = plot_df,
        x = ~n,
        y = ~reorder(storedBy, n),
        type = "bar",
        orientation = "h"
      ) |>
        plotly::layout(
          xaxis = list(title = "Events"),
          yaxis = list(title = "User")
        )
    })
  })
}