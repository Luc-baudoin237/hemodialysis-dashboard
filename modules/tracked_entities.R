entitiesUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "entities",
    
    fluidRow(
      box(
        width = 6,
        title = "Visits per Tracked Entity",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("visits_distribution"), height = 350)
      ),
      box(
        width = 6,
        title = "Top Patients by Number of Events",
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("top_patients"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Tracked Entity Summary",
        status = "success",
        solidHeader = TRUE,
        DTOutput(ns("entity_summary"))
      )
    )
  )
}

entitiesServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    entity_counts <- reactive({
      df <- data()
      if (!is.data.frame(df) || !"trackedEntity" %in% names(df)) {
        return(data.frame())
      }
      
      df <- df[!(is.na(df$trackedEntity) | df$trackedEntity == ""), , drop = FALSE]
      if (nrow(df) == 0) return(data.frame())
      
      out <- dplyr::count(df, trackedEntity, name = "n_events")
      out <- out[order(-out$n_events), , drop = FALSE]
      out
    })
    
    output$visits_distribution <- renderPlotly({
      df <- entity_counts()
      
      if (nrow(df) == 0) {
        return(empty_plotly("No tracked entity data available for the selected filters"))
      }
      
      plotly::plot_ly(
        data = df,
        x = ~n_events,
        type = "histogram"
      ) |>
        plotly::layout(
          xaxis = list(title = "Number of Events per Patient"),
          yaxis = list(title = "Count of Patients")
        )
    })
    
    output$top_patients <- renderDT({
      df <- entity_counts()
      DT::datatable(
        head(df, 20),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$entity_summary <- renderDT({
      counts <- entity_counts()
      df <- data()
      
      summary_df <- tibble::tibble(
        metric = c(
          "Unique tracked entities",
          "Tracked entities with repeated visits",
          "Maximum visits for one patient",
          "Median visits per patient"
        ),
        value = c(
          if ("trackedEntity" %in% names(df)) dplyr::n_distinct(df$trackedEntity) else 0,
          if (nrow(counts) > 0) sum(counts$n_events > 1) else 0,
          if (nrow(counts) > 0) max(counts$n_events, na.rm = TRUE) else 0,
          if (nrow(counts) > 0) stats::median(counts$n_events, na.rm = TRUE) else 0
        )
      )
      
      DT::datatable(
        summary_df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)
      )
    })
  })
}