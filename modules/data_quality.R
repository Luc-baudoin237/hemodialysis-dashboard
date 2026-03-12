qualityUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "quality",
    
    fluidRow(
      box(
        width = 6,
        title = "Variable Missingness",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("missing_plot"), height = 350)
      ),
      box(
        width = 6,
        title = "Completeness Summary",
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("missing_table"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Top Variables with Highest Missingness",
        status = "warning",
        solidHeader = TRUE,
        DTOutput(ns("top_missing_table"))
      )
    )
  )
}

qualityServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    missing_df <- reactive({
      df <- data()
      req(is.data.frame(df))
      
      tibble(
        variable = names(df),
        missing_n = sapply(df, function(x) sum(is.na(x) | x == "")),
        missing_pct = round(sapply(df, function(x) mean(is.na(x) | x == "") * 100), 1),
        completion_pct = round(100 - missing_pct, 1)
      ) %>%
        arrange(desc(missing_pct))
    })
    
    output$missing_plot <- renderPlotly({
      df <- missing_df() %>% slice_head(n = 20)
      req(nrow(df) > 0)
      
      p <- ggplot(df, aes(x = reorder(variable, missing_pct), y = missing_pct)) +
        geom_col() +
        coord_flip() +
        labs(x = "Variable", y = "Missingness (%)")
      
      ggplotly(p)
    })
    
    output$missing_table <- renderDT({
      summary_df <- tibble(
        metric = c(
          "Number of rows",
          "Number of columns",
          "Overall completion score (%)"
        ),
        value = c(
          nrow(data()),
          ncol(data()),
          dataset_completion_score(data())
        )
      )
      
      datatable(
        summary_df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)
      )
    })
    
    output$top_missing_table <- renderDT({
      datatable(
        missing_df(),
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })
  })
}