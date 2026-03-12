dictionaryUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "dictionary",
    
    fluidRow(
      box(
        width = 12,
        title = "Variable Dictionary and Completion",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("dictionary_table"))
      )
    )
  )
}

dictionaryServer <- function(id, data, events_long_data) {
  moduleServer(id, function(input, output, session) {
    
    output$dictionary_table <- renderDT({
      dict <- data
      ev <- events_long_data()
      
      completion_df <- ev %>%
        mutate(shortName = ifelse(is.na(shortName) | shortName == "", paste0("de_", dataElement), shortName)) %>%
        group_by(shortName, dataElement, name, code, valueType) %>%
        summarise(
          n_records = n(),
          non_missing = sum(!(is.na(value) | value == "")),
          completion_rate = round(100 * non_missing / n_records, 1),
          .groups = "drop"
        )
      
      datatable(
        completion_df,
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