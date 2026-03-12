library(shiny)
library(bs4Dash)

source("global.R")

source("modules/overview.R")
source("modules/events_monitoring.R")
source("modules/tracked_entities.R")
source("modules/data_quality.R")
source("modules/performance.R")
source("modules/orgunit_monitoring.R")
source("modules/patient_explorer.R")
source("modules/data_dictionary.R")
source("modules/advanced_analytics.R")
source("modules/audit_supervision.R")

ui <- bs4DashPage(
  header = bs4DashNavbar(
    title = "DHIS2 Clinical Intelligence Platform",
    skin = "light"
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Executive Overview", tabName = "overview", icon = icon("gauge-high")),
      bs4SidebarMenuItem("Event Monitoring", tabName = "events", icon = icon("calendar-days")),
      bs4SidebarMenuItem("Tracked Entities", tabName = "entities", icon = icon("users")),
      bs4SidebarMenuItem("Data Quality", tabName = "quality", icon = icon("shield-halved")),
      bs4SidebarMenuItem("Data Entry Performance", tabName = "performance", icon = icon("user-check")),
      bs4SidebarMenuItem("Facility Monitoring", tabName = "orgunits", icon = icon("hospital")),
      bs4SidebarMenuItem("Patient Explorer", tabName = "patient", icon = icon("magnifying-glass")),
      bs4SidebarMenuItem("Variable Dictionary", tabName = "dictionary", icon = icon("book")),
      bs4SidebarMenuItem("Advanced Analytics", tabName = "analytics", icon = icon("chart-line")),
      bs4SidebarMenuItem("Audit & Supervision", tabName = "audit", icon = icon("triangle-exclamation"))
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Global Filters",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(
            width = 3,
            dateRangeInput(
              "global_date_range",
              "Date range",
              start = min_date,
              end = max_date,
              format = "yyyy-mm-dd"
            )
          ),
          column(
            width = 3,
            selectizeInput(
              "global_orgunit",
              "Org Unit",
              choices = orgunit_choices,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 2,
            selectizeInput(
              "global_stage",
              "Program Stage",
              choices = stage_choices,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 2,
            selectizeInput(
              "global_user",
              "Stored By",
              choices = user_choices,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 2,
            selectizeInput(
              "global_status",
              "Status",
              choices = status_choices,
              selected = "All",
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectizeInput(
              "global_followup",
              "Follow-up",
              choices = followup_choices,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 2,
            br(),
            actionButton("reset_filters", "Reset Filters", icon = icon("rotate-left"), class = "btn-primary")
          )
        )
      )
    ),
    
    tabItems(
      overviewUI("overview"),
      eventsUI("events"),
      entitiesUI("entities"),
      qualityUI("quality"),
      performanceUI("performance"),
      orgunitUI("orgunits"),
      patientUI("patient"),
      dictionaryUI("dictionary"),
      analyticsUI("analytics"),
      auditUI("audit")
    )
  ),
  
  title = "DHIS2 Clinical Intelligence"
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "global_date_range", start = min_date, end = max_date)
    updateSelectizeInput(session, "global_orgunit", selected = "All")
    updateSelectizeInput(session, "global_stage", selected = "All")
    updateSelectizeInput(session, "global_user", selected = "All")
    updateSelectizeInput(session, "global_status", selected = "All")
    updateSelectizeInput(session, "global_followup", selected = "All")
  })
  
  filtered_events <- reactive({
    df <- events_wide_clean
    
    if ("occurredAt" %in% names(df)) {
      df <- df %>%
        mutate(event_date = safe_as_date(occurredAt))
    } else {
      df$event_date <- as.Date(NA)
    }
    
    if (!is.null(input$global_date_range) && !all(is.na(df$event_date))) {
      df <- df %>%
        filter(
          is.na(event_date) |
            (event_date >= input$global_date_range[1] & event_date <= input$global_date_range[2])
        )
    }
    
    if (!is.null(input$global_orgunit) && !("All" %in% input$global_orgunit) && "orgUnit" %in% names(df)) {
      df <- df %>% filter(orgUnit %in% input$global_orgunit)
    }
    
    if (!is.null(input$global_stage) && !("All" %in% input$global_stage) && "programStage" %in% names(df)) {
      df <- df %>% filter(programStage %in% input$global_stage)
    }
    
    if (!is.null(input$global_user) && !("All" %in% input$global_user) && "storedBy" %in% names(df)) {
      df <- df %>% filter(storedBy %in% input$global_user)
    }
    
    if (!is.null(input$global_status) && !("All" %in% input$global_status) && "status" %in% names(df)) {
      df <- df %>% filter(status %in% input$global_status)
    }
    
    if (!is.null(input$global_followup) && !("All" %in% input$global_followup) && "followUp" %in% names(df)) {
      df <- df %>% filter(as.character(followUp) %in% input$global_followup)
    }
    
    df
  })
  
  filtered_events_long <- reactive({
    df <- events_long_labeled
    
    if ("occurredAt" %in% names(df)) {
      df <- df %>% mutate(event_date = safe_as_date(occurredAt))
    } else {
      df$event_date <- as.Date(NA)
    }
    
    if (!is.null(input$global_date_range) && !all(is.na(df$event_date))) {
      df <- df %>%
        filter(
          is.na(event_date) |
            (event_date >= input$global_date_range[1] & event_date <= input$global_date_range[2])
        )
    }
    
    if (!is.null(input$global_orgunit) && !("All" %in% input$global_orgunit) && "orgUnit" %in% names(df)) {
      df <- df %>% filter(orgUnit %in% input$global_orgunit)
    }
    
    if (!is.null(input$global_stage) && !("All" %in% input$global_stage) && "programStage" %in% names(df)) {
      df <- df %>% filter(programStage %in% input$global_stage)
    }
    
    if (!is.null(input$global_user) && !("All" %in% input$global_user) && "storedBy" %in% names(df)) {
      df <- df %>% filter(storedBy %in% input$global_user)
    }
    
    if (!is.null(input$global_status) && !("All" %in% input$global_status) && "status" %in% names(df)) {
      df <- df %>% filter(status %in% input$global_status)
    }
    
    if (!is.null(input$global_followup) && !("All" %in% input$global_followup) && "followUp" %in% names(df)) {
      df <- df %>% filter(as.character(followUp) %in% input$global_followup)
    }
    
    df
  })
  
  filtered_final_db <- reactive({
    df <- final_db
    
    if ("occurredAt" %in% names(df)) {
      df <- df %>% mutate(event_date = safe_as_date(occurredAt))
    } else {
      df$event_date <- as.Date(NA)
    }
    
    if (!is.null(input$global_date_range) && !all(is.na(df$event_date))) {
      df <- df %>%
        filter(
          is.na(event_date) |
            (event_date >= input$global_date_range[1] & event_date <= input$global_date_range[2])
        )
    }
    
    if (!is.null(input$global_orgunit) && !("All" %in% input$global_orgunit) && "orgUnit" %in% names(df)) {
      df <- df %>% filter(orgUnit %in% input$global_orgunit)
    }
    
    if (!is.null(input$global_stage) && !("All" %in% input$global_stage) && "programStage" %in% names(df)) {
      df <- df %>% filter(programStage %in% input$global_stage)
    }
    
    if (!is.null(input$global_user) && !("All" %in% input$global_user) && "storedBy" %in% names(df)) {
      df <- df %>% filter(storedBy %in% input$global_user)
    }
    
    if (!is.null(input$global_status) && !("All" %in% input$global_status) && "status" %in% names(df)) {
      df <- df %>% filter(status %in% input$global_status)
    }
    
    if (!is.null(input$global_followup) && !("All" %in% input$global_followup) && "followUp" %in% names(df)) {
      df <- df %>% filter(as.character(followUp) %in% input$global_followup)
    }
    
    df
  })
  
  overviewServer("overview", filtered_final_db)
  eventsServer("events", filtered_events_long)
  entitiesServer("entities", filtered_final_db)
  qualityServer("quality", filtered_final_db)
  performanceServer("performance", filtered_events_long)
  orgunitServer("orgunits", filtered_events_long)
  patientServer("patient", filtered_final_db)
  dictionaryServer("dictionary", dictionary, filtered_events_long)
  analyticsServer("analytics", filtered_final_db)
  auditServer("audit", filtered_final_db)
}

shinyApp(ui, server)