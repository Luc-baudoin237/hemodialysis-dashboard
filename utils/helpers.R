library(lubridate)

safe_parse_datetime <- function(x) {
  x <- as.character(x)
  
  out <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
  idx <- is.na(out)
  
  if (any(idx)) out[idx] <- suppressWarnings(lubridate::ymd_hm(x[idx], quiet = TRUE))
  idx <- is.na(out)
  
  if (any(idx)) out[idx] <- suppressWarnings(as.POSIXct(lubridate::ymd(x[idx], quiet = TRUE)))
  idx <- is.na(out)
  
  if (any(idx)) out[idx] <- suppressWarnings(as.POSIXct(x[idx], tz = "UTC"))
  
  out
}

safe_as_date <- function(x) {
  as.Date(safe_parse_datetime(x))
}

completion_rate_vector <- function(x) {
  round(mean(!(is.na(x) | x == "")) * 100, 1)
}

dataset_completion_score <- function(df, exclude_cols = c("event_date")) {
  keep <- setdiff(names(df), exclude_cols)
  if (length(keep) == 0) return(0)
  
  vals <- sapply(df[keep], completion_rate_vector)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return(0)
  
  round(mean(vals), 1)
}

quick_quality_snapshot <- function(df) {
  tibble::tibble(
    metric = c(
      "Rows",
      "Unique tracked entities",
      "Unique events",
      "Variables",
      "Overall completion score (%)",
      "Duplicate events",
      "Duplicate tracked entities"
    ),
    value = c(
      nrow(df),
      if ("trackedEntity" %in% names(df)) dplyr::n_distinct(df$trackedEntity) else NA,
      if ("event" %in% names(df)) dplyr::n_distinct(df$event) else NA,
      ncol(df),
      if (nrow(df) == 0) 0 else dataset_completion_score(df),
      if ("event" %in% names(df)) sum(duplicated(df$event)) else NA,
      if ("trackedEntity" %in% names(df)) sum(duplicated(df$trackedEntity)) else NA
    )
  )
}

empty_plotly <- function(msg = "No data available") {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = as.character(msg),
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        )
      )
    )
}