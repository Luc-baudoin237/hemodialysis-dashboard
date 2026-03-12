library(dplyr)

compute_missingness <- function(df) {
  tibble(
    variable = names(df),
    missing_n = sapply(df, function(x) sum(is.na(x) | x == "")),
    missing_pct = sapply(df, function(x) round(mean(is.na(x) | x == "") * 100, 1))
  ) %>%
    arrange(desc(missing_pct))
}

find_duplicate_events <- function(df) {
  if (!"event" %in% names(df)) return(tibble())
  
  df %>%
    count(event, sort = TRUE) %>%
    filter(n > 1)
}

find_duplicate_tracked_entities <- function(df) {
  if (!"trackedEntity" %in% names(df)) return(tibble())
  
  df %>%
    count(trackedEntity, sort = TRUE) %>%
    filter(n > 1)
}

numeric_outlier_summary <- function(df) {
  num_df <- df %>% select(where(is.numeric))
  
  if (ncol(num_df) == 0) return(tibble())
  
  bind_rows(lapply(names(num_df), function(v) {
    x <- num_df[[v]]
    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    tibble(
      variable = v,
      lower_bound = lower,
      upper_bound = upper,
      outliers_n = sum(x < lower | x > upper, na.rm = TRUE)
    )
  }))
}