gen_data_short_term_trend <- function(){
  d <- spltidy::splfmt_rts_data_v1(data.table(
    location_code = "norge",
    date = do.call(c, spltime::dates_by_isoyearweek[isoyear==2020]$days),
    age = "total",
    sex = "total",
    border = 2020
  ))
  set.seed(4)
  d[, cases_n := rpois(.N, lambda = 1:.N)]
  return(d)
}


short_term_trend_internal <- function(x, outcome, trend_days = 14, remove_last_days = 0) {
  num_unique_ts <- spltidy::unique_time_series(x) %>%
    nrow()
  if(num_unique_ts>1){
    stop("There is more than 1 time series in this dataset")
  }
  gran_time <- x$granularity_time[1]
  if(!gran_time %in% c("day", "isoweek")){
    stop("granularity_time is not day or isoweek")
  }

  retval <- rep(NA_character_, nrow(x))
  for(i in seq_along(retval)){
    index <- (i - trend_days + 1):i
    if(i > (nrow(x) - remove_last_days)){
      next()
    } else if(min(index) < 1){
      next()
    }

    fit <- x[index]
    fit[, trend_days := 1:.N]

    model <- glm2::glm2(as.formula(glue::glue("{outcome} ~ trend_days")), data = fit, family = "quasipoisson")

    vals <- coef(summary(model))
    co <- vals["trend_days", "Estimate"]
    pval <- vals["trend_days",][[4]]
    if(pval > 0.05){
      retval[i] <- "null"
    } else if(co < 0){
      retval[i] <- "decreasing"
    } else{
      retval[i] <- "increasing"
    }
  }

  return(retval)
}

#' Determine the short term trend
#' @param x Data object
#' @param trend_days Number of days you want to check the trend
#' @param remove_last_days Number of days you want to remove at the end (due to unreliable data)
#' @export
short_term_trend <- function(x, trend_days = 14, remove_last_days = 0){
  UseMethod("short_term_trend", x)
}

#' @method short_term_trend splfmt_rts_data_v1
#' @export
short_term_trend.splfmt_rts_data_v1 <- function(x, trend_days = 14, remove_last_days = 0){
  if(!"time_series_id" %in% names(x)) on.exit(x[, time_series_id := NULL])

  num_unique_ts <- spltidy::unique_time_series(x, set_time_series_id = TRUE) %>%
    nrow()

  if(num_unique_ts > 1){
    ds <- split(x, x$time_series_id)
    retval <- lapply(ds, function(y){
      short_term_trend_internal(y, outcome = outcome, trend = trend, remove_last_days = remove_last_days)
    })
    retval <- unlist(retval, recursive = FALSE, use.names = FALSE)
  } else {
    retval <- short_term_trend_internal(x, outcome = outcome, trend = trend, remove_last_days = remove_last_days)
  }

  return(retval)
}
