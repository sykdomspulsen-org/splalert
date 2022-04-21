gen_data_short_term_trend <- function(){
  d <- spltidy::splfmt_rts_data_v1(data.table(
    location_code = "norge",
    date = do.call(c, spltime::dates_by_isoyearweek[isoyear==2020]$days),
    age = "total",
    sex = "total",
    border = 2020
  ))
  set.seed(4)
  d[, cases_n := rpois(.N, lambda = seasonweek*4)]
  return(d)
}


short_term_trend_internal <- function(x, numerator, denominator = NULL, trend_days = 14, remove_last_days = 0, forecast_days = trend_days) {
  num_unique_ts <- spltidy::unique_time_series(x) %>%
    nrow()
  if(num_unique_ts>1){
    stop("There is more than 1 time series in this dataset")
  }
  gran_time <- x$granularity_time[1]
  if(!gran_time %in% c("day", "isoweek")){
    stop("granularity_time is not day or isoweek")
  }

  with_pred <- spltidy::expand_time_to(x, max_date = max(x$date)+forecast_days)

  suffix <- stringr::str_extract(numerator, "_[a-z]+$")
  varname_forecast_numerator <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_forecasted", suffix)
  varname_forecast_predinterval_q02x5_numerator <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_forecasted_predinterval_q02x5", suffix)
  varname_forecast_predinterval_q97x5_numerator <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_forecasted_predinterval_q97x5", suffix)
  varname_forecast_numerator <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_forecasted", suffix)
  # varname_forecast_denominator <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_forecast_", suffix)
  varname_trend <- paste0(stringr::str_remove(numerator, "_[a-z]+$"), "_trend0_",trend_days, suffix, "_status")
  varname_days_to_double <- paste0(stringr::str_remove(numerator, "_[a-z]$"), "_doublingdays0_",trend_days, suffix)

  with_pred[, to_be_forecasted := FALSE]
  with_pred[(.N-remove_last_days-forecast_days+1):.N, to_be_forecasted := TRUE]

  with_pred[to_be_forecasted == TRUE, (varname_forecast_numerator) := get(numerator)]
  with_pred[, trend_variable := 1:.N / .N]

  doubling_time <- rep(NA_real_, nrow(with_pred))
  trend <- rep(NA_character_, nrow(with_pred))

  #if(remove_last_days > 0) indexes <- indexes[-c(1:remove_last_days)]
  #indexes <- indexes[which(spltime::keep_sundays_and_latest_date(x$date[indexes]) != "delete")]
  for(i in seq_len(nrow(x)-remove_last_days)){
    index <- (i - trend_days + 1):i
    if(min(index) < 1){
      next()
    }

    training_data <- with_pred[index]

    formula <- glue::glue("{numerator} ~ trend_variable")
    if(!is.null(denominator)) formula <- glue::glue("{formula} + offset(log({denominator}))")
    model <- glm2::glm2(as.formula(formula), data = training_data, family = stats::quasipoisson(link = "log"))

    vals <- coef(summary(model))
    co <- vals["trend_variable", "Estimate"]
    pval <- vals["trend_variable",][[4]]
    if(pval > 0.05){
      trend[i] <- "null"
    } else {
      if(co < 0){
        trend[i] <- "decreasing"
      } else{
        trend[i] <- "increasing"
      }
    }
    doubling_time[i] <- nrow(with_pred)*log(2)/co # remember to scale it so that it is per day!!
  }
  trend <- factor(trend, levels = c("decreasing", "null", "increasing"))
  forecasted <- prediction_interval(model, with_pred[to_be_forecasted==TRUE], alpha = 0.05)

  suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_numerator) := forecasted$point])
  suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q02x5_numerator) := forecasted$lower])
  suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q97x5_numerator) := forecasted$upper])

  with_pred[, trend_variable := NULL]
  with_pred[, to_be_forecasted := NULL]

  with_pred[, (varname_trend) := trend]
  with_pred[, (varname_days_to_double) := doubling_time]

  return(with_pred)
}

#' Determine the short term trend
#' @param x Data object
#' @param numerator Character of name of numerator
#' @param denominator Character of name of denominator (optional)
#' @param trend_days Number of days you want to check the trend
#' @param remove_last_days Number of days you want to remove at the end (due to unreliable data)
#' @param forecast_days Number of days you want to forecast into the future
#' @export
short_term_trend <- function(x, numerator, denominator = NULL, trend_days = 14, remove_last_days = 0, forecast_days = trend_days){
  UseMethod("short_term_trend", x)
}

#' @method short_term_trend splfmt_rts_data_v1
#' @export
short_term_trend.splfmt_rts_data_v1 <- function(x, numerator, denominator = NULL, trend_days = 14, remove_last_days = 0, forecast_days = trend_days){
  if(!"time_series_id" %in% names(x)) on.exit({
    x[, time_series_id := NULL]
    retval[, time_series_id := NULL]
  })

  num_unique_ts <- spltidy::unique_time_series(x, set_time_series_id = TRUE) %>%
    nrow()

  if(num_unique_ts > 1){
    ds <- split(x, x$time_series_id)
    retval <- lapply(ds, function(y){
      short_term_trend_internal(y, numerator = numerator, denominator = denominator, trend_days = trend_days, remove_last_days = remove_last_days, forecast_days = forecast_days)
    })
    retval <- rbindlist(retval) #unlist(retval, recursive = FALSE, use.names = FALSE)
  } else {
    retval <- short_term_trend_internal(x, numerator = numerator, denominator = denominator, trend_days = trend_days, remove_last_days = remove_last_days, forecast_days = forecast_days)
  }

  spltidy::set_splfmt_rts_data_v1(retval)

  data.table::shouldPrint(retval)

  return(retval)
}
