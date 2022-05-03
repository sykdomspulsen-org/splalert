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


short_term_trend_internal <- function(
  x,
  numerator,
  denominator = NULL,
  prX = 100,
  trend_days = 42,
  remove_last_days = 0,
  forecast_days = trend_days,
  trend_isoweeks = ceiling(trend_days / 7),
  remove_last_isoweeks = ceiling(remove_last_days / 7),
  forecast_isoweeks = trend_isoweeks,
  numerator_naming_prefix = "from_numerator",
  denominator_naming_prefix = "from_denominator",
  remove_training_data = FALSE
  ){

  num_unique_ts <- spltidy::unique_time_series(x) %>%
    nrow()
  if(num_unique_ts>1){
    stop("There is more than 1 time series in this dataset")
  }
  gran_time <- x$granularity_time[1]
  if(!gran_time %in% c("day", "isoweek")){
    stop("granularity_time is not day or isoweek")
  }
  # if we have weekly data, then divide everything by 7
  if(gran_time=="isoweek"){
    if(trend_isoweeks < 6){
      stop("trend_isoweeks must be >= 6 when granularity_time is isoweek")
    }
    trend_rows <- trend_isoweeks
    remove_last_rows <- remove_last_isoweeks
    forecast_rows <- forecast_isoweeks

    trend_days <- trend_isoweeks * 7

    with_pred <- spltidy::expand_time_to(x, max_isoyearweek = spltime::date_to_isoyearweek_c(max(x$date)+forecast_isoweeks*7))
  } else {
    if(trend_days < 14){
      stop("trend_days must be >= 14 days when granularity_time is day")
    }
    trend_rows <- trend_days
    remove_last_rows <- remove_last_days
    forecast_rows <- forecast_days

    with_pred <- spltidy::expand_time_to(x, max_date = max(x$date)+forecast_days)
  }

  suffix <- stringr::str_extract(numerator, "_[a-z]+$")
  if(numerator_naming_prefix=="from_numerator"){
    prefix <- stringr::str_remove(numerator, "_[a-z]+$")
  } else if(numerator_naming_prefix=="generic") {
    prefix <- "value"
  } else {
    prefix <- numerator_naming_prefix
  }

  if(denominator_naming_prefix=="from_denominator"){
    prefix_denom <- stringr::str_remove(denominator, "_[a-z]+$")
  } else if(denominator_naming_prefix=="generic") {
    prefix_denom <- "denom"
  } else {
    prefix_denom <- denominator_naming_prefix
  }

  varname_forecast_numerator <- paste0(prefix, "_forecasted", suffix)
  varname_forecast_predinterval_q02x5_numerator <- paste0(prefix, "_forecasted_predinterval_q02x5", suffix)
  varname_forecast_predinterval_q97x5_numerator <- paste0(prefix, "_forecasted_predinterval_q97x5", suffix)
  varname_forecast_numerator <- paste0(prefix, "_forecasted", suffix)
  if(!is.null(denominator)){
    varname_forecast_denominator <- paste0(prefix_denom, "_forecasted", suffix)

    varname_forecast_prX <- paste0(prefix, "_forecasted_pr", prX)
    varname_forecast_prX_is_forecast <- paste0(prefix, "_forecasted_pr", prX,"_forecast")
    varname_forecast_predinterval_q02x5_prX <- paste0(prefix, "_forecasted_predinterval_q02x5_pr", prX)
    varname_forecast_predinterval_q97x5_prX <- paste0(prefix, "_forecasted_predinterval_q97x5_pr", prX)

    varname_trend <- paste0(prefix, "_trend0_",trend_days, "_pr", prX, "_status")
    varname_days_to_double <- paste0(prefix, "_doublingdays0_",trend_days, "_pr", prX)

    varname_forecast <- paste0(varname_forecast_prX, "_forecast")
  } else {
    varname_trend <- paste0(prefix, "_trend0_",trend_days, suffix, "_status")
    varname_days_to_double <- paste0(prefix, "_doublingdays0_",trend_days, suffix)

    varname_forecast <- paste0(varname_forecast_numerator, "_forecast")
  }


  with_pred[, to_be_forecasted := FALSE]
  with_pred[(.N-remove_last_rows-forecast_rows+1):.N, to_be_forecasted := TRUE]

  with_pred[ , (varname_forecast_numerator) := get(numerator)]
  if(!is.null(denominator)) with_pred[, (varname_forecast_denominator) := get(denominator)]
  with_pred[, trend_variable := 1:.N / .N]

  doubling_time <- rep(NA_real_, nrow(with_pred))
  trend <- rep(NA_character_, nrow(with_pred))

  #if(remove_last_days > 0) indexes <- indexes[-c(1:remove_last_days)]
  #indexes <- indexes[which(spltime::keep_sundays_and_latest_date(x$date[indexes]) != "delete")]
  for(i in seq_len(nrow(x)-remove_last_rows)){
    index <- (i - trend_rows + 1):i
    if(min(index) < 1){
      next()
    }

    training_data <- with_pred[index]

    formula <- glue::glue("{varname_forecast_numerator} ~ trend_variable")

    model_denominator <- NULL
    if(!is.null(denominator)){
      # if denominator is zero, replace with 1
      training_data[get(varname_forecast_denominator)==0, (varname_forecast_denominator) := 1]

      formula_denominator <- glue::glue("{varname_forecast_denominator} ~ trend_variable")
      tryCatch({
        model_denominator <- glm2::glm2(as.formula(formula_denominator), data = training_data, family = stats::quasipoisson(link = "log"))
      },
      error = function(e){
        model_denominator <- NULL
      })
      formula <- glue::glue("{formula} + offset(log({varname_forecast_denominator}))")
    }

    model <- NULL
    tryCatch({
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
      if(gran_time=="isoweek"){
        doubling_time[i] <- doubling_time[i]*7 # remember to scale it so that it is per day!!
      }
    },error = function(e){
      warning("Error in fitting model")
    }
    )
  }
  trend <- factor(trend, levels = c("decreasing", "null", "increasing"))
  if(is.null(model) | (!is.null(denominator) & is.null(model_denominator))){
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_denominator) := NA_real_])
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_numerator) := NA_real_])
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q02x5_numerator) := NA_real_])
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q97x5_numerator) := NA_real_])

    if(!is.null(denominator)){
      with_pred[, (varname_forecast_prX) := NA_real_]
      with_pred[, (varname_forecast_predinterval_q02x5_prX) := NA_real_]
      with_pred[, (varname_forecast_predinterval_q97x5_prX) := NA_real_]

      suppressWarnings(with_pred[, (varname_forecast_predinterval_q02x5_numerator) := NULL])
      suppressWarnings(with_pred[, (varname_forecast_predinterval_q97x5_numerator) := NULL])
    }
  } else {
    if(!is.null(denominator)){
      forecasted_denominator <- prediction_interval(model_denominator, with_pred[to_be_forecasted==TRUE], alpha = 0.05)
      suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_denominator) := round(forecasted_denominator$point)])
      # if denominator is zero, replace with 1
      with_pred[to_be_forecasted==TRUE & get(varname_forecast_denominator)==0, (varname_forecast_denominator) := 1]
    }

    forecasted <- prediction_interval(model, with_pred[to_be_forecasted==TRUE], alpha = 0.05)
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_numerator) := round(forecasted$point)])
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q02x5_numerator) := round(forecasted$lower)])
    suppressWarnings(with_pred[to_be_forecasted==TRUE, (varname_forecast_predinterval_q97x5_numerator) := round(forecasted$upper)])

    if(!is.null(denominator)){
      # if numerator is predicted to be bigger than denominator, set numerator to denominator
      # todo: this probably should be fixed
      with_pred[get(varname_forecast_numerator) > get(varname_forecast_denominator), (varname_forecast_numerator) := get(varname_forecast_denominator)]

      with_pred[, (varname_forecast_prX) := prX * get(varname_forecast_numerator) / get(varname_forecast_denominator)]
      with_pred[is.nan(get(varname_forecast_prX)), (varname_forecast_prX) := 0]

      with_pred[, (varname_forecast_predinterval_q02x5_prX) := prX * get(varname_forecast_predinterval_q02x5_numerator) / get(varname_forecast_denominator)]
      with_pred[is.nan(get(varname_forecast_predinterval_q02x5_prX)), (varname_forecast_predinterval_q02x5_prX) := 0]

      with_pred[, (varname_forecast_predinterval_q97x5_prX) := prX * get(varname_forecast_predinterval_q97x5_numerator) / get(varname_forecast_denominator)]
      with_pred[is.nan(get(varname_forecast_predinterval_q97x5_prX)), (varname_forecast_predinterval_q97x5_prX) := 0]

      suppressWarnings(with_pred[, (varname_forecast_predinterval_q02x5_numerator) := NULL])
      suppressWarnings(with_pred[, (varname_forecast_predinterval_q97x5_numerator) := NULL])
    }
  }

  with_pred[, trend_variable := NULL]
  setnames(with_pred, "to_be_forecasted", varname_forecast)

  with_pred[, (varname_trend) := trend]
  with_pred[, (varname_days_to_double) := round(doubling_time,1)]

  if(remove_training_data) with_pred <- with_pred[-(1:(trend_rows-1))]

  return(with_pred)
}

#' Determine the short term trend
#' @param x Data object
#' @param numerator Character of name of numerator
#' @param denominator Character of name of denominator (optional)
#' @param prX If using denominator, what scaling factor should be used for numerator/denominator?
#' @param trend_days Number of days you want to check the trend
#' @param remove_last_days Number of days you want to remove at the end (due to unreliable data)
#' @param forecast_days Number of days you want to forecast into the future
#' @param trend_isoweeks Same as trend_days, but used if granularity_geo=='isoweek'
#' @param remove_last_isoweeks Same as remove_last_days, but used if granularity_geo=='isoweek'
#' @param forecast_isoweeks Same as forecast_days, but used if granularity_geo=='isoweek'
#' @param numerator_naming_prefix "from_numerator", "generic", or a custom prefix
#' @param denominator_naming_prefix "from_denominator", "generic", or a custom prefix
#' @param remove_training_data Boolean. If TRUE, removes the training data (i.e. 1:(trend_days-1) or 1:(trend_isoweeks-1)) from the returned dataset.
#' @export
short_term_trend <- function(
  x,
  numerator,
  denominator = NULL,
  prX = 100,
  trend_days = 42,
  remove_last_days = 0,
  forecast_days = trend_days,
  trend_isoweeks = ceiling(trend_days / 7),
  remove_last_isoweeks = ceiling(remove_last_days / 7),
  forecast_isoweeks = trend_isoweeks,
  numerator_naming_prefix = "from_numerator",
  denominator_naming_prefix = "from_denominator",
  remove_training_data = FALSE
){
  UseMethod("short_term_trend", x)
}

#' @method short_term_trend splfmt_rts_data_v1
#' @export
short_term_trend.splfmt_rts_data_v1 <- function(
  x,
  numerator,
  denominator = NULL,
  prX = 100,
  trend_days = 42,
  remove_last_days = 0,
  forecast_days = trend_days,
  trend_isoweeks = ceiling(trend_days / 7),
  remove_last_isoweeks = ceiling(remove_last_days / 7),
  forecast_isoweeks = trend_isoweeks,
  numerator_naming_prefix = "from_numerator",
  denominator_naming_prefix = "from_denominator",
  remove_training_data = FALSE
  ){
  if(!"time_series_id" %in% names(x)) on.exit({
    x[, time_series_id := NULL]
    if("time_series_id" %in% names(retval)) retval[, time_series_id := NULL]
  })

  num_unique_ts <- spltidy::unique_time_series(x, set_time_series_id = TRUE) %>%
    nrow()

  if(num_unique_ts > 1){
    ds <- split(x, x$time_series_id)
    retval <- lapply(ds, function(y){
      short_term_trend_internal(
        y,
        numerator = numerator,
        denominator = denominator,
        prX = prX,
        trend_days = trend_days,
        remove_last_days = remove_last_days,
        forecast_days = forecast_days,
        trend_isoweeks = trend_isoweeks,
        remove_last_isoweeks = remove_last_isoweeks,
        forecast_isoweeks = forecast_isoweeks,
        numerator_naming_prefix = numerator_naming_prefix,
        denominator_naming_prefix = denominator_naming_prefix,
        remove_training_data = remove_training_data
      )
    })
    retval <- rbindlist(retval) #unlist(retval, recursive = FALSE, use.names = FALSE)
  } else {
    retval <- short_term_trend_internal(
      x,
      numerator = numerator,
      denominator = denominator,
      prX = prX,
      trend_days = trend_days,
      remove_last_days = remove_last_days,
      forecast_days = forecast_days,
      trend_isoweeks = trend_isoweeks,
      remove_last_isoweeks = remove_last_isoweeks,
      forecast_isoweeks = forecast_isoweeks,
      numerator_naming_prefix = numerator_naming_prefix,
      denominator_naming_prefix = denominator_naming_prefix,
      remove_training_data = remove_training_data
    )
  }

  spltidy::set_splfmt_rts_data_v1(retval)

  data.table::shouldPrint(retval)

  return(retval)
}
