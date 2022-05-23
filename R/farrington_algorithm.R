#' Farrington algorithm
#'
#'
#' Description:
#'
#' Input (arguments):
#' dataset: data frame with columns for number of cases, covariates and dates per day
#' datecol: name of date-column (default: 'Dato')
#' predinterval: length of prediction interval (default: 30; last 30 days)
#' historical.data.years: number (should be greater or equal to at least 3) of full years of background data (default: 5)
#' mod.pred.window: number (greater or equal to 0) of days window between datasets used for modelling and prediction(default: 90)
#' reweights: number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
#' remove.pandemic.year: true/false (default: false; keep 2009 data)
#' remove.highcounts: number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
#' sign.level: significance level for the prediction intervals (default: 5%)
#'
#' @param d_train Training data.table
#' @param datasetPredict Prediction data.table
#' @param reweights Number (greater or equal to 0) of residual reweights adjusting for previous outbreaks (default: 1; 1 reweight)
#' @param remove.highcounts Number between 0 and 1 of fraction of high counts to be removed from prediction, to remove impact of earlier outbreaks (default: 0)
#' @param sign.level Significance level for the prediction intervals (default: 0.05)
#' @param isDaily Is it daily data or weekly data?
#' @param v Version (Not in use)
#' @param weeklyDenominatorFunction sum or mean - should the denominator be summed or meaned over time
#' @param uuid uuid
#' @importFrom glm2 glm2
#' @import stats
#' @import data.table
#' @export
farrington_algorithm <- function(data,
                                 reweights = 1,
                                 remove.highcounts = 0,
                                 is_day = T) {



  d <- create_train_prediction_data(data)
  d_train <- d$d_train
  d_prediction <- d$d_prediction


  # SET REGRESSION FORMULA:
  if (is_day) {
    # regformula <- n ~ offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)
    regformula <- n_tot ~  isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday + factor(wday)

  } else {
    # regformula <- n ~ offset(log(denom_n)) + isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday
    regformula <- n_tot ~ isoyear + sin(2 * pi * (isoweek - 1) / 52) + cos(2 * pi * (isoweek - 1) / 52) + holiday

  }

  # If chosen, remove upper given percentage of counts from the prediction:
  if (remove.highcounts > 0) {
    d_train <- d_train[n_tot < quantile(n_tot, (1 - remove.highcounts)), ]
  }

  # FIT QUASI-POISSON REGRESSION MODEL ON THE TRAINING SET:
  normalFunction <- function(regformula, d_train) {
    fit <- glm2::glm2(regformula, data = d_train, family = quasipoisson, na.action = na.omit)
    return(list(fit = fit, failed = !fit$converged))
  }
  exceptionalFunction <- function(err) {
    return(list(fit = NaN, failed = TRUE))
  }
  poisreg <- tryCatch(normalFunction(regformula, d_train), error = exceptionalFunction, warning = exceptionalFunction)
  if (poisreg$failed) {
    d_prediction[, baseline_n_expected := 0.0]
    d_prediction[, baseline_n_predinterval_l4 := 0.0]
    d_prediction[, baseline_n_predinterval_l2 := 0.0]
    d_prediction[, baseline_n_predinterval_u2 := 5.0]
    d_prediction[, baseline_n_predinterval_u4 := 10.0]
    d_prediction[, zscore := 0.0]
    d_prediction[, status := "normal"]
    d_prediction[, failed := TRUE]
  } else {
    # REFIT THE REGRESSION USING RESIDUAL WEIGHTS (TO DOWNWEIGHT PREVIOUS OUTBREAKS):
    d_train[, w_i := 1]
    for (i in sort(1:reweights)) {
      dispersion_parameter <- summary(poisreg$fit)$dispersion
      if (i == 0) {
        break
      }
      try(
        {
          anscombe.res <- anscombe.residuals(poisreg$fit, dispersion_parameter)
          anscombe.res[anscombe.res < 1] <- 1 # Alt. 2.58?
          d_train[, w_i := anscombe.res^(-2)] # The weight
          Gamma <- nrow(d_train) / sum(d_train$w_i)
          d_train[, w_i := Gamma * w_i] # Makes sum(w_i) = n
          poisreg$fit <- glm2::glm2(regformula, data = d_train, weights = w_i, family = quasipoisson, na.action = na.omit)
          dispersion_parameter <- summary(poisreg$fit)$dispersion
          regression_diagnostics <- extract_diagnostics(poisreg$fit)
          od <- max(1, sum(poisreg$fit$weights * poisreg$fit$residuals^2) / poisreg$fit$df.r)
        },
        TRUE
      )
    }
    # CALCULATE SIGNAL THRESHOLD (prediction interval from Farrington 1996):
    pred <- tryCatch(
      list(
        vals = predict(poisreg$fit, type = "response", se.fit = T, newdata = d_prediction),
        failed = FALSE
      ),
      error = function(err) {
        list(
          vals = NULL,
          failed = TRUE
        )
      }
    )
    if (!pred$failed) if (max(pred$vals$fit) > 1e10) pred$failed <- TRUE

    if (pred$failed) {
      d_prediction[, baseline_n_expected := 0.0]
      d_prediction[, baseline_n_predinterval_l4 := 0.0]
      d_prediction[, baseline_n_predinterval_l2 := 0.0]
      d_prediction[, baseline_n_predinterval_u2 := 5.0]
      d_prediction[, baseline_n_predinterval_u4 := 10.0]
      d_prediction[, zscore := 0.0]
      d_prediction[, status := "normal"]
      d_prediction[, failed := TRUE]
    } else {
      d_prediction[, baseline_n_expected := pred$vals$fit]
      d_prediction[, baseline_n_predinterval_l4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -4, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_l2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = -2, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_u2 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 2, skewness.transform = "2/3")]
      d_prediction[, baseline_n_predinterval_u4 := FarringtonThreshold(pred$vals, phi = dispersion_parameter, z = 4, skewness.transform = "2/3")]
      d_prediction[, zscore := FarringtonZscore(pred$vals, phi = dispersion_parameter, z = 6, skewness.transform = "2/3", y = n_tot)]
      d_prediction[, status := "normal"]
      d_prediction[n_tot > baseline_n_predinterval_u2, status := "medium"]
      d_prediction[n_tot > baseline_n_predinterval_u4, status := "high"]
      d_prediction[, failed := FALSE]
    }
  }

  return(d_prediction)
}



#' Calculate Farrington SE in gamma space
#' @param pred Point estimate
#' @param phi Dispersion
#' @param alpha Not used
#' @param z Not used
#' @param skewness.transform "none"/"1/2","2/3"
#' @export farrington_se_gamma_space
farrington_se_gamma_space <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none") {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform,
         none = {
           se <- sqrt(mu0 * tau)
           exponent <- 1
         },
         `1/2` = {
           se <- sqrt(1 / 4 * tau)
           exponent <- 1 / 2
         },
         `2/3` = {
           se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
           exponent <- 2 / 3
         },
         {
           stop("No proper exponent in algo.farrington.threshold.")
         }
  )

  return(se)
}

# Calculate Farrington threshold
# @param pred Point estimate
# @param phi Dispersion
# @param alpha Alpha (e.g 0.05)
# @param z Similar to \code{alpha} (e.g. 1.96)
# @param skewness.transform "none"/"1/2","2/3"
farrington_threshold <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none") {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform,
         none = {
           se <- sqrt(mu0 * tau)
           exponent <- 1
         },
         `1/2` = {
           se <- sqrt(1 / 4 * tau)
           exponent <- 1 / 2
         },
         `2/3` = {
           se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
           exponent <- 2 / 3
         },
         {
           stop("No proper exponent in algo.farrington.threshold.")
         }
  )
  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  lu <- (mu0^exponent + z *
           se)^(1 / exponent)

  return(lu)
}

# Farrington Z score
# @param pred Point estimate
# @param phi Dispersion
# @param alpha Alpha (e.g 0.05)
# @param z Similar to \code{alpha} (e.g. 1.96)
# @param skewness.transform "none"/"1/2","2/3"
# @param y Observation
farrington_zscore <- function(pred, phi, alpha = NULL, z = NULL, skewness.transform = "none", y) {
  mu0 <- pred$fit
  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness.transform,
         none = {
           se <- sqrt(mu0 * tau)
           exponent <- 1
         },
         `1/2` = {
           se <- sqrt(1 / 4 * tau)
           exponent <- 1 / 2
         },
         `2/3` = {
           se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
           exponent <- 2 / 3
         },
         {
           stop("No proper exponent in algo.farrington.threshold.")
         }
  )
  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  zscore <- (y^exponent - mu0^exponent) / se

  return(zscore)
}




#' Farrington thresholds
#' @param object Object
#' @param newdata New data
#' @param alpha Two-sided alpha (e.g 0.05)
#' @param z Similar to \code{alpha} (e.g. z=1.96 is the same as alpha=0.05)
#' @param skewness_transform "none", "1/2", "2/3"
#' @param ... dots
#' @method prediction_interval glm
#' @export
prediction_interval.glm <- function(object, newdata, alpha = 0.05, z = NULL, skewness_transform = "none", ...){
  stopifnot(object$family$family %in% c("poisson", "quasipoisson"))
  stopifnot(skewness_transform %in% c("none", "1/2", "2/3"))


  pred <- predict(object, newdata, type = "response", se.fit = T)

  mu0 <- pred$fit
  phi <- summary(object)$dispersion

  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness_transform, none = {
    se <- sqrt(mu0 * tau)
    exponent <- 1
  }, `1/2` = {
    se <- sqrt(1 / 4 * tau)
    exponent <- 1 / 2
  }, `2/3` = {
    se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
    exponent <- 2 / 3
  }, {
    stop("No proper exponent in prediction_interval.glm")
  })

  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  lower <- (mu0^exponent - z * se)^(1 / exponent)
  upper <- (mu0^exponent + z * se)^(1 / exponent)

  return(data.table(
    lower = lower,
    point = mu0,
    upper = upper
  ))
}




#' Farrington Flexible
#' @param object Object
#' @param newdata New data
#' @param alpha Two-sided alpha (e.g 0.05)
#' @param z Similar to \code{alpha} (e.g. z=1.96 is the same as alpha=0.05)
#' @param skewness_transform "none", "1/2", "2/3"
#' @param ... dots
#' @method prediction_interval glm
#' @export
prediction_interval.glm <- function(object, newdata, alpha = 0.05, z = NULL, skewness_transform = "none", ...){
  stopifnot(object$family$family %in% c("poisson", "quasipoisson"))
  stopifnot(skewness_transform %in% c("none", "1/2", "2/3"))


  pred <- predict(object, newdata, type = "response", se.fit = T)

  mu0 <- pred$fit
  phi <- summary(object)$dispersion

  tau <- phi + (pred$se.fit^2) / mu0
  switch(skewness_transform, none = {
    se <- sqrt(mu0 * tau)
    exponent <- 1
  }, `1/2` = {
    se <- sqrt(1 / 4 * tau)
    exponent <- 1 / 2
  }, `2/3` = {
    se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
    exponent <- 2 / 3
  }, {
    stop("No proper exponent in prediction_interval.glm")
  })

  if (is.null(z)) z <- qnorm(1 - alpha / 2)
  lower <- (mu0^exponent - z * se)^(1 / exponent)
  upper <- (mu0^exponent + z * se)^(1 / exponent)

  return(data.table(
    lower = lower,
    point = mu0,
    upper = upper
  ))
}



