

qp_diagnostics_field_types <- c(
  "tag" = "TEXT",
  "location_code" = "TEXT",
  "granularity_time" = "TEXT",
  "age" = "TEXT",
  "year_run" = "INTEGER",
  "failed" = "INTEGER",
  "intercept" = "DOUBLE",
  "intercept_se" = "DOUBLE",
  "trend" = "DOUBLE",
  "trend_se" = "DOUBLE",
  "sin" = "DOUBLE",
  "sin_se" = "DOUBLE",
  "cos" = "DOUBLE",
  "cos_se" = "DOUBLE",
  "holiday" = "DOUBLE",
  "holiday_se" = "DOUBLE",
  "dispersion" = "DOUBLE",
  "day_of_week_factor_tue" = "DOUBLE",
  "day_of_week_factor_tue_se" = "DOUBLE",
  "day_of_week_factor_wed" = "DOUBLE",
  "day_of_week_factor_wed_se" = "DOUBLE",
  "day_of_week_factor_thu" = "DOUBLE",
  "day_of_week_factor_thu_se" = "DOUBLE",
  "day_of_week_factor_fri" = "DOUBLE",
  "day_of_week_factor_fri_se" = "DOUBLE",
  "day_of_week_factor_sat" = "DOUBLE",
  "day_of_week_factor_sat_se" = "DOUBLE",
  "day_of_week_factor_sun" = "DOUBLE",
  "day_of_week_factor_sun_se" = "DOUBLE"
)

qp_diagnostics_keys <- c(
  "tag",
  "location_code",
  "year_run",
  "granularity_time",
  "age"
)

# get_schema_qp_diagnostics_weekly
#
# DB schema for qp_diagnostics_weekly
#
get_schema_qp_diagnostics <- function() {
  return(schema$new(
    db_table = glue::glue("spuls_qp_diagnostics"),
    db_field_types = qp_diagnostics_field_types,
    db_load_folder = "/xtmp/",
    keys = qp_diagnostics_keys
  ))
}

#
# extracts diangostics data from a regression fit
#
# @param fit QP regression fit
#
extract_diagnostics <- function(fit) {
  df <- new_diagnostics_df(n_row = 1)
  sum <- summary(fit)
  dispersion <- sum$dispersion
  coefs <- coef(sum)
  df[["dispersion"]] <- dispersion
  for (i in 1:nrow(coefs)) {
    c_name <- fix_name(rownames(coefs)[i])
    if (!is.null(c_name)) {
      df[[c_name]] <- coefs[i, 1]
      df[[paste(c_name, "se", sep = "_")]] <- coefs[i, 2]
    }
  }
  df$failed <- 0
  return(df)
}

#
# new_diagnostics_df
#
# returns a data frame with the columns as needed for the diagnostics
# @param n_row determines the number of rows in the data frame.
#
new_diagnostics_df <- function(n_row = 0) {
  df <- data.frame(matrix(ncol = length(qp_diagnostics_field_types), nrow = n_row))
  colnames(df) <- names(qp_diagnostics_field_types)
  return(df)
}


fix_name <- function(name) {
  if (name %in% names(qp_diagnostics_field_types)) {
    return(name)
  } else if (name == "(Intercept)") {
    return("intercept")
  } else if (substr(name, 1, 3) == "sin") {
    return("sin")
  } else if (substr(name, 1, 3) == "cos") {
    return("cos")
  } else if (grepl("factor\\(dayOfWeek\\)", name)) {
    number <- as.numeric(strsplit(name, ")")[[1]][2])
    days <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
    return(paste("day_of_week_factor", days[number], sep = "_"))
  } else {
    #print(name)
    return(NULL)
  }
}


# update_diagnostics
#
# Updates diagnostics results with common data
#
# @param diagnostics Diagnostics results
# @param conf configuration object
# @param x analysis run
#
update_diagnostics <- function(diagnostics, argset) {
  diagnostics$tag <- argset$tag
  diagnostics$location_code <- argset$location_code
  diagnostics$year_run <- argset$year_predict_max
  diagnostics$granularity_time <- argset$granularity_time
  diagnostics$age <- argset$age
  return(diagnostics)
}

