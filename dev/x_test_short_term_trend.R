# test file for short term trend
library(ggplot2)
library(data.table)
library(magrittr)

# spltidy data ----

spltidy::norway_covid19_icu_and_hospitalization %>% colnames()


# original data:
# icu_with_positive_pcr_n
# hospitalization_with_covid19_as_primary_cause_n



# ________ -----
# weekly short term trend ----
?splalert::short_term_trend


# x: data
# numerator: name of numerator
# trend_days = 42 number of days to check the trend (6 weeks by default)
# trend_isoweek: ceiling(trend_days/7), 6 weeks by default
# remove_last_isoweeks (days): due to unreliable data/reporting delay


data_weekly_hosp <- spltidy::norway_covid19_icu_and_hospitalization[granularity_time=="isoweek"]


res_weekly <- splalert::short_term_trend(
  data_weekly_hosp,
  numerator = "hospitalization_with_covid19_as_primary_cause_n",
  trend_isoweeks = 6,
  remove_last_isoweeks = 1
)



colnames(res_weekly)
# result: (in addition)
# forecasted_n
# forecasted_predinterval_q02x5_n
# forecasted_predinterval_q97x5_n
# forecasted_n_forecast
# trend0_42_status
# doublingdays0_42



# attach status

res_weekly[, hospitalization_with_covid19_as_primary_cause_trend0_42_status := factor(
  hospitalization_with_covid19_as_primary_cause_trend0_42_status,
  levels = c("training","forecast","decreasing", "null", "increasing"),
  labels = c("Training","Forecast","Decreasing", "Null", "Increasing")
)]
res_weekly[80]

# !!!! need to understand the algorithm
# training?



# plot ----
# use forecasted n (not original n)
# fill the columns with status



q <- ggplot(res_weekly, aes(x = isoyearweek,
                     y = hospitalization_with_covid19_as_primary_cause_forecasted_n,
                     group = 1))
q <- q + geom_col(mapping = aes(fill = hospitalization_with_covid19_as_primary_cause_trend0_42_status ))
q <- q + geom_errorbar(
  mapping = aes(
    ymin = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q02x5_n,
    ymax = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q97x5_n
  )
)
q <- q + scale_y_continuous("Weekly hospitalization with Covid-19 as primary cause", expand = c(0, 0.1))
q <- q + scale_x_discrete("Isoyearweek", breaks = splstyle::every_nth(8))
q <- q + expand_limits(y=0)
q <- q + splstyle::scale_fill_fhi("6 week trend", palette = "contrast")
q <- q + splstyle::theme_fhi_lines_horizontal(legend_position = "bottom")
q <- q + splstyle::set_x_axis_vertical()
q





# ________ ----
# daily ----


data_daily_hosp <- spltidy::norway_covid19_icu_and_hospitalization[granularity_time=="day"]


# 0-28 trend
res_daily <- splalert::short_term_trend(
  data_daily_hosp,
  numerator = "hospitalization_with_covid19_as_primary_cause_n",
  trend_days = 28,
  remove_last_days = 7
)


res_daily[, hospitalization_with_covid19_as_primary_cause_trend0_28_status := factor(
  hospitalization_with_covid19_as_primary_cause_trend0_28_status,
  levels = c("training","forecast","decreasing", "null", "increasing"),
  labels = c("Training","Forecast","Decreasing", "Null", "Increasing")
)]
res_daily[80]


# plot ----
q <- ggplot(data = res_daily, mapping = aes(x = date, y = hospitalization_with_covid19_as_primary_cause_forecasted_n, fill = hospitalization_with_covid19_as_primary_cause_trend0_28_status))
q <- q + geom_col(alpha = 0) # necessary to get legend to be in right order
q <- q + geom_col(
  data = res_daily[hospitalization_with_covid19_as_primary_cause_trend0_28_status!="Forecast"]
)
q <- q + geom_ribbon(
  data = res_daily[hospitalization_with_covid19_as_primary_cause_trend0_28_status=="Forecast"],
  mapping = aes(
    ymin = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q02x5_n,
    ymax = hospitalization_with_covid19_as_primary_cause_forecasted_predinterval_q97x5_n
  ),
  alpha = 0.75
)
q <- q + splstyle::scale_fill_fhi("28 days trend", palette = "contrast")
q <- q + splstyle::theme_fhi_lines_horizontal(legend_position = "bottom")
q <- q + splstyle::set_x_axis_vertical()

q


# plot 2 ----- #
# q <- ggplot(res_daily, aes(x = date, y = hospitalization_with_covid19_as_primary_cause_doublingdays0_28))
# q <- q + geom_rect(aes(xmin = date-1, xmax=date, ymin = 1, ymax = Inf, fill = hospitalization_with_covid19_as_primary_cause_trend0_28_status), alpha = 0.5)
# q <- q + geom_line(lwd = 1)
# q <- q + scale_y_continuous(trans = "log10", expand = c(0, 0.1))
# q <- q + splstyle::scale_fill_fhi(palette = "contrast")
# q <- q + splstyle::theme_fhi_lines_horizontal(legend_position = "bottom")
# q






