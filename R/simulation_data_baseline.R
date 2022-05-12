
gen_splfmt_rts_baseline_data <- function(start_date,
                                         end_date){

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

      d <- spltidy::splfmt_rts_data_v1(data.table(
        location_code = "norge",
        date = seq.Date(start_date,end_date,by="day"),
        age = "total",
        sex = "total",
        border = 2020
      ))
      d[, time:=1:.N]

      d[, wday:=lubridate::wday(date)]

  return(d)
}

periodic_pattern <- function(n_p=2,
                             g1=0.8,
                             g2=0.4,
                             s=29,
                             p=52*7,
                             t=1:nrow(d)){

        l <-  1:n_p

        c <- rep(0, length(t))

        for(i in 1:length(t)){

            c[i] <- sum(g1*cos((2*pi*l*(t[i]+s))/(p))+g2*sin((2*pi*l*(t[i]+s))/(p)))
        }

        return(c)
}

#' simulate baseline data ----
#' Simulate (daily) baseline data
#'
#' @description
#' Simulates a time series of counts based on the Poisson/Negative Binomial model as described in Noufaily et al. (2019)
#'
#' @param start_date Starting date of the simulation period.
#'   Date is in the format of 'yyyy-mm-dd'.
#'
#' @param end_date Ending date of the simulation period.
#'
#' @param seasonal_pattern
#'
#' @param weekly_pattern
#'
#' @param param_list List of parameters to control the baseline simulation.
#'
#' @details
#'
#' @return
#' A splfmt_rts_data_v1, data.table containing
#'
#' \describe{
#'   \item{granularity_time}{Pseudo ID for death events.}
#'   \item{granularity_geo}{Pseudo ID for death events.}
#'   \item{wday}{Pseudo ID for death events.}
#' }
#'
#'
#' @export
#' @examples
#' start_date <- '2018-01-01'
#' end_date <- '2019-12-31'
#'
# baseline  <- simulate_baseline_data(
# start_date = as.Date("2012-01-01")
# end_date = as.Date("2019-12-31")
# seasonal_pattern_n = 1
# weekly_pattern_n = 1
# param_list = list(alpha = 3, beta = 0,gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.8, gamma4 = 0.4, phi = 4,shift = 29 )
#' )


simulate_baseline_data <-  function(start_date,
                                    end_date,
                                    seasonal_pattern_n,
                                    weekly_pattern_n,
                                    baseline_param_list = list(alpha, beta, gamma1,gamma3,gamma4,phi,shift)){


  list2env(baseline_param_list, envir = globalenv())

  d <- gen_splfmt_rts_baseline_data(start_date, end_date)
  t <- 1:nrow(d)

  if (seasonal_pattern_n == 0 & weekly_pattern_n == 0){

    d[, expected_mean := exp((alpha) + (beta * time))]

  } else {

        if (seasonal_pattern_n == 0){

            wp_n = weekly_pattern_n

            d[, trend := alpha + beta * (time + shift)]
            d[, weekly_pattern := periodic_pattern(wp_n,gamma3,gamma4,shift,7,t)]

            d[, mu:= exp(trend + weekly_pattern)]

        } else {
            wp_n = weekly_pattern_n
            sp_n = seasonal_pattern_n

            d[, trend := alpha + beta * (time + shift)]
            d[, seasonal_pattern := periodic_pattern(sp_n,gamma1,gamma2,shift,52*7,t)]
            d[, weekly_pattern := periodic_pattern(wp_n,gamma3,gamma4,shift,7,t)]

            d[, mu:= exp(trend + seasonal_pattern + weekly_pattern)]

            }

  }


  mu <- d$mu

  if (phi==1) {
    d[, n := rpois(.N, lambda=mu)]
  } else {
    prob <- 1/phi
    size <- mu/(phi-1)
    d[, n_tot := rnbinom(.N,size=size,prob=prob)]
  }




  return(d)
}






#' Simulate seasonal outbreak data ----
#' Simulate (daily) seasonal outbreak data
#'
#' @description
#' @param splfmt_rds_data
#' Simulated or real data
#' @param start_date Starting date simulated outbreak.
#'   Date is in the format of 'yyyy-mm-dd'.
#' @param numoutbk Ending date of the simulation period.
#' @details
#'
#' @return
#' A splfmt_rts_data_v1, data.table containing
#'
#' \describe{
#'   \item{granularity_time}{Pseudo ID for death events.}
#'   \item{granularity_geo}{Pseudo ID for death events.}
#'   \item{wday}{Pseudo ID for death events.}
#' }
#'
#'
#' @export
#' @examples
#' start_date <- '2018-01-01'
#' end_date <- '2019-12-31'
#'
#' baseline  <- simulate_baseline_data(
#'  start_date = as.Date("2012-01-01")
#'  end_date = as.Date("2019-12-31")
#'  seasonal_pattern_n = 1
#'  weekly_pattern_n = 2
#' baseline_param_list = list(alpha = 3, beta = 0, gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.8, gamma4 = 0.4, phi = 4, shift = 29)
#' outbreak_param_list = list(m=2)
#' )

simulate_seasonal_outbreak_data <-  function(data,
                                             week_season_start = 40,
                                             week_season_peak = 4,
                                             week_season_end = 20,
                                             num_season_outbreak = 1,
                                             baseline_param_list = list(alpha, beta, gamma1,gamma3,gamma4,phi,shift),
                                             outbreak_param_list = list(m)){

  list2env(outbreak_param_list, envir = globalenv())
  list2env(baseline_param_list, envir = globalenv())


  d <- copy(data)
  N <- nrow(d)
  d[, sd:= sqrt(mu*phi)]

  d[wday==1, weight:=0.5]
  d[wday==2, weight:=1]
  d[wday==3, weight:=1]
  d[wday==4, weight:=1]
  d[wday==5, weight:=1]
  d[wday==6, weight:=2]
  d[wday==7, weight:=2]

  n_year <- length(unique(d$calyear))

  years <- sort(unique(d$calyear))[1:(n_year-1)]
  n_out <- sample(1:length(years),1)

  years <- sort(sample(years,n_out,replace=F))

  d[, seasonal_outbreak_n:=0]
  d[, seasonal_outbreak_n_rw:=0]

  for (y in years) {
    set.seed(y)

    wtime <- c(paste(y,c(week_season_start:52),sep="-"), paste(y+1,stringr::str_pad(c(1:week_season_end),2,pad="0"),sep="-"))
    time <- d[isoyearweek %in% wtime]$time

    startoutbk <- sample(time, num_season_outbreak,replace = FALSE,p=abs(rnorm(length(time))))


  # OUTBREAK SIZE OF CASES

      sizeoutbk=rep(0,num_season_outbreak)
      soutbk=1
      sou=1

      while(soutbk<2){
        set.seed(sou)
        s <- d[time == startoutbk]$sd
        soutbk=rpois(1,s*m*10)
        sou=sou+1
      }

      sizeoutbk=soutbk


  # DISTRIBUTE THESE CASES OVER TIME USING LOGNORMAL
      outbk <-rlnorm(sizeoutbk, meanlog = 0, sdlog = 0.5)
      h <- hist(outbk,breaks=seq(0,ceiling(max(outbk)),0.1),plot=FALSE)
      cases <- h$counts
      duration <-startoutbk:(startoutbk+length(cases)-1)

      d[time %in% duration, seasonal_outbreak_n := cases]
      d[time %in% duration, seasonal_outbreak_n_rw := cases*weight]
      d[time %in% duration, seasonal_outbreak:=1]

  }


  d[,n_tot := n_tot + seasonal_outbreak_n_rw]

  return(d)
}



#' simulate spike outbreak data ----
#' Simulate (daily) spike outbreak data
#'
#' @description
#' @param splfmt_rds_data
#' Simulated or real data
#' @param start_date Starting date simulated outbreak.
#'   Date is in the format of 'yyyy-mm-dd'.
#' @param numoutbk Ending date of the simulation period.
#' @details
#'
#' @return
#' A splfmt_rts_data_v1, data.table containing
#'
#' \describe{
#'   \item{granularity_time}{Pseudo ID for death events.}
#'   \item{granularity_geo}{Pseudo ID for death events.}
#'   \item{wday}{Pseudo ID for death events.}
#' }
#'
#'
#' @export
#' @examples
#' start_date <- '2018-01-01'
#' end_date <- '2019-12-31'
#'
#' baseline  <- simulate_baseline_data(
#'  start_date = as.Date("2012-01-01")
#'  end_date = as.Date("2019-12-31")
#'  seasonal_pattern_n = 1
#'  weekly_pattern_n = 2
#' baseline_param_list = list(alpha = 3, beta = 0, gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.8, gamma4 = 0.4, phi = 4, shift = 29)
#' outbreak_param_list = list(m=2)
#'
#' # )

simulate_spike_outbreak_data <-  function(data,
                                          num_sp_outbreak=1,
                                          baseline_param_list = list(alpha, beta, gamma1,gamma3,gamma4,phi,shift),
                                          outbreak_param_list = list(m)){

  list2env(outbreak_param_list, envir = globalenv())
  list2env(baseline_param_list, envir = globalenv())


  d <- copy(data)

  N <- nrow(d)
  d[, sd:= sqrt(mu*phi)]

  d[wday==1, weight:=0.5]
  d[wday==2, weight:=1]
  d[wday==3, weight:=1]
  d[wday==4, weight:=1]
  d[wday==5, weight:=1]
  d[wday==6, weight:=2]
  d[wday==7, weight:=2]


    wtime <- (nrow(d) - 49*7) : nrow(d)

    time <- d[time %in% wtime]$time

    startoutbk <- sample(time, num_sp_outbreak,replace = FALSE)

    # OUTBREAK SIZE OF CASES

    sizeoutbk=rep(0,num_sp_outbreak)
    soutbk=1
    sou=1

    d[, sp_outbreak_n:=0]
    d[, sp_outbreak_n_rw:=0]

    while(soutbk<2){
      set.seed(sou)
      s <- d[time == startoutbk]$sd
      soutbk=rpois(1,s*m*10)
      sou=sou+1
    }

    sizeoutbk=soutbk


    # DISTRIBUTE THESE CASES OVER TIME USING LOGNORMAL
    outbk <-rlnorm(sizeoutbk, meanlog = 0, sdlog = 0.5)
    h <- hist(outbk,breaks=seq(0,ceiling(max(outbk)),0.2),plot=FALSE)
    cases <- h$counts
    duration <-startoutbk:(startoutbk+length(cases)-1)

    d[time %in% duration, sp_outbreak_n := cases]
    d[time %in% duration, sp_outbreak_n_rw := cases*weight]
    d[time %in% duration, sp_outbreak:=1]


    d[,n_tot := n_tot + sp_outbreak_n]


    return(d)
}




#' add holiday effect ----
#' add holiday efect to baseline data
#'
#' @description
#' @param splfmt_rds_data
#' Simulated or real data
#' @param start_date Starting date simulated outbreak.
#'   Date is in the format of 'yyyy-mm-dd'.
#' @param numoutbk Ending date of the simulation period.
#' @details
#'
#' @return
#' A splfmt_rts_data_v1, data.table containing
#'
#' \describe{
#'   \item{granularity_time}{Pseudo ID for death events.}
#'   \item{granularity_geo}{Pseudo ID for death events.}
#'   \item{wday}{Pseudo ID for death events.}
#' }
#'
#'
#' @export
#' @examples

# holiday_data <- fhidata::norway_dates_holidays


add_holiday_effect <-  function(data,
                                holiday_data,
                                holiday_effect=2){

  d <- copy(data)
  d[
    holiday_data,
    on=c("date"),
    holiday := is_holiday
  ]

  d[, n_tot:= n_tot]
  d[holiday==T, n_tot:= n_tot*holiday_effect]


  return(d)
}




# run_simulation <- function(start_date,
#                            end_date,
#                            seasonal_pattern_n,
#                            weekly_pattern_n,
#                            week_season_start,
#                            week_season_peak ,
#                            week_season_end,
#                            num_season_outbreak,
#                            num_sp_outbreak,
#                            baseline_param_list = list(alpha, beta, gamma1,gamma3,gamma4,phi,shift),
#                            outbreak_param_list = list(m),
#                            nsim) {
#
#
#     retval <- list()
#
#     for (i in 1:(nsim)) {
#
#           data <- simulate_baseline_data(start_date,
#                                      end_date,
#                                      seasonal_pattern_n,
#                                      weekly_pattern_n,
#                                      baseline_param_list)
#       if (seasonal_pattern_n!=0) {
#
#           data <- simulate_seasonal_outbreak_data(data,
#                                               week_season_start,
#                                               week_season_peak ,
#                                               week_season_end,
#                                               num_season_outbreak,
#                                               baseline_param_list,
#                                               outbreak_param_list)
#       }
#
#       data <- simulate_spike_outbreak_data(data,
#                                            num_sp_outbreak,
#                                            baseline_param_list,
#                                            outbreak_param_list)
#
#       retval[[i]] <-  data
#
#
#     }
#
#     return(retval)
#
# }

# sim <- run_simulation(nsim=100,
#                start_date = as.Date("2012-01-01"),
#                end_date = as.Date("2019-12-31"),
#                seasonal_pattern_n = 1,
#                weekly_pattern_n = 1,
#                week_season_start = 40,
#                week_season_peak = 4,
#                week_season_end = 20,
#                num_season_outbreak = 1,
#                num_sp_outbreak = 1,
#                baseline_param_list = list(alpha = 3, beta = 0,gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.8, gamma4 = 0.4, phi = 4,shift = 29 ),
#                outbreak_param_list = list(m=2)
# )
