# use plnr to plan analysis for 12 locations: norge + 11 counties


library(ggplot2)
library(data.table)



# ____ plnr example ____ ----
#
# We begin by defining a new plan
p <- plnr::Plan$new()

# R6 class object

# add data ----
# We can add data directly
p$add_data(
  name = "cases",
  direct = spltidy::covid19_msis_cases_by_time_location
)

p$get_data()

fn_test <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(1)
  }

  d <- data$cases[location_code==argset$location_code]

  return(5)
}

# EQUIVALENT TO add analysis by df/list?
for(i in spldata::norway_locations_names()[granularity_geo %in% c("nation", "county")]$location_code){
  force(i)
  p$add_analysis(
    name = i,
    fn = fn_test,
    location_code = i
  )
}

p$analyses
(x <- p$run_one(2))
(x <- p$run_all())




# We can then add the analysis (function + argset) to the plan
p$add_analysis(
  name = "fig_1_2002",
  fn_name = "fn_fig_1",
  year_max = 2002
)

# And another analysis
p$add_analysis(
  name = "fig_1_2003",
  fn_name = "fn_fig_1",
  year_max = 2003
)

# And another analysis
# (don't need to provide a name if you refer to it via index)
p$add_analysis(
  # name is null
  fn_name = "fn_fig_1",
  year_max = 2004
)

# How many analyses have we created?
p$len()


# check argset ----

# by index
p$get_argset(1) # year_max = 2002
p$get_argset(2)

# by name
p$get_argset('fig_1_2003')


# check analysis ----
p$get_analysis(1)
p$get_analysis('fig_1_2003')



# run analysis ----



# We can run the analysis for each argset (by index and name):
p$run_one("fig_1_2002")
p$run_one(2)



