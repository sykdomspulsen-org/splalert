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
  name = "deaths",
  direct = data.table(deaths=1:4, year=2001:2004)
)

p$get_data()
p$get_data()$deaths  # this is the deaths data

# print hash
# current
# current elements
p$get_data()$hash



# add analysis ----
# We can then add a simple analysis that returns a figure:

# To do this, we first need to create an analysis function
# (takes two arguments -- data and argset)

# fn_analysis <- function(data, argset){
#   # data <- p$get_data()
#   # argset <- p$get_argset("fig_1_2002)
#
#   # function continues here
# }

fn_fig_1 <- function(data, argset){
  plot_data <- data$deaths[year<= argset$year_max]

  q <- ggplot(plot_data, aes(x=year, y=deaths))
  q <- q + geom_line()
  q <- q + geom_point(size=3)
  q <- q + labs(title = glue::glue("Deaths from 2001 until {argset$year_max}"))
  q
}


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



