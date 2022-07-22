# use plnr to plan analysis for 12 locations: norge + 11 counties


library(ggplot2)
library(data.table)
library(splmaps)


# ____ plnr example ____ ----

# We begin by defining a new plan
p <- plnr::Plan$new()



# 1. add data ----
data_fn <- function(){
  return(spltidy::covid19_msis_cases_by_time_location)
}

# We add sources of data
# We can add data directly
p$add_data(
  name = "covid19_cases",
  fn_name = "data_fn"
)

p$get_data() %>% names





# 2. add argset ----
# check location codes
location_codes <- p$get_data()$covid19_cases$location_code %>%
  unique() %>%
  print()

p$add_argset_from_list(
  plnr::expand_list(
    location_code = location_codes,
    granularity_time = "isoweek"
  )
)
# Examine the argsets that are available
p$get_argsets_as_dt()





# 3. action ----

# We can then add a simple analysis that returns a figure:

# To do this, we first need to create an action function
# (takes two arguments -- data and argset)

action_fn <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(1)  # county03, isoweek
  }

  # develop function for ONE argset only

  # data
  pd <- data$covid19_cases[
    location_code == argset$location_code &
      granularity_time == argset$granularity_time
  ]

  # the function
  # q <- ggplot(pd, aes(x=date, y=covid19_cases_testdate_n))
  # q <- q + geom_line()
  # q <- q + labs(title = argset$location_code)
  # q
  trend_msis <- splalert::short_term_trend(
    pd,
    numerator = "covid19_cases_testdate_n",
    trend_isoweeks = 6,
    remove_last_isoweeks = 1
  )

  trend_msis


}

p$apply_action_fn_to_all_argsets(fn_name = "action_fn")


# run one by one
p$run_one(1)
p$run_one(2)

# run together
# need to save it!!!!!
res <- p$run_all_progress()

res %>% length

res_county03 <- res[[1]]

# weekly,
res_county03$date
res_county03$isoyearweek
plot(res_county03[, .(date, covid19_cases_testdate_n)])


# find one week that each county is different? or not necessary
# for demonstration purpose, just manually set it to increasing?
res_unlisted <- rbindlist(res)
res_unlisted %>% dim

# filter on one particular week
# res_unlisted[date == '2021-09-12']
# res_unlisted[isoyearweek == '2021-50']

# map making: trend + case per 100k----
res_unlisted[isoyearweek == '2021-40']

res_unlisted[isoyearweek == '2021-42']

res_unlisted[isoyearweek == '2021-44']



# select data for one week (narrow data)
this_isoyearweek <- '2021-44'
d_msis_this_isoyearweek <- res_unlisted[granularity_geo == 'county' &
                                   isoyearweek == this_isoyearweek,
                                   .(location_code,
                                     date,
                                     isoyearweek,
                                     covid19_cases_testdate_pr100000,
                                     covid19_cases_testdate_trend0_42_status)]


d_msis_this_isoyearweek
# setnames(d_msis_this_isoyearweek, 'covid19_cases_testdate_pr100000', '')

pd <- copy(splmaps::norway_nuts3_map_b2020_insert_oslo_dt)

# assign each location a random category for different colors
# location_info <- unique(pd[,c("location_code")])
# location_info[,category:=rep(
#   c("Good","Normal","Neutral","Bad","Very Bad"),
#   each=3)[1:.N]
# ]
# location_info[,category:=factor(
#   category,
#   levels=c("Good","Normal","Neutral","Bad","Very Bad")
# )
# ]
# print(location_info)

d_msis_this_isoyearweek[, trend := factor(
  covid19_cases_testdate_trend0_42_status,
  levels = c('decreasing','null', 'increasing')
)]



# join the map data.table
pd[d_msis_this_isoyearweek,on="location_code",trend:=trend]
pd

# plot map
q <- ggplot()
q <- q + geom_polygon(
  data = pd,
  mapping = aes(x = long, y = lat, group = group,fill=trend),
  color="black",
  size=0.25
)
q <- q + coord_quickmap()
q <- q + theme_void()
q <- q + labs(title=glue::glue("MSIS cases per 100k population for week ", this_isoyearweek))

q <- q + splstyle::scale_fill_fhi("Category",palette = "map_seq_complete", direction = 1, drop=F)

q
q <- q + labs(caption = "Data updated 2021 week 44. Demonstration only")

# add watermark
coord <- watermark_coord(fig_obj = q, xdate = F, lab_position = 'bottomright')
q <- watermark_insert(fig_obj = q, xx = coord$xx, yy = coord$yy,
                      hj = coord$hj, vj = coord$vj)
q


# now we add label to the map
# which contains county name + case per 100k
labels <- copy(splmaps::norway_nuts3_position_geolabels_b2020_insert_oslo_dt)
labels[
  d_msis_this_isoyearweek,
  on = "location_code",
  cases_100k := covid19_cases_testdate_pr100000
]

labels[
  spldata::norway_locations_names_b2020,
  on = "location_code",
  location_name := location_name
]



# format case, remove decimal
labels[, cases_100k := splstyle::format_nor_num_0(cases_100k)]
labels

# put case 100k together with county name
labels[, label := paste0(location_name, '\n', cases_100k)]


q <- q + ggrepel::geom_label_repel(
  data = labels,
  mapping = aes(x = long, y = lat, label = label)
)
q
# add title



q




# table making: new cases per 100k ---- #






# functions ----
watermark_coord <- function(fig_obj,
                            xdate = F,
                            lab_position = 'topleft',
                            vjust_manual = NULL,
                            hjust_manual = NULL){
  # lab_position <- 'topright'
  # lab_position <- 'bottomleft'
  # lab_position <- 'bottomright'
  # x,y coordinates
  label_coord <- data.table(
    position = c('topleft', 'topright', 'bottomleft', 'bottomright'),
    xx = c(-Inf, Inf, -Inf, Inf),
    yy = c(Inf, Inf, -Inf, -Inf),
    hj = c(0, 1, 0, 1),
    vj = c(1, 1, -0.3, -0.3)
  )


  if(xdate == T){
    # replace xx by date boundary
    xdate_min <- as.Date(layer_scales(fig_obj)$x$get_limits()[1], origin = '1970-01-01')
    xdate_max <- as.Date(layer_scales(fig_obj)$x$get_limits()[2], origin = '1970-01-01')
    label_coord[position %in% c('topleft', 'bottomleft'), xx := xdate_min]
    label_coord[position %in% c('topright', 'bottomright'), xx := xdate_max]
    label_coord[, xx := as.Date(xx, origin = '1970-01-01')]
  }

  # specify location of label
  label_pos <- label_coord[position == lab_position]

  # manual adjustment
  vj <- label_pos$vj
  hj <- label_pos$hj
  if(!is.null(vjust_manual)){vj <- vjust_manual}
  if(!is.null(hjust_manual)){hj <- hjust_manual}

  return(list(xx = label_pos$xx,
              yy = label_pos$yy,
              hj = hj,
              vj = vj))

}

watermark_insert <- function(fig_obj,
                             xx,
                             yy,
                             hj,
                             vj,
                             credit_text = 'Sykdomspulsen, FHI',
                             small_label = F){
  # fig_obj <- q
  # label size
  # small_label <- F
  # credit_text <- 'Sykdomspulsen, FHI'
  if(small_label == T){
    size <- 3
  }else{
    size <- 4
  }

  # add annotation
  fig_anno <- fig_obj + annotate(
    'label',
    x = xx,
    y = yy,
    hjust = hj,
    vjust = vj,
    label.r = unit(0, 'lines'),
    label.size = 0,
    size = size,
    label = credit_text,  # Sykdomspulsen, FHI
    color = 'black',
    alpha = 0.75
  )
  fig_anno
  return(fig_anno)

}







