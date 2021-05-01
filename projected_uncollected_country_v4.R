source ("/Users/guoyu.zhu/Documents/code/R/init_setting.r")

### Set filepaths
# driver_path = '~/Developer/fraud_dash/data/'
# rider_path = '~/Developer/fraud_dash/data/'

rider_path <- "/Users/guoyu.zhu/Documents/lossforecast/monthlyLoss/"

### Import libraries
library(shiny)
library(ggplot2)
library(lubridate)
library(reshape2)
library(shinydashboard.uber)
library(googleVis)
library(dplyr)
library(dygraphs)
library(xts)
library(DT)
library(utils)  ###View
library(reshape2) ###dcast

library(zoo) #### na.locf
options(stringsAsFactors = FALSE)

#########################################################################################################################
######## Get Gross Bookings and DNS projection                                                  
#########################################################################################################################

project_fun <- function(region_sel, marketplace_sel) {
  
  ### Calculate DNS projection city level for selected regions and marketplaces ###
  
  uc_project <- 
    uc_uncollected %>%
    group_by(region, country_id, city_id, trip_date, marketplace, cancellation_flag) %>%
    # Assume all chargebacks have arrived by 120 days
    filter(  trip_date >= project_start, trip_date <= project_end, days_past == max(days_past), cancellation_flag == 'false', 
             marketplace %in% marketplace_sel, region %in% region_sel
    ) %>%  
    group_by(region, country_id, city_id, trip_date) %>%
    summarize(  # Calculate totals  for each token_type and group of number of days between bill attempt and trip
      days_past = max(days_past),
      unsettled_tot = sum(unsettled_tot, na.rm=T),
      trip_tot = sum(trip_ct, na.rm=T),
      gross_bills = sum(gross_bills, na.rm=T),
      gross_fares = sum(abs(gross_fares), na.rm=T),
      unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T))
  
  uc_project_fares <-
    uc_uncollected %>%
    filter(  trip_date >= project_start, trip_date <= project_end , cancellation_flag == 'false',
             marketplace %in% marketplace_sel, region %in% region_sel
    ) %>%
    group_by(region, country_id, city_id, trip_date) %>%
    summarize(  gross_bills_tot = sum(abs(gross_bills), na.rm=T),
                gross_fares_tot = sum(abs(gross_fares), na.rm=T),
                trip_all = sum(trip_ct, na.rm=T))
  
  uc_project_all <- 
    uc_project %>% 
    inner_join(uc_project_fares, by = c('region', 'country_id', 'city_id', 'trip_date')) %>%
    mutate(  unsettled_fare_bps = unsettled_tot / gross_fares_tot * 10000)
  
  uc_project_baseline <-
    uc_project_all %>%
    inner_join(uc_baseline_curve, by = c('region', 'days_past', 'country_id')) %>%
    mutate(  uc_project = unsettled_tot * unsettled_baseline_pct,
             uc_project_bps = uc_project / gross_fares_tot * 10000,
             uc_bps = unsettled_tot / gross_fares_tot * 10000) 
  
  return(uc_project_baseline)
}

project_aggregate <- function(data, region_flag) {
  
  ### Aggregate DNS projection stats ###
  
  if (region_flag == T) {
    region_order <- c('US & Canada',	'EMEA',	'LatAm',	'SENA',	'ANZ',	'India')
    uc_project_aggregate <-
      data %>% 
      group_by() %>%
      mutate(region_n = ifelse(country_id %in% c(10,126), 'ANZ', region)) %>%
      group_by(month(trip_date), region_n) %>%
      summarize(  unsettled_tot = sum(unsettled_tot, na.rm=T),
                  uc_project_tot = sum(uc_project, na.rm=T),
                  gross_fares_tot = sum(gross_fares_tot, na.rm=T))  %>%
      mutate(region_n =  factor(region_n, levels = region_order)) %>%
      arrange(region_n) 
    
  } else if (region_flag == F) {
    uc_project_aggregate <- 
      data %>%
      group_by(trip_date) %>%
      summarize(   unsettled_tot = sum(unsettled_tot, na.rm=T),
                   unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T),
                   gross_bills_tot = sum(gross_bills_tot, na.rm=T),
                   gross_fares_tot = sum(gross_fares_tot, na.rm=T),
                   trip_all = sum(trip_all, na.rm=T),
                   uc_project_tot = sum(uc_project, na.rm=T)) %>%
      mutate(  uc_project_bps = uc_project_tot / gross_fares_tot * 10000,
               uc_bps = unsettled_tot / gross_fares_tot * 10000)
  }
  
  return(uc_project_aggregate)
}

### Load the DNS baseline data to calculate DNS development factors
# cohort_raw <- read.csv(paste0(rider_path, 'cohort_dns_project_test_2.csv'))

cohort_raw <- runHiveQuery("https://qb.uberinternal.com/querybuilder/builder/B4L7m45jze")
dim(cohort_raw)
head(cohort_raw)
tbl_df(cohort_raw)

### Load monthly data for DNS projection
# uncollected_project_raw <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/queries/02b97f52-1579-4d5a-ab9a-9149a12a239a")
uncollected_project_raw <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/8oTgLhHH3b", date_start='2017-05-01', date_end='2017-06-01')
dim(uncollected_project_raw)
head(uncollected_project_raw)


cohort <- cohort_raw %>%
  ###filter(region != 'China', recent_bill_date != '\\N') %>%
  filter(region != 'China', !is.na(recent_bill_date)) %>%
  mutate( trip_date = as.Date(trip_date),
          recent_bill_date = as.Date(recent_bill_date), 
          week = as.numeric(format(trip_date, "%V")) %% 53 + (as.numeric(format(trip_date, "%Y")) - 2015) * 53,
          month = as.numeric(format(trip_date, "%m")) + (as.numeric(format(trip_date, "%Y")) - 2015) * 12,
          days_past = as.numeric(days_past),
          unsettled_tot = as.numeric(unsettled_tot),
          chargeback_tot = as.numeric(chargeback_tot),
          gross_bills = as.numeric(gross_bills),
          gross_fares = as.numeric(gross_fares),
          country_id = as.character(country_id)) %>%
  filter(days_past >= 0)
head(cohort)
dim(cohort)

uc_uncollected <- uncollected_project_raw %>%
  filter(recent_bill_date != '') %>%
  mutate( trip_date = as.Date(trip_date),
          recent_bill_date = as.Date(recent_bill_date), 
          week = as.numeric(format(trip_date, "%V")) %% 53 + (as.numeric(format(trip_date, "%Y")) - 2015) * 53,
          month = as.numeric(format(trip_date, "%m")) + (as.numeric(format(trip_date, "%Y")) - 2015) * 12,
          country_id = as.character(country_id),
          gross_fares = as.numeric(gross_fares))

head(uc_uncollected)


### Initiate projection parameters

baseline_start = as.Date('2016-06-01')
baseline_end = as.Date('2016-06-30')
project_start = as.Date('2017-05-01')
project_end = as.Date('2017-06-01')
maturity_day = 120


uc_baseline <- 
  cohort %>%
  # Assume all chargebacks have arrived by 120 days
  filter(  trip_date >= baseline_start, trip_date <= baseline_end, days_past <= maturity_day) %>%
  group_by(region, days_past, country_id) %>%
  summarize(  # Calculate totals for each country and group of number of days between bill attempt and trip
    unsettled_tot = sum(unsettled_tot, na.rm=T),
    trip_tot = sum(trip_ct, na.rm=T),
    gross_bills = sum(gross_bills, na.rm=T),
    gross_fares = sum(abs(gross_fares), na.rm=T),
    unsettled_trip_tot = sum(unsettled_trip_ct, na.rm=T))

uc_baseline_total <- 
  uc_baseline %>%
  group_by(region, country_id) %>%
  summarize(  # Calculate totals
    gross_bills_total = sum(gross_bills, na.rm=T),
    gross_fares_total = sum(gross_fares, na.rm=T),
    trips_total = sum(trip_tot, na.rm=T),
    unsettled_total = sum(unsettled_tot, na.rm=T),
    unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T)) %>%
  rename(unsettled_all = unsettled_total, unsettled_trip_all = unsettled_trip_tot) %>%
  select(region, country_id, unsettled_all, unsettled_trip_all, gross_bills_total, gross_fares_total)

uc_baseline_join <- 
  uc_baseline %>% 
  left_join(uc_baseline_total, by = c('region', 'country_id')) 

uc_cumulative <- 
  uc_baseline_join %>% 
  group_by(region, country_id) %>% 
  arrange(days_past) %>%
  mutate( unsettled_cumulative = cumsum(ifelse(is.na(unsettled_tot), 0, unsettled_tot)),
          unsettled_trip_cumulative = cumsum(ifelse(is.na(unsettled_trip_tot), 0, unsettled_trip_tot))) %>%
  arrange(desc(days_past)) %>%
  group_by(region, country_id) %>%
  mutate( settled_later_cumulative = cumsum(ifelse(is.na(gross_bills), 0, gross_bills)) - gross_bills,
          settled_trip_later_cumulative = cumsum(ifelse(is.na(trip_tot), 0, trip_tot)) - trip_tot,
          unsettled_amt = unsettled_cumulative + settled_later_cumulative,
          unsettled_trips = unsettled_trip_cumulative + settled_trip_later_cumulative,
          settled_bills = gross_bills_total - unsettled_amt,
          settled_fares = gross_fares_total - unsettled_amt)

uc_baseline_pct <- 
  uc_cumulative %>% 
  mutate( unsettled_fare_bps = (unsettled_amt / gross_fares_total) * 10000,
          unsettled_baseline_pct = ifelse(ifelse(is.finite(unsettled_all / unsettled_amt), unsettled_all / unsettled_amt, 0) > 1,
                                          1, ifelse(is.finite(unsettled_all / unsettled_amt), unsettled_all / unsettled_amt, 0)),
          unsettled_trip_baseline_pct = ifelse(ifelse(is.finite(unsettled_trip_all / unsettled_trips), unsettled_trip_all / unsettled_trips, 0) > 1,
                                               1, ifelse(is.finite(unsettled_trip_all / unsettled_trips), unsettled_trip_all / unsettled_trips, 0))) %>%
  arrange(days_past)

days_past_all <- data.frame(days_past = 0:120, join_key = 1)

baseline_skeleton <- uc_baseline_pct %>%
  group_by() %>%
  distinct(country_id) %>%
  mutate(join_key = 1) %>%
  inner_join(days_past_all, by = c('join_key')) %>%
  select(country_id, days_past)

uc_baseline_all <- baseline_skeleton %>%
  left_join(uc_baseline_pct, by = c('days_past', 'country_id')) %>%
  arrange(country_id, days_past) %>%
  mutate(  region = na.locf(region),
           unsettled_baseline_pct = na.locf(unsettled_baseline_pct),
           unsettled_trip_baseline_pct = na.locf(unsettled_trip_baseline_pct)
  )

uc_baseline_curve <- 
  uc_baseline_all %>%
  select(region, country_id, days_past, unsettled_baseline_pct) %>%
  arrange(days_past)

uc_baseline_curve_export <-
  uc_baseline_all %>%
  group_by() %>% 
  mutate(country_id = as.numeric(country_id)) %>%
  select(region, country_id, days_past, unsettled_baseline_pct, unsettled_trip_baseline_pct) %>%
  rename(fare_survival_frac = unsettled_baseline_pct, trip_survival_frac = unsettled_trip_baseline_pct) %>%
  filter(fare_survival_frac > 0, !is.na(country_id))



region_all <- c('EMEA', 'India', 'LatAm', 'SENA', 'US & Canada')
region_ex_india <- c('EMEA', 'LatAm', 'SENA', 'US & Canada')
marketplace_all <- c('personal_transport', 'agora')


uc_project_all <- project_fun(region_sel = region_all, marketplace_sel = marketplace_all) 
uc_project_ex_eats <- project_fun(region_sel = region_all, marketplace_sel = 'personal_transport')
uc_project_eats <- project_fun(region_sel = region_all, marketplace_sel = 'agora') 
uc_project_ex_eats_ex_india <- project_fun(region_sel = region_ex_india, marketplace_sel = 'personal_transport')
uc_project_ex_eats_india <- project_fun(region_sel = 'India', marketplace_sel = 'personal_transport')
uc_project_india <- project_fun(region_sel = 'India', marketplace_sel = marketplace_all)


uc_export_all <- # send this to accounting
  uc_project_all %>%  
  select(region, country_id, city_id, trip_date, unsettled_tot, uc_project, gross_fares_tot) %>%
  rename(dns_actual = unsettled_tot, dns_project = uc_project) %>%
  arrange(country_id, city_id, trip_date)

uc_export_ex_eats <- # send this to accounting
  uc_project_ex_eats %>%  
  select(region, country_id, city_id, trip_date, unsettled_tot, uc_project, gross_fares_tot) %>%
  rename(dns_actual = unsettled_tot, dns_project = uc_project) %>%
  arrange(country_id, city_id, trip_date)

write.csv(uc_export_all,file =  paste0(rider_path,"dns_all_may17.csv"))
write.csv(uc_export_ex_eats,file =  paste0(rider_path,"dns_ex_eats_may17.csv"))

print('all trips (incl. India & EATs)')
print_all <- project_aggregate(uc_project_all, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

print('ex EATs')
print_ex_eats <- project_aggregate(uc_project_ex_eats, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

print('EATs')
print_eats <- project_aggregate(uc_project_eats, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

print('ex EATs ex India')
print_ex_eats_ex_India <- project_aggregate(uc_project_ex_eats_ex_india, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

print('ex EATs India')
print_ex_eats_India <- project_aggregate(uc_project_ex_eats_india, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

print('India')
print_India <- project_aggregate(uc_project_india, region_flag = F) %>% 
  group_by(month(trip_date)) %>% 
  summarize(actual_dns = sum(unsettled_tot), project_dns = sum(uc_project_tot), fare_tot = sum(gross_fares_tot))

### print for overall view
print_output <- rbind(print_all,print_ex_eats,print_eats,print_ex_eats_ex_India, print_India, print_ex_eats_India)
print_rowname <- c('all trips','ex_eats','eats','ex_eats_ex_India','India', 'ex_eats_India')
print_tbl <- cbind(print_rowname,print_output[2:4])
print_tbl

View(t(print_tbl))

### print regional breakdown

print('ex EATs by region')
print_ex_eats_region <- project_aggregate(uc_project_ex_eats, region_flag = T)
# print_ex_eats_region
View(t(print_ex_eats_region))

print('EATs by region')
print_eats_region <- project_aggregate(uc_project_eats, region_flag = T)
View(t(print_eats_region))

#########################################################################################################################
######## Get Gross Bookings and Net Bookings
#########################################################################################################################
nb_raw<- runVerticaQuery("https://qb.uberinternal.com/querybuilder/builder/tlvttHTNF8", start_date='2017-05-01', end_date='2017-05-31')

nb_amt <- nb_raw %>% 
    filter(!(mega_region %in% c('', 'China')))

nb_amt
mega_region_order <- c('US & Canada','EMEA','LatAm','APACX','ANZ','India SC')
nb_region_flow <- dcast(nb_amt, Eats_flow ~ factor(mega_region, levels = mega_region_order), value.var="client_net_fare_usd")
View(nb_region_flow)

gb_region_flow <- dcast(nb_amt, Eats_flow ~ factor(mega_region, levels = mega_region_order), value.var="client_gross_fare_usd")
View(gb_region_flow)

gb_split_region_flow <- dcast(nb_amt, Eats_flow ~ factor(mega_region, levels = mega_region_order), value.var="client_gross_fare_usd_fare_split")
View(gb_split_region_flow)

#########################################################################################################################
######## Get the chargeback monthly update                                                  
#########################################################################################################################
###
cb_raw<- runVerticaQuery("https://qb.uberinternal.com/querybuilder/builder/BXoLHxeQiC")
# cb_raw <- runVerticaQuery('/Users/guoyu.zhu/Documents/code/sql/MonthlyLR_Process102.sql')
dim(cb_raw_n)
head(cb_raw)
write.csv(cb_raw,file =  paste0(rider_path,"CB_raw/cb_raw",Sys.Date(),".csv"))  ### keep the file at the time of data pulled.

last_month= as.Date('2017-04-01')
cb_sum <- cb_raw %>% 
  filter(charge_month == last_month) %>% 
  group_by(Eats_flow, mega_region) %>%
  summarize(proj_cb_amt=sum(est_cb_amt,na.rm=T)) 

## table(cb_sum$mega_region)

# library(reshape2) ###dcast
mega_region_order <- c('US & Canada','EMEA','LatAm','APACX','ANZ','India SC')
cb_region_flow <- dcast(cb_sum, Eats_flow ~ factor(mega_region, levels = mega_region_order), value.var="proj_cb_amt")
cb_region_flow


#########################################################################################################################
######## Get ATO loss number                                                  
#########################################################################################################################
### shiny01-sjc1:/mnt/apps/shiny/apps/account_security_dash/data/ato_dash_loss_breakdown_new.csv
### scp shiny01-sjc1:/mnt/apps/shiny/apps/account_security_dash/data/ato_dash_loss_breakdown_new.csv /Users/guoyu.zhu/Documents/lossforecast/monthlyLoss/ATO/

ato_file_bkup <- paste0("cp ",rider_path,"ATO/ato_dash_loss_breakdown_new.csv ", rider_path,"ATO/ato_dash_loss_breakdown_new",Sys.Date(),".csv")
ato_file_bkup
system(ato_file_bkup)

ato_loss_raw <- read.csv(paste0(rider_path,"ATO/ato_dash_loss_breakdown_new.csv"), head=TRUE)
dim(ato_loss_raw)
head(ato_loss_raw)

ato_loss <- ato_loss_raw %>% 
  mutate(f_date=as.Date(date), month = as.numeric(format(f_date, "%m")) )  %>% 
  group_by(month) %>% 
  summarize(tot_refunds = sum(refunds),
            tot_dns = sum(dns),
            tot_cb = sum(cb)) %>% 
  arrange(month) 

ato_loss
View(ato_loss)

