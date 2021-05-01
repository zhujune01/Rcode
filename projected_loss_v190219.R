#########################################################################################################################
######## Payment Loss Reserving process
#########################################################################################################################

source ("/Users/guoyu.zhu/Documents/code_lib/R/init_setting.r")
source ("/Users/guoyu.zhu/Documents/code_lib/R/pymt_risk_loss_func.R")

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
options(dplyr.print_max = Inf)

#########################################################################################################################
######## Parameters define
#########################################################################################################################

updated_folder <- "2019/201902"
first.of.month <- Sys.Date()  - mday(Sys.Date()-1 ) 
first.of.month
#query_start <- seq(first.of.month, length=2, by='-7 month')[2]
query_start <- as.Date ('2018-06-01')
query_start
query_end <- seq(first.of.month, length=2, by='1 month')[2]
query_end

## on Feb 2018; dim_city changed mega_region value, India SC to India SA; add NewCo Mega-Region for Yandex impacted Cities
mega_region_order <- c('US & Canada','EMEA','LatAm','APACX','ANZ','India SA','NewCo Mega-Region','Southeast Asia','U4B', 'U4F','WALLET_PURCHASE')
marketplace_all <- c('personal_transport', 'agora', 'U4B','U4F','eats')
region_all <- c('US & Canada',  'EMEA',  'LatAm',  'APACX',  'ANZ',  'India SA', 'NewCo Mega-Region', 'Southeast Asia')

###################
## read data
##################
##nb_cb_proj<- runHiveQuery("https://querybuilder-dca1.uberinternal.com/querybuilder/builder/8pajwiletT", date_start = '2017-12-01')
nb_cb_proj_csv <- read.csv(paste0(rider_path, updated_folder,"/nb0218-c2QIbNyJR-run-zGNoY3QtF.csv"), header=TRUE)
head(nb_cb_proj_csv)
nrow(filter(nb_cb_proj_csv, mega_region=='\\N'))
nb_cb_proj <- nb_cb_proj_csv %>% rename(country_id = country_id, mega_region = mega_region, client_net_fare_usd = gross_bills)  %>%
              mutate(mega_region=ifelse(mega_region=='\\N', 'US & Canada', mega_region))
saveRDS(nb_cb_proj, file = paste0(rider_path, updated_folder,"/nb_cb_proj",Sys.Date(),".rds"))

table(nb_cb_proj$marketplace)
max(nb_cb_proj$trip_date)
###### DNS data pull
uncollected_project_raw <- read.csv(paste0(rider_path, updated_folder,"/dns_20190228.csv"), header=TRUE)
saveRDS(uncollected_project_raw, file = paste0(rider_path, updated_folder,"/uncollected_project_raw",Sys.Date(),".rds"))

head(uncollected_project_raw)
nrow(uncollected_project_raw)

#### CB data pull
# cb_gross_csv <- read.csv(paste0(rider_path, updated_folder,"/cb0214-6oQoeCKYz-run-ezML6tlRR.csv"), header=TRUE)
cb_gross_csv <- read.csv(paste0(rider_path, updated_folder,"/cb0227-6oQoeCKYz-run-mpcQAUKNF.csv"), header=TRUE)

head(cb_gross_csv)
head(filter(cb_gross_csv, mega_region=='\\N'))
sum(filter(cb_gross_csv, mega_region=='\\N')$act_cb_amt)
cb_gross_raw <- cb_gross_csv %>% filter(act_cb_amt !='\\N')  %>% 
               mutate(est_cb_amt = as.numeric(est_cb_amt), 
                      est_cb_amt_v0 = as.numeric(est_cb_amt_v0), 
                      act_cb_amt = as.numeric(act_cb_amt))

saveRDS(cb_gross_raw, file = paste0(rider_path, updated_folder,"/cb_gross_raw_gs",Sys.Date(),".rds")) 

#########################################################################################################################
######## Get Gross Bookings and Net Bookings
#########################################################################################################################
head(nb_cb_proj)
max(nb_cb_proj$trip_date)
current_month=as.Date(first.of.month)
current_month

nb_amt <- nb_cb_proj %>% 
  filter(!(mega_region %in% c('', 'China'))) %>% 
  filter(trip_month == current_month) %>% 
  # filter( (trip_date>=as.Date('2017-07-01') & trip_date < as.Date('2017-07-16')) | (trip_date>=as.Date('2017-08-16') & trip_date < as.Date('2017-09-01'))) %>% 
  group_by(eats_flow, mega_region) %>% 
  summarise(client_net_fare_usd = sum(client_net_fare_usd, na.rm=T))
        #    client_gross_fare_usd = sum(client_gross_fare_usd, na.rm=T),
        #    client_gross_fare_usd_fare_split = sum(client_gross_fare_usd_fare_split, na.rm=T) )

# head(nb_amt)
nb_region_flow <- dcast(nb_amt, eats_flow ~ factor(mega_region, levels = mega_region_order), value.var="client_net_fare_usd")
View(nb_region_flow)

#########################################################################################################################
######## DNS Projection
#########################################################################################################################

project_start = as.Date(query_start)
project_end = as.Date(query_end)
project_start
project_end

v1uc_baseline_curve_201701 <- readRDS(paste0(rider_path, "dns_baseline/v2uc_baseline_curve_2017012017-11-16.rds"))
v4uc_baseline_curve_201803_90 <- readRDS(paste0(rider_path, "dns_baseline/uc_baseline_curve_201803_90_2018-07-02.rds"))

v4combo_uc_baseline_curve <- rbind(filter(v1uc_baseline_curve_201701, !(country_id %in% c(1, 32, 15,77,163))), filter(v4uc_baseline_curve_201803_90, country_id %in% c(1, 32, 15,77,163)))
saveRDS(v4combo_uc_baseline_curve, file = paste0(rider_path, "dns_baseline/v4combo_uc_baseline_curve",Sys.Date(),".rds")) 

nrow(v4uc_baseline_curve_201803_90)
table(uncollected_project_raw$mega_region, uncollected_project_raw$trip_month)

uc_project_all_v1_v2 <- f_uc_proj_region(uncollected_project_raw, v1uc_baseline_curve_201701, marketplace_sel = marketplace_all, region_sel = region_all)
uc_project_all_v4_v2 <- f_uc_proj_region(uncollected_project_raw, v4uc_baseline_curve_201803_90, marketplace_sel = marketplace_all, region_sel = region_all)
uc_project_all_v5_v2 <- f_uc_proj_region(uncollected_project_raw, v4combo_uc_baseline_curve, marketplace_sel = marketplace_all, region_sel = region_all)

#saveRDS(uc_project_all_v4_v2, file = paste0(rider_path, updated_folder,"/uc_project_all_v4_v2",Sys.Date(),".rds")) 

head(uncollected_project_raw)
str(uc_project_all_v4_v2)
nrow(uc_project_all_v4_v2)
table(uc_project_all_v4_v2$mega_region)

f_uc_agg_mkpl <- function(uc_project_baseline, marketplace_sel){
  uc_project_aggregate <- uc_project_baseline %>% 
    filter(marketplace %in% marketplace_sel ) %>% 
    mutate(trip_month = substr(trip_date,1,7)) %>%
    group_by(trip_month) %>%
    summarize(  unsettled_tot = sum(unsettled_tot, na.rm=T),
                uc_project_tot = sum(uc_project, na.rm=T),
                gross_bills_tot = sum(gross_bills_tot, na.rm=T))  %>%
    mutate(dns_actual_bps = unsettled_tot/gross_bills_tot * 10000,
           dns_proj_bps = uc_project_tot/gross_bills_tot * 10000) %>% 
    # mutate(mega_region =  factor(mega_region, levels = mega_region_order)) %>%
    arrange(trip_month) 
  
  return(uc_project_aggregate)
}


agg_uc_proj_eats <- f_uc_agg_mkpl(uc_project_all_v5_v2, marketplace_sel=  c('agora','eats'))
View(t(agg_uc_proj_eats))
agg_uc_proj_rides <- f_uc_agg_mkpl(uc_project_all_v5_v2, marketplace_sel= c('personal_transport', 'U4B','U4F'))
View(t(agg_uc_proj_rides))
agg_uc_proj_u4b <- f_uc_agg_mkpl(uc_project_all_v5_v2, marketplace_sel= c('U4B'))
View(t(agg_uc_proj_u4b))
agg_uc_proj_u4f <- f_uc_agg_mkpl(uc_project_all_v5_v2, marketplace_sel= c('U4F'))
View(t(agg_uc_proj_u4f))

agg_uc_proj_eats <- f_uc_agg_mkpl(uc_project_all_v1_v2, marketplace_sel=  c('agora','eats'))
View(t(agg_uc_proj_eats))
agg_uc_proj_rides <- f_uc_agg_mkpl(uc_project_all_v1_v2, marketplace_sel= c('personal_transport', 'U4B','U4F'))
View(t(agg_uc_proj_rides))
agg_uc_proj_u4b <- f_uc_agg_mkpl(uc_project_all_v1_v2, marketplace_sel= c('U4B'))
View(t(agg_uc_proj_u4b))
agg_uc_proj_u4f <- f_uc_agg_mkpl(uc_project_all_v1_v2, marketplace_sel= c('U4F'))
View(t(agg_uc_proj_u4f))

f_uc_agg_mos <- function(uc_project_baseline, start_dt, end_dt, marketplace_sel){
  uc_project_aggregate <- uc_project_baseline %>% 
    group_by() %>%
    filter( (trip_date >= start_dt & trip_date < end_dt),  marketplace %in% marketplace_sel ) %>% 
    #mutate(J_mega_region= ifelse(marketplace %in% c('U4F', 'U4B'), marketplace,ifelse(mega_region=='ANZ', 'APACX', mega_region))) %>%
    mutate(J_mega_region= ifelse(mega_region=='ANZ', 'APACX', mega_region)) %>%
    group_by(J_mega_region) %>%
    summarize(  unsettled_tot = sum(unsettled_tot, na.rm=T),
                uc_project_tot = sum(uc_project, na.rm=T),
                ## gross_fares_tot = sum(gross_fares_tot, na.rm=T),
                gross_bills_tot = sum(gross_bills_tot, na.rm=T))  %>%
    mutate(dns_actual_bps = unsettled_tot/gross_bills_tot * 10000,
           dns_proj_bps = uc_project_tot/gross_bills_tot * 10000) %>% 
    mutate(J_mega_region =  factor(J_mega_region, levels = mega_region_order)) %>%
    arrange(J_mega_region) 
  
  return(uc_project_aggregate)
}

# dns_start_dt = Sys.Date() - 60
# dns_end_dt = Sys.Date() - 30

dns_start_dt = as.Date('2019-01-01')
dns_end_dt = as.Date('2019-02-01')

one_uc_project_eats_v4 <- f_uc_agg_mos(uc_project_all_v5_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('agora','eats'))
View(t(one_uc_project_eats_v4))
one_uc_project_rides_v4 <- f_uc_agg_mos(uc_project_all_v5_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('personal_transport', 'U4B','U4F'))
View(t(one_uc_project_rides_v4))

one_uc_project_U4B_v4 <- f_uc_agg_mos(uc_project_all_v5_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('U4B'))
View(t(one_uc_project_U4B_v4))
one_uc_project_U4F_v4 <- f_uc_agg_mos(uc_project_all_v5_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('U4F'))
View(t(one_uc_project_U4F_v4))

one_uc_project_eats_v1 <- f_uc_agg_mos(uc_project_all_v1_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('agora','eats'))
View(t(one_uc_project_eats_v1))
one_uc_project_rides_v1 <- f_uc_agg_mos(uc_project_all_v1_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('personal_transport', 'U4B','U4F'))
View(t(one_uc_project_rides_v1))

one_uc_project_U4B_v4 <- f_uc_agg_mos(uc_project_all_v1_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('U4B'))
View(t(one_uc_project_U4B_v4))
one_uc_project_U4F_v4 <- f_uc_agg_mos(uc_project_all_v1_v2, start_dt=dns_start_dt, end_dt=dns_end_dt, marketplace_sel= c('U4F'))
View(t(one_uc_project_U4F_v4))

write.csv(one_uc_project_all, file = paste0(rider_path,updated_folder,"/one_uc_project_all.csv"))
write.csv(agg_uc_proj_mkpl, file = paste0(rider_path,updated_folder,"/agg_uc_proj_mkpl.csv"))

#########################################################################################################################
######## Get the chargeback monthly update                                                  
#########################################################################################################################
#### bills ever had chargebacks 
head(cb_gross_raw)

cb_seg <- cb_gross_raw %>% 
  group_by(charge_month, trip_month, dispute_month, adj_state, eats_flow, lob_flow, job_type, mega_region) %>%
  summarize(proj_cb_amt=sum(est_cb_amt,na.rm=T),
            proj_cb_amt_v0=sum(est_cb_amt_v0,na.rm=T),
            act_cb_amt = sum(act_cb_amt,na.rm=T),
            n_trip = sum(n_trip,na.rm=T),
            n = sum(n,na.rm=T))

cb_seg$update_date <- Sys.Date()
write.csv(cb_seg, file = paste0(rider_path,updated_folder,"/cb_seg",Sys.Date(),".csv"))

cb_gross <- cb_gross_raw %>%
    ##filter(mega_region !='\\N') %>%
    mutate(eats_flow = ifelse(job_type == 'WALLET_PURCHASE', 'Z', ifelse(lob_flow %in% c('U4B','U4F'), lob_flow, eats_flow)),
           # J_mega_region=ifelse(job_type == 'WALLET_PURCHASE', 'WALLET_PURCHASE',
           #                    ifelse(lob_flow %in% c('U4B','U4F'), lob_flow,
           #                    ifelse(is.na(mega_region)|(mega_region =='\\N'), 'US & Canada',mega_region))),
           J_mega_region=ifelse(is.na(mega_region)|(mega_region =='\\N'), 'US & Canada',mega_region),
           charge_date = as.Date(charge_date),
           charge_month = as.Date(charge_month),
           est_cb_amt = as.numeric(est_cb_amt),
           est_cb_amt_v0 = as.numeric(est_cb_amt_v0),
           act_cb_amt = as.numeric(act_cb_amt)
           )
table(cb_gross$J_mega_region,useNA="always")
table(cb_gross$eats_flow,useNA="always")
saveRDS(cb_gross, file = paste0(rider_path, updated_folder,"/cb_gross",Sys.Date(),".rds")) 
head(cb_gross)


#### net chargebacks as of today
cb_net <- cb_gross %>% filter (adj_state %in% c('NetCB Pre-Arbitration', 'NetCB NOC', 'NetCB','Others'))
saveRDS(cb_net, file = paste0(rider_path, updated_folder,"/cb_net",Sys.Date(),".rds")) 
max(cb_net$charge_date)

#### had chargebacks but already settled as of now 
cb_setttled <- cb_gross %>% filter (adj_state %in% c('Representment Won', 'Self Resolved'))
saveRDS(cb_setttled, file = paste0(rider_path, updated_folder,"/cb_setttled",Sys.Date(),".rds")) 

head(cb_net)
head(cb_setttled)

table(cb_net$mega_region)
filter(cb_net, anyNA(mega_region))

# cb_start_dt = Sys.Date() - 60
# cb_end_dt = Sys.Date() - 30

cb_start_dt = as.Date('2019-01-15')
cb_end_dt = as.Date('2019-02-15')
##str(cb_net)
f_cb_flow_agg <- function(cb_raw) {
  cb_sum <- cb_raw %>% 
    mutate(J_mega_region= ifelse(J_mega_region=='ANZ', 'APACX', J_mega_region)) %>%
    filter( (charge_date >= cb_start_dt & charge_date < cb_end_dt)) %>% 
    group_by(eats_flow, J_mega_region) %>%
    summarize(proj_cb_amt=sum(est_cb_amt,na.rm=T),
              proj_cb_amt_v0=sum(est_cb_amt_v0,na.rm=T),
              act_cb_amt = sum(act_cb_amt,na.rm=T)) 
  
  return(cb_sum)
}
## table(cb_net_sum$mega_region, useNA="always")
cb_net_sum <- f_cb_flow_agg (cb_net)
proj_cb_net <- dcast(cb_net_sum, eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="proj_cb_amt")
proj_cb_net[is.na(proj_cb_net)] <- 0 
View(proj_cb_net)

act_cb_net <- dcast(cb_net_sum, eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="act_cb_amt")
act_cb_net[is.na(act_cb_net)] <- 0 
View(act_cb_net)

v0_proj_cb_net <- dcast(cb_net_sum, eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="proj_cb_amt_v0")
v0_proj_cb_net[is.na(v0_proj_cb_net)] <- 0 
View(v0_proj_cb_net)

#### Total settled already
head(cb_setttled)

cb_setttled_sum <- cb_setttled %>% 
  mutate(J_mega_region= ifelse(J_mega_region=='ANZ', 'APACX', J_mega_region)) %>%
  filter( (charge_date >= cb_start_dt & charge_date < cb_end_dt)) %>% 
  group_by(win,eats_flow, J_mega_region) %>%
  summarize(proj_cb_amt=sum(est_cb_amt,na.rm=T),
            proj_cb_amt_v0=sum(est_cb_amt_v0,na.rm=T),
            act_cb_amt = sum(act_cb_amt,na.rm=T)) 


proj_cb_setttled <- dcast(cb_setttled_sum, win + eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="proj_cb_amt")
proj_cb_setttled[is.na(proj_cb_setttled)] <- 0 
View(proj_cb_setttled)

act_cb_setttled <- dcast(cb_setttled_sum, win + eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="act_cb_amt")
act_cb_setttled[is.na(act_cb_setttled)] <- 0 
View(act_cb_setttled)

v0_proj_cb_setttled <- dcast(cb_setttled_sum, win + eats_flow ~ factor(J_mega_region, levels = mega_region_order), value.var="proj_cb_amt_v0")
v0_proj_cb_setttled[is.na(v0_proj_cb_setttled)] <- 0 
View(v0_proj_cb_setttled)



cb_start_dt
cb_end_dt
table(nb_cb_proj$marketplace)
max(nb_cb_proj$trip_date)
str(nb_cb_proj)
cb_nb_amt <- nb_cb_proj %>% 
  filter(!(mega_region %in% c('', 'China')), marketplace!='') %>% 
  mutate (trip_date = as.Date(trip_date), trip_month = as.Date(trip_month)) %>%
  filter( (trip_date >= cb_start_dt & trip_date < cb_end_dt)) %>% 
  mutate(J_marketplace = ifelse((u4b_flag %in% c('U4B','U4F') & marketplace =='personal_transport'), u4b_flag, marketplace)) %>%
# filter( (trip_date >= '2018-05-01' & trip_date < '2018-05-15') | (trip_date >= '2018-06-01' & trip_date < '2018-06-16')) %>% 
  group_by(J_marketplace, mega_region) %>% 
  summarise( client_net_fare_usd = sum(as.numeric(client_net_fare_usd), na.rm=T))


cb_nb_region_flow <- dcast(cb_nb_amt, J_marketplace ~ factor(mega_region, levels = mega_region_order), value.var="client_net_fare_usd")
View(cb_nb_region_flow)
table(cb_nb_amt$J_marketplace)

#################################################
####### DNS files sent to accounting ################
#################################################
## ## for 19 Jan Booking: baseline - (rides-Dec proj; Eats - Nov Actual)


current_start <- as.Date('2019-02-01')
current_end <- as.Date('2019-03-01')

current_month <- as.Date('2019-02-01')

base_start <- as.Date('2019-01-01')
base_end <- as.Date('2019-02-01')

eat_base_start <- as.Date('2018-11-01')
eat_base_end <- as.Date('2018-12-01')

nb_cur_csv <- read.csv(paste0(rider_path, updated_folder,"/Output_Jan19 _NetBookings.csv"), header=TRUE)
head(nb_cur_csv)
table(nb_cur_csv$mega_region)

nb_cur <- nb_cur_csv %>% mutate(mega_region=ifelse(mega_region=='ANZ', 'APACX', mega_region), client_net_fare_usd = as.numeric(gross_bills), trip_month=as.Date(trip_month))  %>%
  filter (client_net_fare_usd > 0, mega_region !='0')
View(nb_cur %>%  mutate(mega_region =  factor(mega_region, levels = mega_region_order)) 
     %>% group_by(eats_flow, mega_region)  %>% summarise(nb = sum(client_net_fare_usd)) )
nb_cur %>% group_by(eats_flow, trip_month) %>% summarise(nb = sum(client_net_fare_usd))
head(nb_cur)

# uncollected_project_raw_cur <- runHiveQuery("https://qb.uberinternal.com/querybuilder/builder/FoyiW8HsnF", date_start=current_start, date_end=current_end)
# saveRDS(uncollected_project_raw_cur, file = paste0(rider_path, updated_folder,"/uncollected_project_raw_cur",Sys.Date(),".rds"))

## uncollected_project_raw <- readRDS(paste0(rider_path, "2018/180415/uncollected_project_raw2019-01-30.rds"))

uncollected_project_raw_cur <- filter(uncollected_project_raw, trip_date >= current_start & trip_date < current_end)
max(uncollected_project_raw_cur$trip_date)
table(uncollected_project_raw_cur$mega_region)

uncollected_project_raw_base <- filter(uncollected_project_raw, trip_date >= base_start & trip_date < base_end)
uncollected_project_raw_base_eat <- filter(uncollected_project_raw, trip_date >= eat_base_start & trip_date < eat_base_end)

max(uncollected_project_raw_base$trip_date)
max(uncollected_project_raw_base_eat$trip_date)

project_start = as.Date(query_start)
project_end = as.Date(query_end)

#v1uc_baseline_curve_201701 <- readRDS(paste0(rider_path, "dns_baseline/v2uc_baseline_curve_2017012017-11-16.rds"))
v4uc_baseline_curve_201803_90 <- readRDS(paste0(rider_path, "dns_baseline/uc_baseline_curve_201803_90_2018-07-02.rds"))
head(v4uc_baseline_curve_201803_90)
# write.csv(v2uc_baseline_curve_201701,file =  paste0(rider_path,"dns_baseline/v2uc_baseline_curve_201701",Sys.Date(),".csv"))

f_uc_agg_mega <- function(uc_project_baseline){
  uc_project_aggregate <- uc_project_baseline %>%
    group_by() %>%
    mutate(trip_month = substr(trip_date,1,7)) %>%
    mutate(mega_region= ifelse(mega_region=='ANZ', 'APACX', mega_region)) %>%
    group_by(trip_month, mega_region) %>%
    #group_by(mega_region) %>%
    summarize(  unsettled_tot = sum(unsettled_tot, na.rm=T),
                uc_project_tot = sum(uc_project, na.rm=T),
                gross_bills_tot = sum(gross_bills_tot, na.rm=T))  %>%
    mutate(dns_actual_bps = unsettled_tot/gross_bills_tot * 10000,
           dns_proj_bps = uc_project_tot/gross_bills_tot * 10000) %>%
    mutate(mega_region =  factor(mega_region, levels = mega_region_order)) %>%
    arrange(trip_month,mega_region)

  return(uc_project_aggregate)
}

# uc_all_base <- f_uc_proj_region(uncollected_project_raw_aug1016, v4uc_baseline_curve_201803_90, region_sel = region_all, marketplace_sel = marketplace_all) 
uc_ex_eats_base <- f_uc_proj_region(uncollected_project_raw_base, v4uc_baseline_curve_201803_90, region_sel = region_all, marketplace_sel = c('personal_transport', 'U4B','U4F'))
uc_eats_base <- f_uc_proj_region(uncollected_project_raw_base_eat, v4uc_baseline_curve_201803_90, region_sel = region_all, marketplace_sel=c( 'agora', 'eats'))

# uc_ex_eats_base <- uc_project_ex_eats_12
# uc_eats_base <- uc_project_eats_12

View(t(f_uc_agg_mega(uc_ex_eats_base)))
View(t(f_uc_agg_mega(uc_eats_base)))

# uc_all_cur <- f_uc_proj_region(uncollected_project_raw_sep, v2uc_baseline_curve_201701, region_sel = region_all, marketplace_sel = marketplace_all) 
uc_project_ex_eats <- f_uc_proj_region(uncollected_project_raw_cur, v4uc_baseline_curve_201803_90, marketplace_sel = c('personal_transport', 'U4B','U4F'), region_sel = region_all)
uc_project_eats <- f_uc_proj_region(uncollected_project_raw_cur, v4uc_baseline_curve_201803_90, marketplace_sel =c('agora', 'eats'), region_sel = region_all)

View(t(f_uc_agg_mega(uc_project_ex_eats)))
View(t(f_uc_agg_mega(uc_project_eats)))

uc_ex_eats_cur <- uc_project_ex_eats %>% group_by(city_id) %>% 
  summarise(unsettled_tot=sum(unsettled_tot,na.rm=T),
            gross_bills_tot=sum(gross_bills_tot,na.rm=T))
uc_eats_cur <- uc_project_eats  %>% group_by(city_id) %>% 
  summarise(unsettled_tot=sum(unsettled_tot,na.rm=T),
            gross_bills_tot=sum(gross_bills_tot,na.rm=T))

# str(uc_ex_eats_cur)

current_month

# head(uc_ex_eats_base)
# head(nb_cur)
# str(nb_cur)
nb_ex_eats_cur <- nb_cur %>% 
  filter(!(mega_region %in% c('', 'China'))) %>% 
  mutate (trip_month = as.Date(trip_month), country_id = as.character(country_id), city_id = as.integer(city_id)) %>% 
  filter(trip_month == current_month) %>% 
  filter(eats_flow == 'N') %>% 
  group_by(trip_month, mega_region, country_id, city_id) %>% 
  summarise(base_net_fare_usd=sum(client_net_fare_usd,na.rm=T))

head(nb_ex_eats_cur)

nb_eats_cur <- nb_cur %>% 
  filter(!(mega_region %in% c('', 'China'))) %>% 
  mutate (trip_month = as.Date(trip_month), country_id = as.character(country_id), city_id = as.integer(city_id)) %>% 
  filter(trip_month == current_month) %>% 
  filter(eats_flow == 'Y') %>% 
  group_by(trip_month,mega_region, country_id, city_id) %>% 
  summarise(base_net_fare_usd=sum(client_net_fare_usd,na.rm=T))

head(nb_eats_cur)
str(nb_ex_eats_cur)

# write.csv(uc_ex_eats_cur,file =  paste0(rider_path,"1116/uc_ex_eats_cur",Sys.Date(),".csv"))
# write.csv(uc_ex_eats_base,file =  paste0(rider_path,"1116/uc_ex_eats_base",Sys.Date(),".csv"))    
# uc_all_base <- uc_ex_eats_base
# uc_all_cur <- uc_ex_eats_cur
# nb_all_cur <- nb_ex_eats_cur
# table(filter(uc_ex_eats_base, mega_region %in% ('India SA'))$country_id)

f_uc_export <- function(uc_all_base, uc_all_cur, nb_all_cur) {
  
  uc_all_base_city <- uc_all_base %>%  
    group_by(country_id) %>% 
    summarise(uc_actual_base = sum(unsettled_tot, na.rm=T),
              uc_project_base = sum(uc_project, na.rm=T),
              gross_bills_tot_base = sum(gross_bills_tot,na.rm=T)) %>% 
    #mutate(uc_project_bps =  uc_project_base/gross_bills_tot_base) %>%   ### use base month projection loss rate as current month projection
    #mutate(uc_project_bps =  uc_actual_base/gross_bills_tot_base) %>%   ### use base month actual loss rate as current month projection
    ## mutate(uc_project_bps = ifelse(country_id %in% c(15,163,77), uc_project_base, uc_actual_base)/gross_bills_tot_base) %>%  ### India using projection; other region using base month actual loss rate as current month projection
    select(country_id, uc_project_base, gross_bills_tot_base, uc_project_bps) %>% 
    arrange(country_id)
  
  head(uc_all_base_city)
  
  uc_all_cur_act <- nb_all_cur  %>% 
    left_join(uc_all_cur , by = c("city_id")) %>%    
    # filter (trip_date >= as.Date('2017-08-01'), trip_date < as.Date('2017-09-01')) %>% 
    select(mega_region, country_id, city_id, trip_month, unsettled_tot, base_net_fare_usd, gross_bills_tot) %>%
    rename(dns_actual = unsettled_tot) %>%
    arrange(country_id, city_id)
  
  head(uc_all_cur_act)
  ##sum(uc_all_cur_act$dns_actual)
  
  uc_all_proj <- uc_all_cur_act %>%  
    ## full_join(uc_all_base_city, by = c("country_id", "city_id")) %>% 
    left_join(uc_all_base_city, by = c("country_id")) %>% 
    mutate(dns_project = ifelse(is.na(uc_project_bps),dns_actual, base_net_fare_usd * uc_project_bps),
           dns_actual = ifelse(is.na(dns_actual),0,dns_actual)) %>% 
    select(trip_month, mega_region, country_id, city_id,  dns_actual, dns_project, base_net_fare_usd, uc_project_bps)
  # head(uc_all_proj)
  return(uc_all_proj)
}

uc_ex_eats_proj <- f_uc_export(uc_ex_eats_base, uc_ex_eats_cur, nb_ex_eats_cur)
# write.csv(uc_ex_eats_proj,file =  paste0(rider_path,"to_accounting/dns_ex_eats_",Sys.Date(),".csv"))
uc_eats_proj <- f_uc_export(uc_eats_base, uc_eats_cur, nb_eats_cur)

uc_ex_eats_proj["Eats_flow"] <- "N"
uc_eats_proj["Eats_flow"] <- "Y"
uc_all_flow_proj <- rbind(uc_ex_eats_proj,uc_eats_proj)

uc_all_flow_proj_out <- uc_all_flow_proj %>% filter (!is.na(mega_region))
nrow(uc_all_flow_proj_out)
head(uc_all_flow_proj_out)
write.csv(uc_all_flow_proj_out,file =  paste0(rider_path,"to_accounting/dns_all_",Sys.Date(),".csv"))


uc_all_proj_sum <- uc_all_flow_proj %>% 
  group_by(Eats_flow, mega_region) %>% 
  summarise(dns_actual = sum(dns_actual,na.rm=T),
            dns_project = sum(dns_project,na.rm=T),
            ##dns_proj = sum(dns_proj,na.rm=T),
            base_net_fare_usd = sum(base_net_fare_usd,na.rm=T)) %>% 
  mutate(dns_actual_bps = dns_actual/base_net_fare_usd * 10000,
         dns_proj_bps = dns_project/base_net_fare_usd * 10000) %>% 
  mutate(mega_region =  factor(mega_region, levels = mega_region_order), base_net_fare_usd=format(base_net_fare_usd, scientific=F)) %>%
  arrange(Eats_flow, mega_region)

View(uc_all_proj_sum)

######################################################
#### cb files sent to accounting
#####################################################
# for 19-Jan booking - base: Rides/Eats -  '2018-12-16' ~ '2019-01-16' projected CB 

current_month=as.Date('2019-02-01')
rides_cb_base_start = as.Date('2019-01-01')
rides_cb_base_end = as.Date('2019-02-01')

eats_cb_base_start = as.Date('2019-01-15')
eats_cb_base_end = as.Date('2019-02-15')

cb_nb <- filter(nb_cb_proj, marketplace!='ubercash')
cb_net_base <-  cb_net %>%   ####readRDS(paste0(rider_path, updated_folder,"/cb_net2019-02-01.rds")) %>%  
                rename (f_trip_month = trip_month) %>%
                mutate(eats_flow = ifelse(eats_flow %in% c('U4B','U4F'), 'N', eats_flow), mega_region=ifelse(mega_region=='ANZ', 'APACX', mega_region)) %>%
                filter(eats_flow != 'Z')                
cb_settled_base <- cb_setttled  %>% ####readRDS(paste0(rider_path, updated_folder,"/cb_setttled2019-02-01.rds"))  %>%  
                rename (f_trip_month = trip_month) %>%
                mutate(eats_flow = ifelse(eats_flow %in% c('U4B','U4F'), 'N', eats_flow), mega_region=ifelse(mega_region=='ANZ', 'APACX', mega_region)) %>%
                filter(eats_flow != 'Z')

# cb_base_start <- rides_cb_base_start
# cb_base_end <- rides_cb_base_end

f_cb_base_bps <-function(cb_nb, cb_net_base, cb_settled_base, perc, cb_base_start, cb_base_end) {
  
  nb_cb_base <- cb_nb %>%
    filter(!(mega_region %in% c('', 'China')), client_net_fare_usd > 0) %>%
    mutate (mega_region=ifelse(mega_region=='ANZ', 'APACX', mega_region), ##trip_date = as.Date(trip_date), 
            trip_month = as.Date(trip_month), country_id = as.integer(country_id), city_id = as.integer(city_id)) %>%
    filter((trip_date >= cb_base_start & trip_date < cb_base_end)) %>% 
    group_by(eats_flow,mega_region,country_id,city_id) %>%
    summarise( base_net_fare_usd=sum(as.numeric(client_net_fare_usd),na.rm=T))
  
  cb_cur_base <- cb_net_base %>% 
    mutate (charge_date = as.Date(charge_date), charge_month = as.Date(charge_month), country_id = as.integer(country_id), city_id = as.integer(city_id)) %>% 
    filter( (charge_date >= cb_base_start & charge_date < cb_base_end)) %>% 
    # filter((charge_date>=as.Date('2017-06-04') & charge_date < as.Date('2017-06-18')) | (charge_date>=as.Date('2017-07-02') & charge_date < as.Date('2017-07-16'))) %>% 
    rename (trip_date = charge_date, trip_month = charge_month) %>% 
    group_by (city_id, country_id, mega_region, eats_flow) %>% 
    summarize(est_cb_amt = sum(est_cb_amt,na.rm=T),
              act_cb_amt = sum(act_cb_amt,na.rm=T)) 
  
  cb_cur_base  %>% group_by(eats_flow) %>% summarize(est_cb_amt = sum(est_cb_amt,na.rm=T), act_cb_amt = sum(act_cb_amt,na.rm=T)) 
  
  cb_win_base <- cb_settled_base %>% 
    mutate (charge_date = as.Date(charge_date), charge_month = as.Date(charge_month), country_id = as.integer(country_id), city_id = as.integer(city_id)) %>% 
    filter(win==1) %>%  ########?????
    filter( (charge_date >= cb_start_dt & charge_date < cb_end_dt)) %>% 
    # filter((charge_date>=as.Date('2017-06-04') & charge_date < as.Date('2017-06-18')) | (charge_date>=as.Date('2017-07-02') & charge_date < as.Date('2017-07-16'))) %>% 
    rename (trip_date = charge_date, trip_month = charge_month) %>% 
    group_by (city_id, country_id, mega_region, eats_flow) %>% 
    summarize(est_cb_amt = sum(est_cb_amt,na.rm=T),
              act_cb_amt = sum(act_cb_amt,na.rm=T)) 
  
  
   cb_win_base  %>% group_by(eats_flow) %>% summarize(est_cb_amt = sum(est_cb_amt,na.rm=T),act_cb_amt = sum(act_cb_amt,na.rm=T)) 
  
   perc=0.8
  cb_final <- cb_cur_base %>% 
    left_join(cb_win_base, by = c("city_id","country_id", "mega_region", "eats_flow"), suffix=c("_cur","_win")) %>% 
    mutate(
      est_cb_amt_win = ifelse(is.na(est_cb_amt_win), 0, est_cb_amt_win),
      act_cb_amt_win = ifelse(is.na(act_cb_amt_win), 0, act_cb_amt_win),
      est_cb_amt_final = ifelse(is.na(est_cb_amt_cur), 0, est_cb_amt_cur) + est_cb_amt_win - perc*act_cb_amt_win)
  
  cb_base_city <- nb_cb_base %>% 
    left_join(cb_final, by=c("city_id","country_id", "mega_region", "eats_flow")) %>% 
    group_by(eats_flow,mega_region,country_id,city_id) %>% 
    summarise(act_cb_amt_cur=sum(act_cb_amt_cur),
              est_cb_amt_final=sum(est_cb_amt_final),
              base_net_fare_usd=sum(base_net_fare_usd,na.rm=T),
              est_cb_bps = est_cb_amt_final/base_net_fare_usd)
  
  cb_base_city  %>% group_by(eats_flow) %>% summarize(est_cb_amt_final = sum(est_cb_amt_final,na.rm=T)) 
  
  return(cb_base_city)
}

cb_base_city_rides <- f_cb_base_bps(cb_nb, cb_net_base, cb_settled_base, 0.8, rides_cb_base_start, rides_cb_base_end)
cb_base_city_eats <- f_cb_base_bps(cb_nb, cb_net_base, cb_settled_base, 0.8, eats_cb_base_start, eats_cb_base_end) 

cb_base_city <-rbind(filter(cb_base_city_rides, eats_flow =='N'), filter(cb_base_city_eats, eats_flow=='Y'))
 #head(cb_base_city_rides)

base_cb_sum <- cb_base_city %>% 
  group_by(eats_flow,mega_region) %>%
  # filter(Eats_flow =='Y') %>% 
  summarize(net_booking = sum(base_net_fare_usd*1,na.rm=T),
            proj_cb_amt = sum(est_cb_amt_final,na.rm=T),
            act_cb_amt = sum(act_cb_amt_cur, na.rm=T)) %>% 
  mutate(proj_cb_bps = proj_cb_amt/net_booking * 10000) %>% 
  mutate(mega_region =  factor(mega_region, levels = mega_region_order)) %>% 
  arrange(eats_flow, mega_region)
View(base_cb_sum)

# nb_cb_proj<- runVerticaQuery("https://qb.uberinternal.com/querybuilder/builder/HGTNpLCNUX", start_date= '2017-07-01', end_date= '2017-08-01')
# saveRDS(nb_cb_proj, file = paste0(rider_path, updated_folder,"/nb_cb_proj",Sys.Date(),".rds"))

# cb_raw <- readRDS(paste0(rider_path, "0928","/cb_raw2017-10-02.rds"))

head(nb_cur)
nb_cb_cur <- nb_cur %>% 
  filter(!(mega_region %in% c('', 'China','0')), client_net_fare_usd>0) %>% 
  mutate (mega_region=ifelse(mega_region=='ANZ', 'APACX', mega_region), trip_month = as.Date(trip_month),country_id = as.integer(country_id), city_id = as.integer(city_id)) %>% 
  filter(trip_month == current_month) %>% 
  group_by(trip_month, eats_flow, mega_region, country_id,city_id) %>% 
  summarise( client_net_fare_usd=sum(client_net_fare_usd,na.rm=T)) 

table(nb_cb_cur$mega_region)
head(cb_net)
current_month
cb_act <- cb_net %>% 
  filter(charge_month == current_month) %>% 
  mutate(charge_date = as.Date(charge_date), charge_month = as.Date(charge_month), country_id = as.integer(country_id), city_id = as.integer(city_id)) %>% 
  rename (f_trip_month = trip_month, trip_date = charge_date, trip_month = charge_month) %>% 
  group_by(trip_month,eats_flow,mega_region, country_id,city_id) %>% 
  summarise( act_cb_amt=sum(act_cb_amt,na.rm=T),
             est_cb_amt=sum(est_cb_amt,na.rm=T))  

head(cb_act)
# str(cb_base_city)
# str(nb_cb_cur)

cb_sum_city <- nb_cb_cur %>% 
  left_join(cb_act, by = c("trip_month","eats_flow","mega_region","country_id","city_id")) %>%
  mutate(act_cb_amt_usd = ifelse(is.na(act_cb_amt),0,act_cb_amt),
         est_cb_amt_raw = ifelse(is.na(est_cb_amt),0,est_cb_amt)) %>%
  left_join(cb_base_city, by = c("eats_flow","mega_region","country_id","city_id")) %>% 
  mutate(est_cb_amt_usd_orig = ifelse(is.na(est_cb_bps),0,est_cb_bps) * client_net_fare_usd,
         est_cb_amt_usd = ifelse(est_cb_amt_usd_orig==0, est_cb_amt_raw,  est_cb_amt_usd_orig),
         est_cb_bps_city = ifelse(is.na(est_cb_bps),0,est_cb_bps))  %>% 
  select(trip_month, eats_flow,mega_region, country_id,city_id, client_net_fare_usd, est_cb_amt_usd, act_cb_amt_usd, est_cb_bps_city, est_cb_amt_usd_orig)

head(cb_sum_city)

cb_sum_city_out <- cb_sum_city %>% 
  select(trip_month, eats_flow, mega_region, country_id,city_id, client_net_fare_usd, est_cb_amt_usd, act_cb_amt_usd, est_cb_bps_city)
write.csv(cb_sum_city_out,file =  paste0(rider_path,"to_accounting/cb_sum_city_",Sys.Date(),".csv"))

proj_cb_sum <- cb_sum_city %>% 
  # filter(Eats_flow =='Y') %>% 
  group_by(eats_flow, mega_region) %>%
  summarize(net_booking = sum(client_net_fare_usd,na.rm=T),
            act_cb_amt = sum(act_cb_amt_usd, na.rm=T),
            proj_cb_amt = sum(est_cb_amt_usd,na.rm=T),
            proj_cb_amt_orig = sum(est_cb_amt_usd_orig,na.rm=T)
  ) %>% 
  mutate(proj_cb_bps = proj_cb_amt/net_booking * 10000,
         proj_cb_bps_orig = proj_cb_amt_orig/net_booking * 10000) %>% 
  mutate(mega_region =  factor(mega_region, levels = mega_region_order)) %>%
  arrange(eats_flow, mega_region)
View(proj_cb_sum)



