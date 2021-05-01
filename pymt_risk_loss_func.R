## source ("/Users/guoyu.zhu/Documents/code_lib/R/init_setting.r")

### Set filepaths
# rider_path <- "/Users/guoyu.zhu/Documents/lossforecast/monthlyLoss/"
# 
# f_uc_baseline - calculated uc baseline curve for projection
# f_uc_baseline - parameters
# cohort_raw
# baseline_start = as.Date('2016-06-01')
# baseline_end = as.Date('2016-07-01')
# maturity_day = 120
# ex. uc_baseline_curve_201606 <- f_uc_baseline(cohort_raw, as.Date('2016-06-01'), as.Date('2016-07-01'), 120)


# f_uc_proj_region - aggregate the actual/projected UC by region by month
# f_uc_proj_region - Parameters
# uncollected_project_raw
# uc_baseline_curve
# project_start = as.Date('2016-11-01')
# project_end = as.Date('2017-07-01')
# ex. f_uc_proj_region(uncollected_project_raw, uc_baseline_curve_201606, marketplace_sel, region_sel)

# region_sel <- c('EMEA', 'India', 'LatAm', 'SENA', 'US & Canada')
# marketplace_sel <- c('personal_transport', 'agora')

# region_order <- c('US & Canada',  'EMEA',  'LatAm',  'SENA',  'ANZ',  'India')

# cohort_raw <- uc_baseline_raw_16jun
# baseline_start <-  as.Date('2016-06-01')
# baseline_end <- as.Date('2016-07-01')
# maturity_day <- 120

f_uc_baseline <- function(cohort_raw, baseline_start, baseline_end, maturity_day ) {
  cohort <- cohort_raw %>%
    ###filter(mega_region != 'China', recent_bill_date != '\\N') %>%
    filter(mega_region != 'China', !is.na(recent_bill_date)) %>%
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
  
  uc_baseline <- 
    cohort %>%
    # Assume all chargebacks have arrived by 120 days
    filter(  trip_date >= baseline_start, trip_date < baseline_end, days_past <= maturity_day) %>%
    group_by(mega_region, region, days_past, country_id) %>%
    summarize(  # Calculate totals for each country and group of number of days between bill attempt and trip
      unsettled_tot = sum(unsettled_tot, na.rm=T),
      trip_tot = sum(trip_ct, na.rm=T),
      gross_bills = sum(gross_bills, na.rm=T),
      gross_fares = sum(abs(gross_fares), na.rm=T),
      unsettled_trip_tot = sum(unsettled_trip_ct, na.rm=T))
  
  head(uc_baseline)
  
  uc_baseline_total <- 
    uc_baseline %>%
    group_by(mega_region, region, country_id) %>%
    summarize(  # Calculate totals
      gross_bills_total = sum(gross_bills, na.rm=T),
      gross_fares_total = sum(gross_fares, na.rm=T),
      trips_total = sum(trip_tot, na.rm=T),
      unsettled_total = sum(unsettled_tot, na.rm=T),
      unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T)) %>%
    rename(unsettled_all = unsettled_total, unsettled_trip_all = unsettled_trip_tot) %>%
    select(mega_region, region, country_id, unsettled_all, unsettled_trip_all, gross_bills_total, gross_fares_total)
  
  head(uc_baseline_total)
  
  uc_baseline_join <- 
    uc_baseline %>% 
    left_join(uc_baseline_total, by = c('mega_region','region', 'country_id')) 
  
  uc_cumulative <- 
    uc_baseline_join %>% 
    group_by(mega_region, region, country_id) %>% 
    arrange(days_past) %>%
    mutate( unsettled_cumulative = cumsum(ifelse(is.na(unsettled_tot), 0, unsettled_tot)),
            unsettled_trip_cumulative = cumsum(ifelse(is.na(unsettled_trip_tot), 0, unsettled_trip_tot))) %>%
    arrange(desc(days_past)) %>%
    group_by(mega_region, region, country_id) %>%
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
  
  head(uc_baseline_pct)
  
  days_past_all <- data.frame(days_past = 0:maturity_day, join_key = 1)
  
  baseline_skeleton <- uc_baseline_pct %>%
    group_by() %>%
    distinct(country_id) %>%
    mutate(join_key = 1) %>%
    inner_join(days_past_all, by = c('join_key')) %>%
    select(country_id, days_past)
  
  head(baseline_skeleton)
  
  uc_baseline_all <- baseline_skeleton %>%
    left_join(uc_baseline_pct, by = c('days_past', 'country_id')) %>%
    arrange(country_id, days_past) %>%
    mutate(  mega_region = na.locf(mega_region),
             region = na.locf(region),
             unsettled_baseline_pct = na.locf(unsettled_baseline_pct),
             unsettled_trip_baseline_pct = na.locf(unsettled_trip_baseline_pct)
    )
  
   uc_baseline_curve <- 
    uc_baseline_all %>%
    select(mega_region, region, country_id, days_past, unsettled_baseline_pct) %>%
    arrange(days_past)
  
  uc_baseline_curve_export <-
    uc_baseline_all %>%
    group_by() %>% 
    mutate(country_id = as.numeric(country_id)) %>%
    select(mega_region, region, country_id, days_past, unsettled_baseline_pct, unsettled_trip_baseline_pct) %>%
    rename(fare_survival_frac = unsettled_baseline_pct, trip_survival_frac = unsettled_trip_baseline_pct) %>%
    filter(fare_survival_frac > 0, !is.na(country_id)) %>%
    arrange(country_id, days_past) 
  
  head(uc_baseline_curve_export)
  
  write.csv(uc_baseline_curve_export,file =  paste0(rider_path,"uc_baseline_curve_export",baseline_start,"_",maturity_day, ".csv"))
  
  # write.csv(uc_baseline_curve_export,file =  paste0(rider_path,"uc_baseline_curve_export_201606.csv"))
  # write.csv(uc_baseline_curve_export,file =  paste0(rider_path,"uc_baseline_curve_export_201701.csv"))
  
  return(uc_baseline_curve)
}

fv2_uc_baseline <- function(cohort_raw, baseline_start, baseline_end, maturity_day ) {
  
  # v2uc_baseline_raw_jan <- readRDS(paste0(rider_path, "dns_baseline/v2uc_baseline_raw_jan2017-08-21.rds"))
  # v2uc_baseline_raw_16jun <- readRDS(paste0(rider_path, "dns_baseline/v2uc_baseline_raw_16jun2017-08-30.rds"))
  # cohort_raw <- v2uc_baseline_raw_jan
  # head(cohort_raw)
  # baseline_start <- as.Date('2017-01-01')
  # baseline_end <- as.Date('2017-02-01')
  # maturity_day <- 120
  
  cohort <- cohort_raw %>%
    ###filter(mega_region != 'China', recent_bill_date != '\\N') %>%
    ## filter(mega_region != 'China', !is.na(recent_bill_date), cancellation_flag  %in% c('false')) %>%
    filter(!is.na(recent_bill_date)) %>%
    
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
  
  uc_baseline <- 
    cohort %>%
    # Assume all chargebacks have arrived by 120 days
    filter(  trip_date >= baseline_start, trip_date < baseline_end, days_past <= maturity_day) %>%
    group_by(days_past, country_id) %>%
    # group_by( mega_region, region, days_past ) %>%
    summarize(  # Calculate totals for each country and group of number of days between bill attempt and trip
      unsettled_tot = sum(unsettled_tot, na.rm=T),
      settled_tot = sum(settled_tot, na.rm=T),
      bills_tot = sum(unsettled_tot+settled_tot),
      gross_bills = sum(gross_bills, na.rm=T),
      gross_fares = sum(abs(gross_fares), na.rm=T),
      
      unsettled_trip_tot = sum(unsettled_trip_ct, na.rm=T),
      settled_trip_tot = sum(settled_trip_ct, na.rm=T),
      billed_trip_tot = sum(unsettled_trip_ct+settled_trip_ct),
      trip_tot = sum(trip_ct, na.rm=T))
  
  head(uc_baseline)
  
  uc_baseline_total <- 
    uc_baseline %>%
    group_by(country_id) %>%
    summarize(  # Calculate totals
      gross_bills_total = sum(gross_bills, na.rm=T),
      gross_fares_total = sum(gross_fares, na.rm=T),
      grand_bills_tot = sum(bills_tot, na.rm=T),
      
      trips_total = sum(trip_tot, na.rm=T),
      grand_trips_tot = sum(billed_trip_tot, na.rm=T),
      unsettled_total = sum(unsettled_tot, na.rm=T),
      unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T)) %>%
    rename(unsettled_all = unsettled_total, unsettled_trip_all = unsettled_trip_tot)
  # select(mega_region, region, country_id, unsettled_all, unsettled_trip_all, gross_bills_total, gross_fares_total)
  # mutate(join_key = 1)
  
  head(uc_baseline_total)
  
  uc_baseline_join <- 
    uc_baseline %>%  ### mutate(join_key = 1) %>%
    left_join(uc_baseline_total, by = c('country_id'))   ###,by = c('mega_region','region','join_key'))
  
  head(uc_baseline_join)
  
  uc_cumulative <- 
    uc_baseline_join %>% 
    group_by(country_id) %>% 
    arrange(days_past) %>%
    mutate( unsettled_cumulative = cumsum(ifelse(is.na(unsettled_tot), 0, unsettled_tot)),
            settled_cumulative = cumsum(ifelse(is.na(settled_tot), 0, settled_tot)),
            unsettled_amt = grand_bills_tot -settled_cumulative,
            
            unsettled_trip_cumulative = cumsum(ifelse(is.na(unsettled_trip_tot), 0, unsettled_trip_tot)),
            settled_trip_cumulative = cumsum(ifelse(is.na(settled_trip_tot), 0, settled_trip_tot)),
            unsettled_trips = grand_trips_tot -settled_trip_cumulative)
  
  head(uc_cumulative)
  
  uc_baseline_pct <- 
    uc_cumulative %>% 
    mutate( unsettled_fare_bps = (unsettled_amt / gross_bills_total) * 10000,
            unsettled_baseline_pct = ifelse(ifelse(is.finite(unsettled_all / unsettled_amt), unsettled_all / unsettled_amt, 0) > 1,
                                            1, ifelse(is.finite(unsettled_all / unsettled_amt), unsettled_all / unsettled_amt, 0)),
            unsettled_trip_baseline_pct = ifelse(ifelse(is.finite(unsettled_trip_all / unsettled_trips), unsettled_trip_all / unsettled_trips, 0) > 1,
                                                 1, ifelse(is.finite(unsettled_trip_all / unsettled_trips), unsettled_trip_all / unsettled_trips, 0))) %>%
    arrange(days_past)
  
  head(uc_baseline_pct) 
  ##write.csv(uc_baseline_pct,file =  paste0(rider_path,"dns_baseline/uc_baseline_pct",baseline_start,"_",maturity_day, ".csv"))
  
  
  days_past_all <- data.frame(days_past = 0:maturity_day, join_key = 1)
  
  baseline_skeleton <- uc_baseline_pct %>%
    group_by() %>%
    distinct(country_id) %>%
    mutate(join_key = 1) %>%
    inner_join(days_past_all, by = c('join_key')) %>%
    select(country_id, days_past)
  
  head(baseline_skeleton)
  
  uc_baseline_all <- baseline_skeleton %>%
    left_join(uc_baseline_pct, by = c('days_past', 'country_id')) %>%
    arrange(country_id, days_past) %>%
    mutate( # mega_region = na.locf(mega_region),
      # region = na.locf(region),
      unsettled_baseline_pct = na.locf(unsettled_baseline_pct),
      unsettled_trip_baseline_pct = na.locf(unsettled_trip_baseline_pct)
    )
  head(uc_baseline_all)
  
  uc_baseline_curve <- 
    uc_baseline_all %>%
    select(country_id, days_past, unsettled_baseline_pct) %>%
    arrange(days_past)
  
  uc_baseline_curve_export <-
    uc_baseline_all %>%
    group_by() %>% 
    mutate(country_id = as.numeric(country_id)) %>%
    select(country_id, days_past, unsettled_baseline_pct, unsettled_trip_baseline_pct) %>%
    rename(fare_survival_frac = unsettled_baseline_pct, trip_survival_frac = unsettled_trip_baseline_pct) %>%
    filter(fare_survival_frac > 0, !is.na(country_id)) %>%
    arrange(country_id, days_past) 
  
  head(uc_baseline_curve_export)
  write.csv(uc_baseline_curve_export,file =  paste0(rider_path,"dns_baseline/uc_baseline_curve_export",baseline_start,"_",maturity_day, ".csv"))
  
  return(uc_baseline_curve)
}

f_uc_proj_region <- function(uncollected_project_raw, uc_baseline_curve, marketplace_sel, region_sel) {
  # uncollected_project_raw <- uncollected_project_raw
  # uc_baseline_curve <-  v4uc_baseline_curve_201803_90
  # marketplace_sel <- marketplace_all
  # region_sel <- region_all

  uc_uncollected <- uncollected_project_raw %>%
    #filter(recent_bill_date != '') %>%
    mutate( trip_date = as.Date(trip_date),
            recent_bill_date = as.Date(recent_bill_date), 
            #week = as.numeric(format(trip_date, "%V")) %% 53 + (as.numeric(format(trip_date, "%Y")) - 2015) * 53,
           # month = as.numeric(format(trip_date, "%m")) + (as.numeric(format(trip_date, "%Y")) - 2015) * 12,
            country_id = as.character(country_id),
            gross_bills = as.numeric(gross_bills))
  
  # head(uc_uncollected)
  
  uc_project <- 
    uc_uncollected %>%
    # group_by(mega_region, region, country_id, city_id, trip_date, marketplace, cancellation_flag) %>%
    # # Assume all chargebacks have arrived by 120 days
    # filter(  trip_date >= project_start, trip_date < project_end, days_past == max(days_past), cancellation_flag %in% c('false'), 
    #          marketplace %in% marketplace_sel, mega_region %in% region_sel) %>%  
    group_by(mega_region, region, country_id, city_id, trip_date, marketplace) %>%
    filter(  trip_date >= project_start, trip_date < project_end, days_past == max(days_past), marketplace %in% marketplace_sel, mega_region %in% region_sel) %>% 
    group_by(marketplace, mega_region, region, country_id, city_id, trip_date) %>%
    summarize(  # Calculate totals  for each token_type and group of number of days between bill attempt and trip
      days_past = max(days_past),
      unsettled_tot = sum(unsettled_tot, na.rm=T),
      trip_tot = sum(trip_ct, na.rm=T),
      gross_bills = sum(gross_bills, na.rm=T),
     ## gross_fares = sum(abs(gross_fares), na.rm=T),
      unsettled_trip_tot = sum(unsettled_trip_tot, na.rm=T))
  
 # table(uc_project$mega_region)
  
  uc_project_fares <-
    uc_uncollected %>%
    filter(  trip_date >= project_start, trip_date < project_end , ### cancellation_flag %in% c('false'),
             marketplace %in% marketplace_sel, mega_region %in% region_sel) %>%
    group_by(marketplace, mega_region, region, country_id, city_id, trip_date) %>%
    summarize(  gross_bills_tot = sum(abs(gross_bills), na.rm=T),
               ## gross_fares_tot = sum(abs(gross_fares), na.rm=T),
                trip_all = sum(trip_ct, na.rm=T))
  
  #table(uc_project_fares$mega_region)
  uc_project_all <- 
    uc_project %>% 
    inner_join(uc_project_fares, by = c('marketplace', 'mega_region', 'region', 'country_id', 'city_id', 'trip_date')) %>%
    mutate(unsettled_fare_bps = unsettled_tot / gross_bills_tot * 10000) 
  
  
  #table(uc_project_all$mega_region)
  
  uc_project_baseline <-
    uc_project_all %>%  
    left_join(uc_baseline_curve, by = c('days_past', 'country_id')) %>%
    # left_join(uc_baseline_curve, by = c( 'mega_region','region','days_past', 'country_id')) %>%
    mutate(  uc_project = ifelse(is.na(unsettled_baseline_pct), unsettled_tot, unsettled_tot * unsettled_baseline_pct),
             uc_project_bps = uc_project / gross_bills_tot * 10000,
             uc_bps = unsettled_tot / gross_bills_tot * 10000) 
  
  #table(uc_project_baseline$mega_region)
  return(uc_project_baseline)
}


f_dedup <- function (uc_fbt) {
  d_uc_fbt <- uc_fbt %>% 
    group_by(trip_uuid) %>%
    arrange(trip_uuid, desc(abs(amount_usd))) %>% 
    filter(row_number(trip_uuid) == 1)
  return(d_uc_fbt)
}

f_crosscheck <- function(file1, file2) {
  dns_ftb_0927_only <- dns_ftb_0927 %>%
    anti_join(dns_ftb_1011,  c('uuid', 'trip_uuid', 'order_id')) %>%
    as.data.frame()
  
  cb_new_txn_06_only <- cb_new_txn_06 %>%
    anti_join(cb_new_setttled_txn_06,  c('uuid' = 'trip_uuid')) %>%
    as.data.frame()
}
# trip_list <- paste("('",paste(head(uc_fbt3_mar_stl_1$trip_uuid,10),collapse = "','"),"')", sep = "")
# trip_list

##saveRDS(cb_raw, file = paste0(rider_path, updated_folder,"/cb_raw",Sys.Date(),".rds")) ## 603,418
##cb_raw_0801 <- readRDS(paste0(rider_path,"0731/cb_raw2017-08-01.rds"))

## cb_agg <- read.csv(paste0(rider_path, updated_folder,"/cb_agg_2018-08-04.csv"), head=TRUE, sep=",")
## write.csv(tmd_sum_reg_all,file =  paste0(rider_path,updated_folder,"/tmd_sum_reg_all",Sys.Date(),".csv"))
