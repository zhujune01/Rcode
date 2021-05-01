source ("/Users/guoyu.zhu/Documents/code_lib/R/init_setting.r")
source ("/Users/guoyu.zhu/Documents/code_lib/R/pymt_risk_loss_func.R")

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
options(dplyr.print_max = Inf)

################################################

region_order <- c('US & Canada',  'EMEA',  'LatAm',  'SENA',  'ANZ',  'India')
# region_all <- c('EMEA', 'India', 'LatAm', 'SENA', 'US & Canada')
# marketplace_all <- c('personal_transport', 'agora')
# region_ex_india <- c('EMEA', 'LatAm', 'SENA', 'US & Canada')

region_all <- c('EMEA', 'India', 'LatAm', 'SENA', 'US & Canada', 'ANZ')
marketplace_all <- c('personal_transport', 'agora','eats','rush')

region_ex_india <- c('EMEA', 'LatAm', 'SENA', 'US & Canada', 'ANZ')
marketplace_ex_eats <- c('personal_transport')
marketplace_eats <- c('agora')

updated_folder <- '0831' ###  0717

uc_baseline_curve_201606 <- readRDS(paste0(rider_path,"uc_baseline_curve_2016062017-07-12.rds"))
uc_baseline_curve_201701 <- readRDS(paste0(rider_path,"uc_baseline_curve_2017012017-07-12.rds"))

write.csv(uc_baseline_curve_201606,file =  paste0(rider_path,"uc_baseline_curve_201606_120.csv"))
write.csv(uc_baseline_curve_201701,file =  paste0(rider_path,"uc_baseline_curve_201701_120.csv"))


uc_cohort_16jun <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2016-06-01', date_end='2016-07-01')
saveRDS(uc_cohort_16jun, file = paste0(rider_path, "0717/uc_cohort_16jun",Sys.Date(),".rds"))
max(uc_cohort_16jun$recent_bill_date)

uc_cohort_16oct <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2016-10-01', date_end='2016-11-01')
saveRDS(uc_cohort_16oct, file = paste0(rider_path, "0717/uc_cohort_16oct",Sys.Date(),".rds"))
head(uc_cohort_16oct)
max(uc_cohort_16oct$days_past)
max(uc_cohort_16oct$recent_bill_date)

uc_cohort_16nov <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2016-11-01', date_end='2016-12-01')
saveRDS(uc_cohort_16nov, file = paste0(rider_path, "0717/uc_cohort_16nov",Sys.Date(),".rds"))

uc_cohort_16dec <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2016-12-01', date_end='2017-01-01')
saveRDS(uc_cohort_16dec, file = paste0(rider_path, "0717/uc_cohort_16dec",Sys.Date(),".rds"))

uc_cohort_jan <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2017-01-01', date_end='2017-02-01')
saveRDS(uc_cohort_jan, file = paste0(rider_path, "0717/uc_cohort_jan",Sys.Date(),".rds"))

uc_cohort_16jun <- readRDS(paste0(rider_path,"0717/uc_cohort_16jun2017-07-20.rds"))
uc_cohort_16nov <- readRDS(paste0(rider_path,"0717/uc_cohort_16nov2017-07-20.rds"))
uc_cohort_jan <- readRDS(paste0(rider_path,"0717/uc_cohort_jan2017-07-20.rds"))


uc_cohort_feb <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2017-02-01', date_end='2017-03-01')
saveRDS(uc_cohort_feb, file = paste0(rider_path, updated_folder,"/uc_cohort_feb",Sys.Date(),".rds"))
head(uc_cohort_feb)

uc_cohort_mar <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2017-03-01', date_end='2017-04-01')
saveRDS(uc_cohort_mar, file = paste0(rider_path, updated_folder,"/uc_cohort_mar",Sys.Date(),".rds"))

uc_cohort_apr <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/s7F5sJlnfo", date_start='2017-04-01', date_end='2017-05-01')
saveRDS(uc_cohort_apr, file = paste0(rider_path, updated_folder,"/uc_cohort_apr",Sys.Date(),".rds"))

### https://qb.uberinternal.com/querybuilder/builder/lqwTtoJGlh
uc_cohort_may <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/IPtD9vmrMR", date_start='2017-05-01', date_end='2017-06-01')
saveRDS(uc_cohort_may, file = paste0(rider_path, updated_folder,"/uc_cohort_may",Sys.Date(),".rds"))

uc_cohort_jun <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/IPtD9vmrMR", date_start='2017-06-01', date_end='2017-07-01')
saveRDS(uc_cohort_jun, file = paste0(rider_path, updated_folder,"/uc_cohort_jun",Sys.Date(),".rds"))

uc_cohort_jul <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/IPtD9vmrMR", date_start='2017-07-01', date_end='2017-08-01')
saveRDS(uc_cohort_jul, file = paste0(rider_path, updated_folder,"/uc_cohort_jul",Sys.Date(),".rds"))

uc_cohort_aug <- runPrestoQuery("https://qb.uberinternal.com/querybuilder/builder/IPtD9vmrMR", date_start='2017-08-01', date_end='2017-09-01')
saveRDS(uc_cohort_aug, file = paste0(rider_path, updated_folder,"/uc_cohort_aug",Sys.Date(),".rds"))

uc_cohort_aug <- readRDS(paste0(rider_path,"0831/uc_cohort_aug2017-08-31.rds"))
uc_cohort_jul <- readRDS(paste0(rider_path,"0831/uc_cohort_jul2017-08-31.rds"))
uc_cohort_jun <- readRDS(paste0(rider_path,"0831/uc_cohort_jun2017-08-30.rds"))
uc_cohort_may <- readRDS(paste0(rider_path,"0831/uc_cohort_may2017-08-30.rds"))

head(uc_cohort_feb)
uc_cohort_feb <- uc_cohort_feb %>%  rename(mega_region = region)
uc_cohort_mar <- uc_cohort_mar %>%  rename(mega_region = region)
uc_cohort_apr <- uc_cohort_apr %>%  rename(mega_region = region)

table(uc_cohort_may$marketplace)

f_cohort <- function(uc_cohort) {
uc_view <- uc_cohort %>% 
  filter(days_past>=0, cancellation_flag %in% c('false'), marketplace %in% marketplace_ex_eats, mega_region %in% region_all) %>%
  group_by(days_past) %>% 
  summarise(uc_actual_tot = sum(unsettled_tot, na.rm=T),
            gross_fares_tot = sum(gross_fares, na.rm=T),
            gross_bills_tot = sum(gross_bills, na.rm=T)) %>% 
  mutate(uc_actual_bps = round(abs(uc_actual_tot/sum(gross_bills_tot)*10000),2),
         gb_tot = sum(gross_bills_tot))
}

uc_cohort_may1 <- uc_cohort_may %>% filter(trip_date <= as.Date('2017-05-15'))
uc_cohort_may2 <- uc_cohort_may %>% filter(trip_date > as.Date('2017-05-15'))
uc_cohort_jun1<- uc_cohort_jun %>% filter(trip_date <= as.Date('2017-06-15'))
uc_cohort_jun2 <- uc_cohort_jun %>% filter(trip_date > as.Date('2017-06-15'))
uc_cohort_jul1<- uc_cohort_jul %>% filter(trip_date <= as.Date('2017-07-15'))
uc_cohort_jul2 <- uc_cohort_jul %>% filter(trip_date > as.Date('2017-07-15'))
uc_cohort_aug1 <- uc_cohort_aug %>% filter(trip_date <= as.Date('2017-08-15'))


# uc_view_16jun <- f_cohort(uc_cohort_16jun)
# uc_view_16nov <- f_cohort(uc_cohort_16nov)
# uc_view_jan <- f_cohort(uc_cohort_jan)
# 
uc_view_feb <- f_cohort(uc_cohort_feb)
uc_view_mar <- f_cohort(uc_cohort_mar)
uc_view_apr <- f_cohort(uc_cohort_apr)

# uc_view_may1 <- f_cohort(uc_cohort_may1)
# uc_view_may2 <- f_cohort(uc_cohort_may2)
# uc_view_jun1 <- f_cohort(uc_cohort_jun1)
# uc_view_jun2 <- f_cohort(uc_cohort_jun2)
# uc_view_jul1 <- f_cohort(uc_cohort_jul1)
# uc_view_jul2 <- f_cohort(uc_cohort_jul2)

# view_mos <- c('feb','mar','apr','may1','may2','jun1','jun2','jul1','jul2','aug1')
view_mos <- c('may1','may2','jun1','jun2','jul1','jul2','aug1')
for (i in 1:length(view_mos)) { 
    mos <- view_mos[i]
    df <- f_cohort(get(paste("uc_cohort", mos, sep = '_')))
    ##write.csv(df,file = paste0(rider_path, updated_folder,"/",paste("uc_view", mos, sep = '_'),".csv"))
    assign(paste("uc_view", mos, sep = '_'), df)
} 

head(uc_cohort_may2)
tail(uc_view_may2)

update_date <- as.Date('2017-08-30')
start <-0
#update_date - as.Date('2017-06-01') + 1
# head(uc_view_16jun)
# uc_plot_16jun <- uc_view_16jun[start:180, c(1,5)]
# uc_plot_16nov <- uc_view_16nov[start:180, c(1,5)]
# uc_plot_jan <- uc_view_jan[start:as.numeric(as.Date('2017-07-18') - as.Date('2017-02-01') + 1), c(1,5)]

uc_plot_feb <- uc_view_feb[start:as.numeric(update_date - as.Date('2017-03-01') + 1), c(1,5)]
uc_plot_mar <- uc_view_mar[start:as.numeric(update_date - as.Date('2017-04-01') + 1), c(1,5)]
uc_plot_apr <- uc_view_apr[start:as.numeric(update_date - as.Date('2017-05-01') + 1), c(1,5)]
uc_plot_may1 <- uc_view_may1[start:as.numeric(update_date - as.Date('2017-05-16') + 1), c(1,5)]
uc_plot_may2 <- uc_view_may2[start:as.numeric(update_date - as.Date('2017-06-01') + 1), c(1,5)]
uc_plot_jun1 <- uc_view_jun1[start:as.numeric(update_date - as.Date('2017-06-16') + 1), c(1,5)]
uc_plot_jun2 <- uc_view_jun2[start:as.numeric(update_date - as.Date('2017-07-01') + 1), c(1,5)]
uc_plot_jul1 <- uc_view_jul1[start:as.numeric(update_date - as.Date('2017-07-16') + 1), c(1,5)]
uc_plot_jul2 <- uc_view_jul2[start:as.numeric(update_date - as.Date('2017-08-01') + 1), c(1,5)]
uc_plot_aug1 <- uc_view_aug1[start:as.numeric(update_date - as.Date('2017-08-16') + 1), c(1,5)]

head(uc_plot_aug1)
tail(uc_plot_jan)
nrow(uc_plot_aug1)
nrow(uc_plot_jan)
##write.csv(df,file = paste0(rider_path, updated_folder,"/",paste("uc_view", mos, sep = '_'),".csv"))

#### print out all curves
days_past_all <- data.frame(days_past = 0:180)
head(days_past_all)

view_mos <- c('feb','mar','apr','may1','may2','jun1','jun2','jul1','jul2','aug1')
view_mos <- c('may1','may2','jun1','jun2','jul1','jul2','aug1')

uc_plot_all <- days_past_all
for (i in 1:length(view_mos)) { 
  mos <- view_mos[i]
  uc_plot_all <- uc_plot_all %>% 
    left_join(get(paste("uc_plot", mos, sep = '_')), by='days_past')
} 

head(uc_plot_all)
write.csv(uc_plot_all,file = paste0(rider_path, updated_folder,"/uc_plot_all",update_date,".csv"))



# set up the plot 
start <-6
uc_plot_feb <- uc_view_feb[start:as.numeric(update_date - as.Date('2017-03-01') + 1), c(1,5)]
uc_plot_mar <- uc_view_mar[start:as.numeric(update_date - as.Date('2017-04-01') + 1), c(1,5)]
uc_plot_apr <- uc_view_apr[start:as.numeric(update_date - as.Date('2017-05-01') + 1), c(1,5)]
uc_plot_may1 <- uc_view_may1[start:as.numeric(update_date - as.Date('2017-05-16') + 1), c(1,5)]
uc_plot_may2 <- uc_view_may2[start:as.numeric(update_date - as.Date('2017-06-01') + 1), c(1,5)]
uc_plot_jun1 <- uc_view_jun1[start:as.numeric(update_date - as.Date('2017-06-16') + 1), c(1,5)]
uc_plot_jun2 <- uc_view_jun2[start:as.numeric(update_date - as.Date('2017-07-01') + 1), c(1,5)]
uc_plot_jul1 <- uc_view_jul1[start:as.numeric(update_date - as.Date('2017-07-16') + 1), c(1,5)]
uc_plot_jul2 <- uc_view_jul2[start:as.numeric(update_date - as.Date('2017-08-01') + 1), c(1,5)]
uc_plot_aug1 <- uc_view_aug1[start:as.numeric(update_date - as.Date('2017-08-16') + 1), c(1,5)]

# plot(xrange, yrange, type="n", xlab="Age (days)",ylab="Circumference (mm)" )
plot(range(1:180), range(1:100), type="n", xlab="Age (days)", ylab="uc_actual_bps" ) 

# add lines 
# plot_mos <- c('16jun','16nov','jan','feb','mar','apr')
plot_mos <-  c('may1','may2','jun1','jun2','jul1','jul2','aug1')
colors <- rainbow(length(plot_mos)) 
##colors <- c('red','green','blue')
for (i in 1:length(plot_mos)) { 
  pmos <- plot_mos[i]
  line_name <- paste("uc_plot", pmos, sep = '_')
  lines(get(line_name),type="l", lwd=1.5, col=colors[i]) 
} 

# add a title and subtitle 
title("DNS Cohort View (bps)", paste0("updated on " , update_date))

# add a legend 
legend(120, 80, 1:length(plot_mos), legend=plot_mos, col=colors, bty='n', 
       lty=1:1, lwd=1.5, cex=0.8, pch=NA, border = FALSE, fill= NA)
       





