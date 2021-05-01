install.packages('ChainLadder')
library(ChainLadder)
require(ChainLadder)
library(reshape2) ###dcast
library(utils)  ###View
library(dplyr)

proj_path <- "/Users/guoyu.zhu/Documents/lossforecast/forecasting/"

cb_cum <- read.csv("/Users/guoyu.zhu/Documents/lossforecast/forecasting/raw/cb_2017_all_cum.csv", head=TRUE)

head(cb_cum)
### Get aggregated gross fares by region by trip month
pv_gross_fares <- cb_cum %>% 
  filter(f.region != 'NULL') %>% 
  group_by (f.region, trip_month)  %>% 
  summarize (f.gross_fares = sum(f.gross_fares, na.rm=T))
pv_gross_fares_print <- dcast(pv_gross_fares, f.region ~ trip_month, value.var = "f.gross_fares")

View(pv_gross_fares_print)

### Get cumulative chargeback amount by bill_diff_date by trip month
fun_cb_cum_pv <- function (region_slct) {
cb_cum_pv <- cb_cum %>% 
            filter (f.region %in% region_slct, f.bill_diff_date > 0) %>% 
            group_by (f.bill_diff_date, trip_month)  %>% 
            summarize (cum_cb_tot = sum(cum_cb_tot, na.rm=T))
cb_cum_pv_print <- round(dcast(cb_cum_pv, f.bill_diff_date ~ trip_month, value.var = "cum_cb_tot"),0)
  
return(cb_cum_pv_print)
}

cb_cum_pv_emea <- fun_cb_cum_pv(region_slct='EMEA')
View(cb_cum_pv_emea)

cb_cum_pv_na <- fun_cb_cum_pv(region_slct='US & Canada')
View(cb_cum_pv_na)

cb_cum_pv_latam <- fun_cb_cum_pv(region_slct='LatAm')
View(cb_cum_pv_latam)

cb_cum_pv_sena <- fun_cb_cum_pv(region_slct='SENA')
View(cb_cum_pv_sena)

region_slct <- c('EMEA','SENA','India','LatAm')
cb_cum_pv_exna <- fun_cb_cum_pv(region_slct=region_slct)
View(cb_cum_pv_exna)

#### Generate Loss Development factor

cutoff_day <-7
##fun_age_dev_factor <- function (region_slct, cutoff_day) {

#### 1. generated triangle matrix
cb_cum_slct <- cb_cum %>%
  filter(f.region %in% region_slct, f.bill_diff_date >=cutoff_day, f.trip_date != '2017-04-13')  %>%
  group_by(f.trip_date, f.bill_diff_date)    %>%
  summarise(cum_cb_tot = sum(cum_cb_tot))  %>%
  arrange(as.Date(f.trip_date))  

cb_pivot <- dcast(cb_cum_slct, f.trip_date ~ f.bill_diff_date, value.var ="cum_cb_tot")
cb_pivot

cb_pivot_cln <- cb_pivot[, 2:ncol(cb_pivot)]
cb_tri <- as.triangle(as.matrix(cb_pivot_cln))
cb_tri
dim(cb_tri)
tail(cb_tri)

#### 1. read triangle matrix from csv file
cb_tri_raw <- read.csv(paste0(proj_path,"raw/eats_cb_triangle.csv"), head=FALSE)
cb_tri <- as.triangle(as.matrix(cb_tri_raw))
cb_tri

n <-nrow(cb_tri)
n

## linkratios_vwtd
# round(linkratios_vwtd, 3) # display to only three decimal places
## Loss Development Factor (LDF) method.
linkratios_vwtd <- c(attr(ata(cb_tri), "vwtd"), tail = 1.000)
LR_vwtd <-round(linkratios_vwtd[1:n-1],3)
LR_vwtd

LDF_vwtd <- rev(cumprod(rev(LR_vwtd)))
LDF_vwtd

linkratios_smpl <- c(attr(ata(cb_tri), "smpl"), tail = 1.000)
LR_smpl <- round(linkratios_smpl[1:n-1],3)
LDF_smpl <- rev(cumprod(rev(LR_smpl)))
LDF_smpl

r <-30
f_lr_vwtd_lastX <-  function (r) {
  linkratios_vwtd_lxd <- sapply(1:(n-1),
            function(i){
              if (n-i-r <= 1){
                sum(cb_tri[c(1:(n-i)),i+1])/sum(cb_tri[c(1:(n-i)),i])
                } else {
                sum(cb_tri[c((n-i-r):(n-i)),i+1])/sum(cb_tri[c((n-i-r):(n-i)),i])
                }
            } )
 return(linkratios_vwtd_lxd)
}

##LR_vwtd_l30d <- round(linkratios_vwtd_l30d,3)
LR_vwtd_l30d <- round(f_lr_vwtd_lastX(30),3)
LR_vwtd_l30d

LR_vwtd_l60d <- round(f_lr_vwtd_lastX(60),3)
LR_vwtd_l60d

## attr(ata(cb_tri),"smpl")
# print(ata(cb_tri))
LR_tri <- ata(cb_tri)
dim(LR_tri)
m <-nrow(cb_tri)
f_lr_smpl_lastX <-  function (r) {
  linkratios_smpl_lxd <- sapply(1:(m-1),
                               function(i){
                                 if (m-i-r <= 1){
                                   mean(LR_tri[c(1:(m-i)),i],na.rm=T)
                                 } else {
                                   mean(LR_tri[c((m-i-r):(m-i)),i],na.rm=T)
                                 }
                               })
  return(linkratios_smpl_lxd)
}
##LR_smpl_l30d <- round(linkratios_smpl_l30d,3)
LR_smpl_l30d <- round(f_lr_smpl_lastX(30),3)
LR_smpl_l30d
LR_smpl_l60d <- round(f_lr_smpl_lastX(60),3)
LR_smpl_l60d

LDF_all <- round(cbind(LR_vwtd, LR_smpl, LR_vwtd_l30d, LR_vwtd_l60d, LR_smpl_l30d, LR_smpl_l60d, LDF_vwtd),3)
rownames(LDF_all) <- colnames(cb_tri)[1:n-1]
## return(LDF_all) }

View(LDF_all)

dim(LR_tri)
n
LDF_EMEA <- fun_age_dev_factor(region_slct='EMEA', cutoff_day=7)
View(LDF_EMEA[1:120,])

LDF_LatAm <- fun_age_dev_factor(region_slct='LatAm', cutoff_day=7)
View(LDF_LatAm[1:130,])

LDF_SENA <- fun_age_dev_factor(region_slct='SENA', cutoff_day=8)
View(LDF_SENA[1:130,])

LDF_US_CA <- fun_age_dev_factor(region_slct='US & Canada', cutoff_day=1)
View(LDF_US_CA[1:130,])

region_slct <- c('EMEA','SENA','India','LatAm')
cutoff_day=6
LDF_ex_USCA1 <- fun_age_dev_factor(region_slct=region_slct, cutoff_day=5)
View(LDF_ex_USCA1[1:130,])

region_slct <- c('US & Canada','EMEA','SENA','India','LatAm')
LDF_global <- fun_age_dev_factor(region_slct=region_slct, cutoff_day=1)
View(LDF_global[1:130,])


LDF_ex_USCA <-LDF_all
write.csv(LR_tri, paste0(proj_path,"LR_tri_EMEA.csv"))
View(LR_tri)

cb_tri[1,]
LR_tri[1,]

currentEval <- getLatestCumulative(cb_tri) 
currentEval

# Reverse the LDFs so the first, least mature factor [1]
#        is applied to the last origin year (1990)
EstdUlt_vwtd <- currentEval * rev(LDF_vwtd)
EstdUlt_smpl <- currentEval * rev(LDF_smpl)

### Start with the body of the exhibit
Exhibit <- data.frame(cb_pivot$f.trip_date, currentEval, LDF_vwtd = round(rev(LDF_vwtd), 3), EstdUlt_vwtd, LDF_smpl = round(rev(LDF_smpl), 3), EstdUlt_smpl)
View(Exhibit)

Exhibit <- rbind(Exhibit,data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt), row.names = "Total"))
Exhibit



