library(dplyr)
library(ggplot2)
library(uberplot)

getwd()
setwd("/Users/guoyu.zhu/Documents/Safety")
getwd()

incident <- read.csv("model_incident_vars.csv", head=TRUE, sep=",")
rule <- read.csv("model_safety_rule_vars.csv", head=TRUE, sep=",")

colnames(incident) 
colnames(rule) 
###colnames(rule)[1] <- "Week"
head(incident)
head(rule)
nrow(incident)
nrow(rule)

summary(incident)
str(incident)


all <- merge(incident, rule, by=c("Week", "Region","RiderSeg"), all.x=TRUE, all.y=FALSE)
names(all)
str(all)
nrow(all)
head(all)

###na_rec <- all[is.na(all$NumCmpltTrips),]
library(plyr)
##ddply(na_rec, ~ Week,summarise,sum=sum(ReportedIncidents))

all$NumCmpltTrips_MM <- all$NumCmpltTrips/1000000
all$PercRiderRuleHit_bps <- all$PercRiderRuleHit*10000
summary(all)

# print default contrasts
contrasts(all$Region)

all$Region <- relevel(all$Region, ref = "Other")
contrasts(all$Region)

contrasts(all$RiderSeg)
contrasts(all$SeverityLevel)

lm.all <- lm(ReportedIncidents ~ Region + RiderSeg + SeverityLevel
             + TheftWeaponIncidents + NumRiderHits + NumCmpltTrips_MM, data=all)

summary(lm.all)
coef(summary(lm.all)) # show regression coefficients table
anova(lm.all) # show ANOVA table


lm2.all <- lm(ReportedIncidents ~ Region + RiderSeg + SeverityLevel
              + PercRiderRuleHit_bps + NumCmpltTrips_MM, data=all)

summary(lm2.all)
coef(summary(lm2.all)) # show regression coefficients table
anova(lm2.all) # show ANOVA table

anova(lm2.all, lm.all)

ddply(all, ~ Week,summarise,sum=sum(ReportedIncidents))

brazil <- subset(all, Region =='Brazil')
brazil_s <- subset(brazil, !(Week %in% c("2/6/17","4/17/17", "4/24/17")))

ddply(brazil_s, ~ Week,summarise,sum=sum(ReportedIncidents))

## + PercRiderRuleHit
lm.brazil <- lm(ReportedIncidents ~ RiderSeg + SeverityLevel
                + NumRiderHits + NumCmpltTrips_MM, data=brazil_s)
summary(lm.brazil)

lm2.brazil <- lm(ReportedIncidents ~ RiderSeg + SeverityLevel
                + PercRiderRuleHit_bps + NumCmpltTrips_MM, data=brazil_s)
summary(lm2.brazil)

lm3.brazil <- lm(ReportedIncidents ~ RiderSeg
                 + PercRiderRuleHit_bps + NumCmpltTrips_MM, data=brazil_s)
summary(lm3.brazil)

brazil_new <- subset(brazil_s, RiderSeg =='New Rider')

lm4.brazil <- lm(ReportedIncidents ~ PercRiderRuleHit_bps + NumCmpltTrips_MM, data=brazil_new)
summary(lm4.brazil)

lm5.brazil <- lm(ReportedIncidents ~ PercRiderRuleHit_bps, data=brazil_new)
summary(lm5.brazil)
