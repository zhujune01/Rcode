getwd()
setwd("C:\\Users\\gzhu2\\Documents\\2014\\Return_fraud\\CA\\Rmodel\\V2")
getwd()

## load training data
ca_nrr <- read.csv("C:/Users/gzhu2/Documents/2014/Return_fraud/CA/Rmodel/V2/srm_mdl_nrr_cafl_v2_n.csv", header = TRUE)

ca_nrr_OOT <- read.csv("C:/Users/gzhu2/Documents/2014/Return_fraud/CA/Rmodel/V2/srm_mdl_nrr_cafl_v2_OOt.csv", header = TRUE)
ca_nrr_OOT$bad <- as.factor(ca_nrr_OOT$bad)
ca_nrr_OOT[is.na(ca_nrr_OOT)]<-0

save(ca_nrr_OOT, file="ca_nrr_oot")
load("ca_nrr_OOT")


ca_nrr_aug <- read.csv("C:/Users/gzhu2/Documents/2014/Return_fraud/CA/Rmodel/V2/srm_mdl_nrr_cafl_16aug.csv", header = TRUE)

names(ca_nrr_aug)
ca_nrr_aug$bad <- as.factor(ca_nrr_aug$bad)
ca_nrr_aug[is.na(ca_nrr_aug)]<-0

save(ca_nrr_aug, file="ca_nrr_aug")
load("ca_nrr_aug")

##raw_data$check<-rep(0,nrow(raw_data))
##raw_data$check[which(raw_data$rf_all_score==raw_data$nrr_rf_v1_score)]<-1
##table(raw_data$check)
##table(raw_data$nrr_rf_v1_score>0.3, raw_data$score_nrr>-0.38)


names(ca_nrr)
head(ca_nrr)
str(ca_nrr)
dim(ca_nrr)


## Training (2/3) and Test data (1/3) set
set.seed(1234)
ind <- sample(2, nrow(ca_nrr), replace=TRUE, prob=c(0.67, 0.33))

ca_nrr$bad <- as.factor(ca_nrr$bad)

table(ca_nrr$bad)
table(ca_nrr$region_name)
hist(ca_nrr$e_ret_amt)

train <- ca_nrr[ind==1, -c(1,14,37,38,41:43,46:47, 71:75,77,137)]
names(train)
test <- ca_nrr[ind==2, -c(1,14,37,38,41:43,46:47, 71:75,77,137)]

train[is.na(train)]<-0
test[is.na(test)]<-0

save(train, file="ca_nrr_train")
save(test, file="ca_nrr_test")

load("ca_nrr_train")
load("ca_nrr_test")


nrow(test)
names(test)
write.csv(test, file = "ca_nrr_test.csv",row.names=TRUE)


head(train)
nrow(train)
head(train)
summary(train)
nrow(test)

####Random forest
install.packages("randomForest")
install.packages("ROCR")


library(randomForest)
library(ROCR)
library(caret)

## importance - Should importance of predictors be assessed? Default=FALSE
## proximity - Should proximity measure among the rows be calculated? Default=FALSE

rf <- randomForest(bad ~ ., data=train,ntree=50, importance=TRUE)

rf2 <- randomForest(bad ~ ., data=train,ntree=200, importance=TRUE)

plot(rf2, log="y", main="Random Forest with Default mtry")
legend("right", colnames(rf2$err.rate),col=1:4,cex=0.8,fill=1:4)
##legend("topright", colnames(rf2$err.rate),col=1:4,cex=0.8,fill=1:4)



ptm <- proc.time()
rf3 <- randomForest(bad ~ ., data=train, mtry=33, ntree=150, importance=TRUE)
proc.time() - ptm
save(rf3, file="ca_nrr_rf3.Rdata")

###save any set of objects on a file, can be loaded back to R using load()
save(rf, file="ca_nrr_rf.Rdata")
save(rf2, file="ca_nrr_rf2.Rdata")


ptm <- proc.time()
rf_50_11 <- randomForest(bad ~ ., data=train, ntree=50, importance=TRUE)
proc.time() - ptm
save(rf_50_11, file="ca_nrr_rf_50_11.Rdata")

ptm <- proc.time()
rf_100_11 <- randomForest(bad ~ ., data=train, ntree=100, importance=TRUE)
proc.time() - ptm
save(rf_100_11, file="ca_nrr_rf_100_11.Rdata")

ptm <- proc.time()
rf_100_22 <- randomForest(bad ~ ., data=train, mtry=22, ntree=100, importance=TRUE)
proc.time() - ptm
save(rf_100_22, file="ca_nrr_rf_100_22.Rdata")

ptm <- proc.time()
rf_100_33 <- randomForest(bad ~ ., data=train, mtry=33, ntree=100, importance=TRUE)
proc.time() - ptm
save(rf_100_33, file="ca_nrr_rf_100_33.Rdata")

ptm <- proc.time()
rf_100_73 <- randomForest(bad ~ ., data=train, mtry=73, ntree=100, importance=TRUE)
proc.time() - ptm
save(rf_100_73, file="ca_nrr_rf_100_73.Rdata")

ptm <- proc.time()
rf_150_11 <- randomForest(bad ~ ., data=train, ntree=150, importance=TRUE)
proc.time() - ptm
save(rf_150_11, file="ca_nrr_rf_150_11.Rdata")

ptm <- proc.time()
rf_150_33 <- randomForest(bad ~ ., data=train, mtry=33, ntree=150, importance=TRUE)
proc.time() - ptm
save(rf_150_33, file="ca_nrr_rf_150_33.Rdata")

ptm <- proc.time()
rf_150_73 <- randomForest(bad ~ ., data=train, mtry=73, ntree=150, importance=TRUE)
proc.time() - ptm
save(rf_150_73, file="ca_nrr_rf_150_73.Rdata")


load("ca_nrr_rf.Rdata")
load("ca_nrr_rf2.Rdata")
load("ca_nrr_rf3.Rdata")
load("ca_nrr_rf_50_11.Rdata")
load("ca_nrr_rf_100_11.Rdata")
load("ca_nrr_rf_100_22.Rdata")
load("ca_nrr_rf_100_33.Rdata")
load("ca_nrr_rf_100_73.Rdata")
load("ca_nrr_rf_150_11.Rdata")
load("ca_nrr_rf_150_33.Rdata")
load("ca_nrr_rf_150_73.Rdata")


# plot.randomForest shows how OOB error and in-class OOB error evolved with increasing number of trees; 
# for classification, black solid line for overall OOB error 
# and a bunch of colour lines, one for each class' error (i.e. 1-this class recall).
# for regression, one black solid line for OOB MSE error.

# print(rf.model) and that gave me a confusion matrix with the class.error.


# varImpPlot shows attribute importance measures for top attributes and 
# MDSplot all objects plotted on the 2D projection of RF object proximity measure.
varImpPlot(rf)
varImpPlot(rf_50_11)
varImpPlot(rf_50_11,type=2)

varImpPlot(rf_100_33)


print(rf_50_11) # view results 
importance(rf_50_11) # importance of each predictor

print(rf_100_11) # view results 
importance(rf_100_11) # importance of each predictor

print(rf_100_22) # view results 
importance(rf_100_22) # importance of each predictor

print(rf_100_33) # view results 
importance(rf_100_33) # importance of each predictor

print(rf_100_73) # view results 
importance(rf_100_73) # importance of each predictor


print(rf_150_11) # view results 
importance(rf_150_11) # importance of each predictor

print(rf_150_33) # view results 
importance(rf_150_33) # importance of each predictor

print(rf_150_73) # view results 
importance(rf_150_73) # importance of each predictor

#####################################################
#####model performance chec ########################
#####################################################
rf_score<-function(rf_model){
test$rf_model_score<-predict(rf_model, type='prob', test)[,2]
pred_rf_mode1<-prediction(test$rf_model_score, test$bad)
}

rf_score(rf_50_11)
head(test)

ca_test<-test
test<-ca_nrr_OOT
dim(test)

test$rf_50_11_score<-predict(rf_50_11, type='prob', test)[,2]
pred_rf_50_11<-prediction(test$rf_50_11_score, test$bad)
perfpr_rf_50_11<-performance(pred_rf_50_11, "prec", "rec")


test$rf_100_11_score<-predict(rf_100_11, type='prob', test)[,2]
pred_rf_100_11<-prediction(test$rf_100_11_score, test$bad)
perfpr_rf_100_11<-performance(pred_rf_100_11, "prec", "rec")


test$rf_100_22_score<-predict(rf_100_22, type='prob', test)[,2]
pred_rf_100_22<-prediction(test$rf_100_22_score, test$bad)
perfpr_rf_100_22<-performance(pred_rf_100_22, "prec", "rec")

test$rf_100_33_score<-predict(rf_100_33, type='prob', test)[,2]
pred_rf_100_33<-prediction(test$rf_100_33_score, test$bad)
perfpr_rf_100_33<-performance(pred_rf_100_33, "prec", "rec")

test$rf_100_73_score<-predict(rf_100_73, type='prob', test)[,2]
pred_rf_100_73<-prediction(test$rf_100_73_score, test$bad)
perfpr_rf_100_73<-performance(pred_rf_100_73, "prec", "rec")

test$rf_150_11_score<-predict(rf_150_11, type='prob', test)[,2]
pred_rf_150_11<-prediction(test$rf_150_11_score, test$bad)
perfpr_rf_150_11<-performance(pred_rf_150_11, "prec", "rec")

test$rf_150_33_score<-predict(rf_150_33, type='prob', test)[,2]
pred_rf_150_33<-prediction(test$rf_150_33_score, test$bad)
perfpr_rf_150_33<-performance(pred_rf_150_33, "prec", "rec")

test$rf_150_73_score<-predict(rf_150_73, type='prob', test)[,2]
pred_rf_150_73<-prediction(test$rf_150_73_score, test$bad)
perfpr_rf_150_73<-performance(pred_rf_150_73, "prec", "rec")



###PR curve
plot(perfpr_rf_50_11, main='PR Curve')
plot(perfpr_rf_100_11, col='blue', add=TRUE)
plot(perfpr_rf_100_22, col='green', add=TRUE)
plot(perfpr_rf_100_33, col='purple', add=TRUE)
plot(perfpr_rf_100_73, col='red', add=TRUE)

plot(perfpr_rf_150_11, col='brown', add=TRUE)
plot(perfpr_rf_150_33, col='orange', add=TRUE)
plot(perfpr_rf_150_73, col='darkgreen', add=TRUE)

plot(perfpr_rf_100_11, col='blue', main='PR Curve')
legend(0.2,0.4, c('rf_50_11', 'rf_100_33','rf_100_73','rf_150_11','rf_150_33','rf_150_73'), col=c('black', 'purple','red','brown','orange', 'darkgreen'), lwd=3)

legend(0.2,0.4, c('rf_50_11','rf_100_11','rf_100_22','rf_100_33','rf_100_73'), col=c('black','blue','green', 'purple','red'), lwd=3)

legend(0.2,0.4, c('rf_50_11','rf_100_11','rf_150_11','rf_100_22'), col=c('black','blue','brown','green'), lwd=3)

###ROC curve
perf_rf_50_11<-performance(pred_rf_50_11, "tpr", "fpr")
plot(perf_rf_50_11, col='green', main='ROC curve')

perf_rf_100_11<-performance(pred_rf_100_11, "tpr", "fpr")
plot(perf_rf_100_11, col='blue', add=TRUE, lty=2)


perf_rf<-performance(pred_rf, "tpr", "fpr")
plot(perf_rf, col='green', main='ROC curve')


perf_rf2<-performance(pred_rf2, "tpr", "fpr")
plot(perf_rf2, col='blue', add=TRUE, lty=2)


###PR curve
perfpr_rf<-performance(pred_rf, "prec", "rec")
plot(perfpr_rf, main='PR Curve')

perfpr_rf2<-performance(pred_rf2, "prec", "rec")
plot(perfpr_rf2, col='blue', add=TRUE)

legend(0.2,0.2, c('Random Forest(in-time)', 'Random Forest(off-time)'), col=c('black', 'blue'), lwd=3)

ca_nrr_jan$rf_score<-predict(rf, type='prob', ca_nrr_jan)[,2]
pred_rf2<-prediction(ca_nrr_jan$rf_score, ca_nrr_jan$bad)




###########define model variable list
mdl_vars <- c('c_nrr_ex_num_visit_45',
              'p_n_sal_ret_ccdcebtsvc_pct',
              'g_n_nrr_num_visits',
              'n_tot_amt',
              'n_ret_svc_amt',
        
              'g_n_ret_num_visits',
              'n_ret_amt',
              'p_n_ret_tot_sal_pct_30',
              'n_nrr_amt',
              'g_n_ret_num_u_str',
              'g_n_ret_num_non_cash',
              'n_ret_num_u_str_30',
              'p_n_nrr_ret_visit_pct',
              'p_n_sal_ccdcebt_pct',
              'g_n_num_u_svc',
              'p_n_sal_svc_pct',
              'g_n_ret_num_visits_30',
              'n_ret_amt_30',
              'p_n_ret_qty_item_pct',
              'c_nrr_ex_num_visits_90',
              'n_ret_qty',
              'p_n_ret_sal_item_pct',
              'c_nrr_amt_60',
              'p_n_ret_visit_pct',
              'c_nrr_ex_num_visits_30',
              'e_nrr_amt',
              'g_n_num_visits',
              'c_nrr_num_u_str_60',
              'p_n_nrr_ret_pct',
              'n_ret_amt_15',
              'n_dist_stddev',
              'n_ret_amt_2',
              'p_n_ret_store_pct',
              'c_nrr_qty_60',
              'n_ret_amt_7',
              'n_dist_max',
              'n_dist_avg',
              'p_n_ret_qty_depts_pct',
              'p_n_ret_sal_depts_pct',
              'c_nrr_amt_30',
              'n_tot_ret_cash_amt',

              'g_n_num_u_str',
              'n_dist_min',
              'n_sal_svc_amt',
              'p_n_ret_svc_pct',
              's_rr2nrr_visit_7_60',
              's_ret2sal_visit_7_60',
              's_rr2nrr_qty_7_60',
              's_ret2sal_qty_7_60',
              'g_n_sal_u_item',
              'n_sal_amt',
             
              'g_s_ro_nrr_rr_amt_7',
              'g_s_ro_nrr_rr_num_visits_7',
              'g_n_noncash_cnt',
              'g_s_ro_nrr_rr_qty_7',
              'g_n_ret_u_item',
              'c_num_u_str_7',

              'np_nrr_svc_rr_amt',
              'g_n_ret_num_visits_ge50',
              'g_n_ret_num_visits_lt20',
              'p_n_xch_ret_pct',
              'n_ret_num_u_str_7',
              'g_n_num_u_depts',
              'c_num_u_str_30',
              'n_xch_amt',
              'g_n_ret_num_u_depts',
              'g_n_days_bw_sale_ret_avg',
              'g_n_ret_num_visits_15',
              
              'g_n_cid_cnt',
              'g_n_ret_num_visits_7',
              'n_num_u_str_oor_30',
              'g_n_num_u_cc',
              'p_n_ret_ccdcebt_pct'
                                   
)

### 73 variables selected per VARimportance
train_s<-train[,c('bad',mdl_vars)]

dim(train_s)

ptm <- proc.time()
##rf_100_33s <- randomForest(bad ~ ., data=train_s, mtry=33, ntree=100, importance=TRUE)
rf_100_33s2 <- randomForest(bad ~ ., data=train_s, mtry=33, ntree=100, importance=TRUE)
proc.time() - ptm
save(rf_100_33s2, file="ca_nrr_rf_100_33s2.Rdata")

load("nrr_rf_data.Rdata")

##save(rf_100_33s, file="ca_nrr_rf_100_33s.Rdata")

varImpPlot(rf_100_33s)
importance(rf_100_33s) # importance of each predictor

test$rf_100_33s_score<-predict(rf_100_33s, type='prob', test)[,2]
pred_rf_100_33s<-prediction(test$rf_100_33s_score, test$bad)
perfpr_rf_100_33s<-performance(pred_rf_100_33s, "prec", "rec")


ca_nrr_aug$rf_100_33s_score<-predict(rf_100_33s, type='prob', ca_nrr_aug)[,2]
pred_rf_100_33s_aug<-prediction(ca_nrr_aug$rf_100_33s_score, ca_nrr_aug$bad)
perfpr_rf_100_33s_aug<-performance(pred_rf_100_33s_aug, "prec", "rec")


pred_lgr_v2_aug<-prediction(ca_nrr_aug$score_v2, ca_nrr_aug$bad)
perfpr_lgr_v2_aug<-performance(pred_lgr_v2_aug, "prec", "rec")

pred_lgr_v2<-prediction(test$score_v2, test$bad)
perfpr_lgr_v2<-performance(pred_lgr_v2, "prec", "rec")

plot(perfpr_rf_100_33, col='purple', main='PR curve')
plot(perfpr_rf_100_33s, col='green', add=TRUE)
plot(perfpr_rf_100_33s_aug, col='black', add=TRUE)

plot(perfpr_lgr_v2, col='blue', add=TRUE)
plot(perfpr_lgr_v2_aug, col='red', add=TRUE)

legend(0.2,0.4, c('rf_100_33_jul','rf_100_33s_jul','rf_100_33s_aug','lgr_jul','lgr_aug'), 
       col=c('purple','green','black', 'blue','red'), lwd=3)

test$rf_100_33s2_score<-predict(rf_100_33s2, type='prob', test)[,2]
pred_rf_100_33s2<-prediction(test$rf_100_33s2_score, test$bad)
perfpr_rf_100_33s2<-performance(pred_rf_100_33s2, "prec", "rec")


ca_nrr_aug$rf_100_33s2_score<-predict(rf_100_33s2, type='prob', ca_nrr_aug)[,2]
pred_rf_100_33s2_aug<-prediction(ca_nrr_aug$rf_100_33s2_score, ca_nrr_aug$bad)
perfpr_rf_100_33s2_aug<-performance(pred_rf_100_33s2_aug, "prec", "rec")

#########################
##### model evaluation
########################
raw_data$bad<-rep(0,nrow(raw_data))
raw_data$bad[which(raw_data$reco_nrr_inkiru=='DENY')]<-1
table(raw_data$reco_nrr_inkiru, raw_data$bad)


raw_data$score <- predict(rf_100_33s2, type='prob', raw_data[mdl_vars])[,2]
pred_rf_100_33s2_sep<-prediction(raw_data$score, raw_data$bad)
perfpr_rf_100_33s2_sep<-performance(pred_rf_100_33s2_sep, "prec", "rec")

pred_nrr_rf_v1_sep<-prediction(raw_data$nrr_rf_v1_score, raw_data$bad)
perfpr_nrr_rf_v1_sep<-performance(pred_nrr_rf_v1_sep, "prec", "rec")

pred_lgr_v2_sep<-prediction(raw_data$score_nrr, raw_data$bad)
perfpr_lgr_v2_sep<-performance(pred_lgr_v2_sep, "prec", "rec")

pred_lgr_v1_sep<-prediction(raw_data$score_nrr_chlg, raw_data$bad)
perfpr_lgr_v1_sep<-performance(pred_lgr_v1_sep, "prec", "rec")

plot(perfpr_rf_100_33s2, col='blue', main='PR curve')
plot(perfpr_rf_100_33s2_aug, col='red', add=TRUE)
plot(perfpr_rf_100_33s2_sep, col='brown', add=TRUE)
plot(perfpr_nrr_rf_v1_sep, col='darkgreen', add=TRUE)
plot(perfpr_lgr_v2_sep, col='orange', add=TRUE)
plot(perfpr_lgr_v1_sep, col='green', add=TRUE)

summary(test$rf_100_33s_score)
summary(test$rf_100_33_score)

names(ca_nrr_OOT)

write.csv(ca_nrr_OOT, file = "ca_nrr_OOT_jul.csv",row.names=TRUE)
write.csv(ca_nrr_aug, file = "ca_nrr_OOT_aug.csv",row.names=TRUE)


train$rf_100_33s_score<-predict(rf_100_33s, type='prob', train)[,2]
table((train$rf_100_33s_score>0.5),train$bad )

train$rf_100_33_score<-predict(rf_100_33, type='prob', train)[,2]
table((train$rf_100_33_score>0.3),train$bad )

table((test$rf_100_33_score>0.5),test$bad )
table((test$rf_100_33s_score>0.3),test$bad )
table((ca_nrr_aug$rf_100_33s_score>0.3),ca_nrr_aug$bad )
table((ca_nrr_aug$score_v2>-0.62),ca_nrr_aug$bad )

table((ca_nrr_aug$rf_100_33s_score>0.5),ca_nrr_aug$reco_nrr )
table((ca_nrr_aug$rf_100_33s_score>0.5 | ca_nrr_aug$score_v2>-0.62),ca_nrr_aug$bad )

table((test$rf_50_11_score>0.5),test$bad )
table((test$rf_100_11_score>0.5),test$bad )
table((test$rf_100_22_score>0.5),test$bad )
table((test$rf_100_73_score>0.5),test$bad )

names(test)
dim(test)
ca_nrr_OOT<-test
save(ca_nrr_OOT, file="ca_nrr_oot")

install.packages("gains")
library(gains)

###Gains table graph on training set.
test$bad.n <- as.numeric(test$bad)

gains_rf_100_33s <- gains(actual=test$bad.n, predicted=test$rf_100_33s_score, groups=100, optimal=TRUE)
plot(gains_rf_100_33s)
print(gains_rf_100_33s)


