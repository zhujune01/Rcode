netvar <- read.csv("C:/Users/gzhu2/Documents/2014/Return_fraud/network/s_netvar_apr_180d.csv", header = TRUE)
names(netvar)
head(netvar)
str(netvar)
summary(netvar)

### use all the data for clustering
netvar[is.na(netvar)]<-0
netvar_a<-netvar[-c(1:19)]
names(netvar_a)
nrow(netvar_a)
ncol(netvar_a)
summary(netvar_a)

library(stats)
km_1 <- kmeans(netvar_a, centers= 3, iter.max=100, nstart=1000)
table(km_1$cluster)

plot(netvar_a$rtn2sale_nongroc, netvar_a$z_purchaseNonGrocAmt,col=km_2$cluster)

# get cluster means 
aggregate(netvar_a,by=list(km_1$cluster),FUN=mean)

# append cluster assignment
netvar.pred1 <- data.frame(netvar, km_1$cluster)
write.csv(netvar.pred1, file = "C:/Users/gzhu2/Documents/2014/Return_fraud/network/netvar_april_180d_c3.csv",row.names=TRUE)
