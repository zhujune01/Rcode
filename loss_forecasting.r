source ("/Users/guoyu.zhu/Documents/code/R/init_setting.r")

install.packages('ChainLadder')
library(ChainLadder)
require(ChainLadder)
data(package="ChainLadder")
RAA
plot(RAA)
plot(RAA, lattice=TRUE)

raa.inc <- cum2incr(RAA)
raa.inc
raa.cum <- incr2cum(raa.inc)
raa.cum


tri <- read.csv("/Users/guoyu.zhu/Documents/lossforecast/loss_triangle.csv", head=FALSE)
tri <- as.triangle(as.matrix(tri))
tri

tri_wk <- read.csv("/Users/guoyu.zhu/Documents/lossforecast/loss_tri_week.csv", head=FALSE)
tri_wk <- as.triangle(as.matrix(tri_wk))
tri_wk

#### the volume weighted average development ratios of a cumulative loss development triangle
### sapply - passing a set of index values, works on a list or vector of data, return a vector
### sapply(1:3, function(x) x^2)
n <-135
f <- sapply(1:(n-1),
            function(i){
              sum(tri[c(1:(n-i)),i+1])/sum(tri[c(1:(n-i)),i])
            }
)
f
as.data.frame(f)


### Often it is not suitable to assume that the oldest origin year is fully developed. A
### typical approach is to extrapolate the development ratios, e.g. assuming a log-linear model.
n <- 135
f[is.na(f)]<-1.002
f[134]<-1.002
dev.period <- 1:(n-1)
plot(log(f-1) ~ dev.period, main="Log-linear extrapolation of age-to-age factors")
tail.model <- lm(log(f-1) ~ dev.period)
abline(tail.model)
co <- coef(tail.model)
co
## extrapolate another 100 dev. period
tail <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
tail
f.tail <- prod(tail) ####product of vector elements
f.tail
prod(1:5)
cumprod(1:5)
rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))

plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected loss development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

###The link ratios are then applied to the latest known cumulative claims amount 
###to forecast the next development period

f <- c(f, f.tail)
f
nrow(tri)
ncol(tri)

fulltri <- cbind(tri, ult = rep(0,nrow(tri)))
fulltri
for(k in 1:n){
  fulltri[(n-k+1):n, k+1] <- fulltri[(n-k+1):n,k]*f[k]
}

fulltri[,c('ult')]
getLatestCumulative(tri)
sum(fulltri[ ,c('ult')] - getLatestCumulative(tri))


linkratios <- c(attr(ata(tri), "vwtd"), tail = 1.001)
round(linkratios, 3) # display to only three decimal places

attr(ata(tri),"smpl")
summary(ata(tri))
print(ata(tri))

?tri
## Loss Development Factor (LDF) method.
LDF <- rev(cumprod(rev(linkratios)))
LDF
names(LDF) <- colnames(tri)
round(LDF,3)

currentEval <- getLatestCumulative(tri)
currentEval

# Reverse the LDFs so the first, least mature factor [1]
#        is applied to the last origin year (1990)
EstdUlt <- currentEval * rev(LDF)

### Start with the body of the exhibit
Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)
Exhibit

Exhibit <- rbind(Exhibit,data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt), row.names = "Total"))

###Since the early 1990s several papers have been published to embed the simple chain- ladder method into a statistical framework. 
###Ben Zehnwirth and Glenn Barnett point out in [ZB00] that the age-to-age link ratios can be regarded as the coefficients 
###of a weighted linear regression through the origin
lmCL <- function(i, Triangle){
  lm(y~x+0, weights=1/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
}

tri
lapply(c(1:(nrow(RAA)-1)), lmCL, RAA)
lm1<-lm(RAA[,2] ~ RAA[,1]+0, weights= 1/RAA[,1])
lm1

sapply(lapply(c(1:(nrow(tri)-1)), lmCL, tri), coef)
tri


mack <- MackChainLadder(RAA, est.sigma="Mack")


