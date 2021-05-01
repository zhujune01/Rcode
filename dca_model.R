library(dplyr)
library(ggplot2)
library(uberplot)

setwd("/Users/guoyu.zhu/Documents/DriverArrears/")
getwd()

###https://stats.stackexchange.com/questions/318971/fitting-exponential-decay-with-negative-y-values
##train
dca_india01 <- read.csv("dca_train_india.csv", head=TRUE, sep=",")
head(dca_india01)
##validation
dca_india02 <- read.csv("B0v2DDUfZO.csv", head=TRUE, sep=",")
head(dca_india02)

dca_india03 <- read.csv("OLr2e4lZVb.csv", head=TRUE, sep=",")
head(dca_india03)

dca_latam01 <- read.csv("latam/PrTlMDSTtE.csv", head=TRUE, sep=",")
head(dca_latam01)

ggplot(dca_latam01, aes(x=cash_mix, y=arrear_rate)) + geom_point()
ggplot(dca_latam01, aes(x=inc_rate, y=arrear_rate)) + geom_point()


f_cash_mix <- function (dca_input, cutoff) {
  hi_cash_mix01 <- filter(dca_input, cash_mix >= cutoff)
  hi_cum01 <- hi_cash_mix01 %>% 
  group_by(cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)
  return (hi_cum01)
}

hi_cum_latam01 <- f_cash_mix(dca_latam01, 0.3)
ggplot(hi_cum_latam01, aes(x=cash_mix, y=cum_arrear_rate)) + geom_point()

head(dca_latam01)

f_inc_rate <- function (dca_input, cutoff) {
  hi_cash_mix01 <- filter(dca_input, cash_mix == 0.7, inc_rate < cutoff)
  # hi_cum01 <- hi_cash_mix01 %>% 
  #   group_by(inc_rate) %>%
  #   summarize(cum_arrear= sum(new_arrears),
  #             sum_gb= sum(gross_bookings)) %>% 
  #   mutate(cum_arrear_rate = cum_arrear/sum_gb)
  return (hi_cash_mix01)
}

hi_inc_latam01 <- f_inc_rate(dca_latam01, 0.05)
ggplot(hi_inc_latam01, aes(x=inc_rate, y=arrear_rate)) + geom_point()

dca_latam01_le1p <- filter(dca_latam01, inc_rate < 0.01)
ggplot(dca_latam01_le1p, aes(inc_rate, weight = partners)) + geom_histogram(binwidth = 0.001)
ggplot(dca_latam01_le1p, aes(inc_rate, weight = new_arrears)) + geom_histogram(binwidth = 0.001)

##ggplot(hi_cum, aes(x = cash_mix, y = cum_arrear_rate*100)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

f_inc_cash_mix <- function (dca_input, cutoff) {
  hi_cash_mix01 <- filter(dca_input, cash_mix >= cutoff)  
  hi_cash_mix_cum01 <- hi_cash_mix01 %>% 
  group_by(inc_rate, cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)
  return (hi_cash_mix_cum01)
}

hi_cash_mix_latam01 <- f_inc_cash_mix (dca_latam01, 0.3)
ggplot(hi_cash_mix_latam01, aes(x=inc_rate, y=cum_arrear_rate)) + geom_point()

hi_cash_mix_latam01$f_cash_mix <- as.factor(hi_cash_mix_latam01$cash_mix)
ggplot(hi_cash_mix_latam01, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point()
#ggplot(hi_cash_mix_latam01, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)



dca_india <- dca_india01
ggplot(dca_india, aes(x=cash_mix, y=arrear_rate)) + geom_point()
ggplot(dca_india, aes(x=inc_rate, y=arrear_rate)) + geom_point()

hi_cash_mix01 <- filter(dca_india01, cash_mix >= 0.5)
hi_cum01 <- hi_cash_mix01 %>% 
  group_by(cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)
head(hi_cum01)
View(hi_cum01)

ggplot(hi_cum01, aes(x=cash_mix, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cum, aes(x = cash_mix, y = cum_arrear_rate*100)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

head(hi_cash_mix01)
nrow(hi_cash_mix01)
hi_cash_mix_cum01 <- hi_cash_mix01 %>% 
  group_by(inc_rate, cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)

nrow(hi_cash_mix_cum01)
head(hi_cash_mix_cum01)
ggplot(hi_cash_mix_cum01, aes(x=inc_rate, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cash_mix_cum, aes(x=interaction(inc_rate,cash_mix), y=cum_arrear_rate)) + geom_point() 

hi_cash_mix_cum01$f_cash_mix <- as.factor(hi_cash_mix_cum01$cash_mix)
ggplot(hi_cash_mix_cum01, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point()
ggplot(hi_cash_mix_cum01, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)
#ggplot(hi_cash_mix_cum, aes(x = inc_rate, y = cum_arrear_rate, color = cash_mix)) + geom_point() + geom_smooth(method = "glm", family = gaussian(lin="log"), start=c(0.1,0))

table(hi_cash_mix_cum01$cash_mix)

head(hi_cash_mix_cum01)
#### select start parameter
train <- filter(hi_cash_mix_cum01, cash_mix == 0.9)
fit <- nls(cum_arrear_rate ~ SSasymp(inc_rate, Asym, R0, lrc), data = train)
summary(fit)
## Asym+(R0-Asym)*exp(-exp(lrc)*input)
exp(coef(fit)[["lrc"]]) #lambda
## https://stats.stackexchange.com/questions/318971/fitting-exponential-decay-with-negative-y-values


# fit a nonlinear model using preselected start parameter
f <- function(x1,x2,a,b1,b2) {a * exp(-b1*x1 + b2*x2) }
fm01 <- nls(cum_arrear_rate ~ f(inc_rate,cash_mix,a,b1,b2), data = hi_cash_mix_cum01, start = list(a=3, b1=38,b2=15))
summary(fm01)
hi_cash_mix_cum01$pred_arrear <- predict(fm)

ggplot(hi_cash_mix_cum01, aes(x = inc_rate, color = f_cash_mix)) + geom_point(aes(y = cum_arrear_rate))  + geom_line(aes(y = pred_arrear))

head(hi_cash_mix_cum)
co <- coef(fm)
co[["a"]]
co[["b1"]]
co[["b2"]]
###ggplot(x, aes(x = time, color = Factor)) + geom_point(aes(y = y)) + geom_line(aes(y = modelled))
## g <- append(g, predict(nls(y ~ dec.fun(N, r, time), data = z, start = list(N = 5, r = 0))))
# fit <- nls(Y~ c1*(exp(-k1*X1))+ c2/(1+b2*exp(-k2*X2)) + X3+ X4,
#            start=list(c1=Y[1], k1=0, c2 = 1, b2 = 0, k2 = 111))
# 
# # get estimates of a, b
# co <- coef(fm)
# co

fm02 <- nls(cum_arrear_rate ~ f(inc_rate,cash_mix,a,b1,b2), data = hi_cash_mix_cum02, start = list(a=3, b1=38,b2=15))
summary(fm02)
hi_cash_mix_cum02$pred_arrear <- predict(fm02)
ggplot(hi_cash_mix_cum02, aes(x = inc_rate, color = f_cash_mix)) + geom_point(aes(y = cum_arrear_rate))  + geom_line(aes(y = pred_arrear))

co02 <- coef(fm02)
co02[["a"]]
co02[["b1"]]
co02[["b2"]]

##validation
dca_india02 <- read.csv("B0v2DDUfZO.csv", head=TRUE, sep=",")
head(dca_india02)

ggplot(dca_india02, aes(x=cash_mix, y=arrear_rate)) + geom_point()
ggplot(dca_india02, aes(x=inc_rate, y=arrear_rate)) + geom_point()

hi_cash_mix02 <- filter(dca_india02, cash_mix >= 0.5)
hi_cum02 <- hi_cash_mix02 %>% 
  group_by(cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)

View(hi_cum02)

ggplot(hi_cum02, aes(x=cash_mix, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cum, aes(x = cash_mix, y = cum_arrear_rate*100)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

head(hi_cash_mix02)
nrow(hi_cash_mix02)
hi_cash_mix_cum02 <- hi_cash_mix02 %>% 
  group_by(inc_rate, cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)
hi_cash_mix_cum02$f_cash_mix <- as.factor(hi_cash_mix_cum02$cash_mix)

head(hi_cash_mix_cum02)
nrow(hi_cash_mix_cum02)
ggplot(hi_cash_mix_cum02, aes(x=inc_rate, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cash_mix_cum, aes(x=interaction(inc_rate,cash_mix), y=cum_arrear_rate)) + geom_point() 

ggplot(hi_cash_mix_cum02, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point()
ggplot(hi_cash_mix_cum02, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)
#ggplot(hi_cash_mix_cum02, aes(x = inc_rate, y = cum_arrear_rate, color = cash_mix)) + geom_point() + geom_smooth(method = "glm", family = gaussian(lin="log"), start=c(0.1,0))


hi_cash_mix03 <- filter(dca_india03, cash_mix >= 0.5)
hi_cum03 <- hi_cash_mix03 %>% 
  group_by(cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)

View(hi_cum03)

ggplot(hi_cum03, aes(x=cash_mix, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cum, aes(x = cash_mix, y = cum_arrear_rate*100)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

head(hi_cash_mix03)
nrow(hi_cash_mix03)
hi_cash_mix_cum03 <- hi_cash_mix03 %>% 
  group_by(inc_rate, cash_mix) %>%
  summarize(cum_arrear= sum(new_arrears),
            sum_gb= sum(gross_bookings)) %>% 
  mutate(cum_arrear_rate = cum_arrear/sum_gb)

hi_cash_mix_cum03$f_cash_mix <- as.factor(hi_cash_mix_cum03$cash_mix)

head(hi_cash_mix_cum03)
nrow(hi_cash_mix_cum03)
ggplot(hi_cash_mix_cum03, aes(x=inc_rate, y=cum_arrear_rate)) + geom_point()
##ggplot(hi_cash_mix_cum, aes(x=interaction(inc_rate,cash_mix), y=cum_arrear_rate)) + geom_point() 

ggplot(hi_cash_mix_cum03, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point()
ggplot(hi_cash_mix_cum03, aes(x = inc_rate, y = cum_arrear_rate, color = f_cash_mix)) + geom_point() + stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)


## g <- append(g, predict(nls(y ~ dec.fun(N, r, time), data = z, start = list(N = 5, r = 0))))
f <- function(x1,x2,a,b1,b2) {a * exp(-b1*x1 + b2*x2) }
#f(inc_rate,cash_mix,a,b1,b2)
a <- co[["a"]]
b1 <- co[["b1"]]
b2 <- co[["b2"]]
str(hi_cash_mix_cum02)
#hi_cash_mix_cum02$pred_arrear <- predict(nls(cum_arrear_rate ~ f(inc_rate,cash_mix,a,b1,b2), data = hi_cash_mix_cum02, start = list(a=3, b1=38,b2=15)))
hi_cash_mix_cum02$pred_arrear <- mapply(function(x1,x2,a,b1,b2) {a * exp(-b1*x1 + b2*x2) }, hi_cash_mix_cum02$inc_rate,hi_cash_mix_cum02$cash_mix,a,b1,b2)
head(hi_cash_mix_cum02)
## mutate(hi_cash_mix_cum02, pred_arrear = f(inc_rate,cash_mix,a,b1,b2) )
## warnings()

hi_cash_mix_cum02$cash_mix <- as.factor(hi_cash_mix_cum02$cash_mix)
ggplot(hi_cash_mix_cum02, aes(x = inc_rate, color = cash_mix)) + geom_point(aes(y = cum_arrear_rate))  + geom_line(aes(y = pred_arrear))

hi_cash_mix_cum01$month <- '2018-Jan'
hi_cash_mix_cum02$month <- '2018-Feb'
hi_cash_mix_cum03$month <- '2018-Mar'
###plot actual by month
head(hi_cash_mix_cum03)
hi_cash_mix_cum_all <- rbind(hi_cash_mix_cum01, hi_cash_mix_cum02, hi_cash_mix_cum03)
head(hi_cash_mix_cum_all)
ggplot(filter(hi_cash_mix_cum_all, cash_mix >0.8), aes(x = inc_rate, y = cum_arrear_rate, color = month)) + geom_point() 

#validation using new data

a <- co02[["a"]]
b1 <- co02[["b1"]]
b2 <- co02[["b2"]]
str(hi_cash_mix_cum02)
#hi_cash_mix_cum02$pred_arrear <- predict(nls(cum_arrear_rate ~ f(inc_rate,cash_mix,a,b1,b2), data = hi_cash_mix_cum02, start = list(a=3, b1=38,b2=15)))
hi_cash_mix_cum03$pred_arrear <- mapply(function(x1,x2,a,b1,b2) {a * exp(-b1*x1 + b2*x2) }, hi_cash_mix_cum03$inc_rate,hi_cash_mix_cum03$cash_mix,a,b1,b2)
head(hi_cash_mix_cum03)

ggplot(hi_cash_mix_cum03, aes(x = inc_rate, color = f_cash_mix)) + geom_point(aes(y = cum_arrear_rate))  + geom_line(aes(y = pred_arrear))




num<- as.numeric(0.9)

f_model <- function(data, num) {
train <- filter(hi_cash_mix_cum, cash_mix == num)
fit <- glm(log(cum_arrear_rate+0.0001) ~ inc_rate, data=train)  
summary(fit)

ggplot(train, aes(x = inc_rate, y = cum_arrear_rate)) + geom_point() + 
  geom_line(aes(x=inc_rate, y=exp(fit$fitted.values)), color = "red") +
  ggtitle(paste("model fit for cash_mix:", cash_mix))

}
head(train)

fit_0.8 <- f_model(hi_cash_mix_cum, 0.8)
fit_0.8

fit_0.9 <- f_model(hi_cash_mix_cum, 0.9)
fit_0.9

fit_1 <- f_model(hi_cash_mix_cum, 1)
fit_1



