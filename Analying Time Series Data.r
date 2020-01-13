install.packages("boot")
library(boot)
data("manaus")
?manaus

hist(as.numeric(manaus))

#Empirical CDF
manaus.emp <- ecdf(manaus)
n<- length(manaus)
min(manaus)
max(manaus)
grid=seq(-7,6,length.out=1000)
manaus.lower <- (manaus.emp(grid) - sqrt((1/2*n)*log(2/.05)))
manaus.lower<-pmax(manaus.lower, 0)
manaus.upper <- (manaus.emp(grid) + sqrt((1/2*n)*log(2/.05)))
manaus.upper<-pmin(manaus.upper, 1)
plot(ecdf(manaus),xlab='X',ylab='ecdf function')
lines(grid, manaus.lower)
lines(grid, manaus.upper)

#Bootstrap standard errors and confidence intervals
num_resamples<- 1000
block_length<- round(length(manaus)^(1/3))
manaus.tsboot<- tsboot(manaus, median, R = num_resamples, l = block_length, sim = "fixed")
manaus.median<-median(as.numeric(manaus))

#Normal confidence interval
manaus.tsboot.se<-sd(manaus.tsboot$t)
CI.normal<-c(manaus.median-1.96*manaus.tsboot.se, manaus.median+1.96*manaus.tsboot.se)

#Pivotal confidence interval
CI.pivotal<-2*manaus.median-quantile(manaus.tsboot$t,probs = c(0.975, 0.025))

#Quantile confidence inteval
CI.quantile<-quantile(manaus.tsboot$t,probs = c(0.025, 0.975) )

#MLE 
mu<- mean(manaus)  
# Wald test for Hypothesis Testing
## First 45 years data
manaus_num <- as.data.frame(as.numeric(manaus))
manaus_first <-manaus_num[1:540,1]
manaus_first_median <- median(manaus_first)
manaus_first_variance <- sd(manaus_first)*sd(manaus_first)
 
##Last 45 years data
manaus_second <-manaus_num[541:1080,1]
manaus_second_median <- median(manaus_second)
manaus_second_variance <- sd(manaus_second)*sd(manaus_second)
 
z_statistic <- (manaus_first_variance - manaus_second_variance)/sqrt(manaus_first_variance+manaus_second_variance)
 
pnorm(z_statistic)
