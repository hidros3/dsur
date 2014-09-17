install.packages("car")
install.packages("pastecs")
install.packages("psych")
install.packages("outliers")

library(car)
library(pastecs)
library(psych)
library(outliers)
library(ggplot2)

dlf<-read.delim(file.choose(), header = T)
out1 <- outlier(x = dlf$day1, opposite = F, logical = F)
out2 <- outlier(x = dlf$day2, opposite = F, logical = F)
out3 <- outlier(x = dlf$day3,opposite = F, logical = F)

dlf$day1[dlf$day1 == out1] <- NA
dlf$day2[dlf$day2 == out2] <- NA
dlf$day3[dlf$day3 == out3] <- NA

qplot(dlf$day1)

hist.day1 <- ggplot(data = dlf, aes(day1)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), color = "black", fill = "white") + labs(x = "Hygiene score on day 1", y = "Density")
hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = T), sd = sd(dlf$day1, na.rm = T)), color = "black", size =1)

hist.day2 <- ggplot(data = dlf, aes(day2)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), color = "black", fill = "white") + labs(x = "Hygiene score on day 2", y = "Density")
hist.day2 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day2, na.rm = T), sd = sd(dlf$day2, na.rm = T)), color = "black", size =1)

hist.day3 <- ggplot(data = dlf, aes(day3)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), color = "black", fill = "white") + labs(x = "Hygiene score on day 3", y = "Density")
hist.day3 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day3, na.rm = T), sd = sd(dlf$day3, na.rm = T)), color = "black", size =1)


qqplot.day1 <- qplot(sample = dlf$day1, stat = "qq")
qqplot.day1

qqplot.day2 <- qplot(sample = dlf$day2, stat = "qq")
qqplot.day2

qqplot.day3 <- qplot(sample = dlf$day3, stat = "qq")
qqplot.day3

describe(cbind(dlf$day1, dlf$day2, dlf$day3))
stat.desc(dlf$day1, basic = F, norm = T)

describe(cbind(dlf$day1, dlf$day2, dlf$day3))
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = F, norm = T)

describe(dlf[,c("day1", "day2", "day3")])
stat.desc(dlf[, c("day1","day2", "day3")], basic = F, norm = T)
