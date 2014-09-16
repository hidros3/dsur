mushroom <- read.csv(file.choose(), header = T)
p1 = 4488/(4488+3928)
p2 = 3928/(4488+3928)
entropyMushroom <- -(p1*log2(p1)+p2*log2(p2))

install.packages("outliers")
library(outliers)

dlf<-read.delim(file.choose(), header = T)
day1 <- outlier(x = dlf$day1, opposite = F, logical = F)
out2 <- outlier(x = dlf$day2, opposite = F, logical = F)
out3 <- outlier(x = dlf$day3,opposite = F, logical = F)

dlf$day1[dlf$day1 == 20.02] <- NA
dlf$day2[dlf$day2 == 3.45] <- NA
dlf$day2[dlf$day2 == 3.41] <- NA

library(ggplot2)
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
