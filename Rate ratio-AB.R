#set working directory and remove hashtag on line 2 
#setwd("C:/foo/")
source('fit_brandt_poisson_f.R')
source('plot_SSD.R')
source('plots_right_side.R')
source('plots_left_side.R')
source('openGraphSaveGraph.R')

#read the data file that is stored as an xlsx file
#sheet 1 contains problem and acceptable behaviors 
#subject number is in first column in the xlsx file, outcome in second
#time in the third, phase in fourth
data <- read.csv('Sam.csv', header = TRUE)
final.est <- data.frame()
nphases <- max(data[,1])
plotname <- matrix(NA, 4, 1)
x <- matrix(NA, 200000, nphases - 1)
for (i in 1:(nphases - 1)){
  y1 <- data[(data[, 1]<(i+2)) & (data[, 1]>i - 1),1:2]
  t1 <- sum(y1[,1]==i)
  result <- fit_brandt_poisson_f(y1[,2], t1)
  samples <- combine.mcmc(result$mcmc)
  x[,i] <- exp(samples[,"mu[2]"] - samples[, "mu[1]"])
  res <- cbind(summary(result), i)
  xnew <- x[((x[,i]>quantile(x[,i], .02)) & 
               (x[,i]<quantile(x[,i],.98))),i]
  rate_ratio <- c(quantile(xnew, c(0.05, 0.5, 0.975)), mean(xnew), sd(xnew),
           rep(NA, 6), i)
  bind <- data.frame(rbind(res, rate_ratio))
  final.est <- rbind(final.est, bind)
}
plot(density(samples[, "rho"]))
# Open graphics window and specify layout:
jpeg("plots.jpg")
#openGraph(width=7.5,height=7.5)

for (i in 1:3){
  if (i %% 2 == 0){
    plotname[i] <- paste0("Phase-", i, ":Phase-", (i + 1))
  xnew <- x[((x[,i]>quantile(x[,i], .02)) & (x[,i]<quantile(x[,i],.98))),i]
  plots_right_side(xnew, compVal = .4, ropeRad = 0.0, plotname[i], HDImass = .95)
  }
  else{
    plotname[i] <- paste0("Phase-", i, ":Phase-", (i + 1))
    xnew <- x[((x[,i]>quantile(x[,i], .02)) & (x[,i]<quantile(x[,i],.98))),i]
    plots_left_side(xnew, compVal = 1/.4, ropeRad = 0.0, plotname[i], HDImass = .95)
  }
}

dev.off()
colnames(final.est)[12] <- "phase"
write.csv(final.est, "Poisson-beta-ratio-Brandt-f.csv")
#to plot the data

data1 <- data[, 1:2]
a <- data1[data1[, 1]==1, 2]
b <- data1[data1[, 1]==2, 2]

maxdim <- max(length(a), length(b))
a <- c(a, rep(NA, maxdim - length(a)))
b <- c(b, rep(NA, maxdim - length(b)))

plot.data <- rbind(a, b)
plot_SSD(plot.data)
data1 <- cbind(data1, c(1:nrow(data1)))
plot(data1[, 3], data1[, 2], type = "l", pch = 5)




