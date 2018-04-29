#set working directory and remove hashtag on line 2 
#setwd("C:/foo/")
source('fit_brandt_poisson_f.R')
source('plot_SSD.R')
source('plots_one_side.R')
source('openGraphSaveGraph.R')
library(openxlsx)


#read the data file that is stored as an xlsx file
#sheet 1 contains problem and acceptable behaviors 
#subject number is in first column in the xlsx file, outcome in second
#time in the third, phase in fourth
data <- read.xlsx('C:/Users/Computer/Desktop/Single case projects/Smita/Github/Data-rearranged.xlsx', sheet = 1, colNames = TRUE)
final.est <- data.frame()
codes <- read.xlsx('C:/Users/Computer/Desktop/Single case projects/Smita/Github/Data-rearranged.xlsx', sheet = "Code", colNames = TRUE)
x <- matrix(NA, 200000, 8)
nsubjects <- length(unique(data$case))
#change the ratio from beta2/1 to 1/2 in the model for i < 6
plotname <- matrix(NA, 8, 1)
for (i in 1:nsubjects){
  y <- data[data$case==i,]
  y1 <- y[,2] 
  t1 <- sum(y[,4]==0)
  result <- fit_brandt_poisson_f(y1, t1)
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

# data.AB <- read.xlsx('Data-rearranged.xlsx', sheet = 2, colNames = TRUE)
# nsubjects.AB <- length(unique(data.AB$case))
# for (i in (nsubjects + 1):(nsubjects+nsubjects.AB))
# Specify the desired comparison value, ROPE radius, and HDI mass:

# Open graphics window and specify layout:
openGraph(width=7.5,height=3.5)
par(mar=c(3.5,3.5,2,1),mgp=c(2,0.7,0), mfcol = c(4, 2))
for (i in 1:4){
  plotname[i] <- paste(codes[codes$case==i,2], codes[codes$case==i,3], sep = "-")
  xnew <- x[((x[,i]>quantile(x[,i], .02)) & (x[,i]<quantile(x[,i],.98))),i]
  plots_one_side(xnew, compVal = 0.2, ropeRad = 0.0, plotname[i], HDImass = .95)
}
for (i in 5:8){
  plotname[i] <- paste(codes[codes$case==i,2], codes[codes$case==i,3], sep = "-")
  xnew <- x[((x[,i]>quantile(x[,i], .02)) & (x[,i]<quantile(x[,i],.98))),i]
  plots_one_side(xnew, compVal = 1.5, ropeRad = 0.0, plotname[i], HDImass = .95)
}

colnames(final.est)[12] <- "case"
write.csv(final.est, "Poisson-beta-ratio-Brandt-f.csv")

