#Sean Shelley X00121161 | Robert Osborne X00109797

# install.packages("ggplot2")
# install.packages("ggloop")
# install.packages("corrplot")
# install.packages("e1071")

library("ggplot","ggloop","corrplot","e1071")

#Mode function. https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

BANKINGrel<-read.csv(file = "c:/BANKINGrel.csv", header=TRUE) #Imports BANKINGrel.csv


v = (1:ncol(BANKINGrel))

NumberOfRecords = nrow(BANKINGrel)

l <- list();

TypeV <-c()
MaxV <- c()
MinV <- c()
MeanV <- c()
MedianV <- c()
ModeV <- c()
SDV <- c()
SkewV <- c()
KurtV <- c()
SkewTypeV <- c()
KurtTypeV <- c()

NameV <- c("Age", "Job", "Marital Status", "Education Level", "Housing Loan", "Personal Loan", "Contact Type", "Month", "Day", "Duration", "Campaign", "Days Passed", "Previus Contacts", "Previous Outcome", "CPI", "CCI", "Euribor", "No. Employees", "Subscribed")

NameF = factor(NameV)

MissingV <-c()

for(i in v)
{
  if(is.numeric(BANKINGrel[,i]))
  {
    ModeV[i] <-getmode(BANKINGrel[,i])
    MaxV[i] <- max(BANKINGrel[,i], na.rm = TRUE)
    MinV[i] <- min(BANKINGrel[,i], na.rm = TRUE)
    MeanV[i] <- mean(BANKINGrel[,i], na.rm = TRUE)
    SDV[i] <- sd(BANKINGrel[,i], na.rm = TRUE)
    MedianV[i] <- median(BANKINGrel[,i], na.rm = TRUE)
    SkewV[i] <- skewness(BANKINGrel[,i], na.rm = TRUE)
    KurtV[i] <- kurtosis(BANKINGrel[,i], na.rm = TRUE)
    
    if(SkewV[i] < 0) {SkewTypeV[i] = "Skew Left"}
    else if (SkewV[i] > 0) {SkewTypeV[i] = "Skew Right"}
    else {SkewTypeV[i] = "Normal"}
    
    if(KurtV[i] < 0) {KurtTypeV[i] = "Platykurtic"}
    else if(KurtV[i] > 0) {KurtTypeV[i] = "Leptokurtic"}
    else {KurtTypeV[i] = "Mesokurtic"}
  }
  
  if(!is.numeric(BANKINGrel[,i]))
  {
    resultMode <- getmode(BANKINGrel[,i])
    ModeV[i] <-as.character(resultMode)
    MaxV[i] <- NA
    MinV[i] <- NA
    MeanV[i] <- NA
    SDV[i] <- NA
    MedianV[i] <- NA
    SkewV[i] <- NA
    KurtV[i] <- NA
    SkewTypeV[i] <- NA
    KurtTypeV[i] <- NA
  }
  
  TypeV[i] <- class(BANKINGrel[,i])
  
  MissingV[i] <- ((sum(is.na(BANKINGrel[,i])) / NumberOfRecords) * 100)
  
}

results.data <- data.frame(NameF,TypeV,MaxV,MinV,MeanV,MedianV,SDV,ModeV,MissingV,SkewV,KurtV,SkewTypeV,KurtTypeV)

p <- ggplot(BANKINGrel, aes(x = y ))

#Histograms Charts - Categories by Target 
hists <- ggloop(BANKINGrel, aes_loop(x = age:nr.employed, fill = y))
hists %L+% stat_count(width = 0.1)

#Bar Charts - Categories by Target 
bars <- ggloop(BANKINGrel, aes_loop(x = age:nr.employed, fill = y))
bars %L+% geom_bar()


ColNo = 10
  
ggplot(BANKINGrel, aes(BANKINGrel[,ColNo], fill = y))+ geom_histogram(binwidth = 10, col="black")+ ggtitle(paste("Histogram for", colnames(BANKINGrel[ColNo])))+ labs(x=colnames(BANKINGrel[ColNo]), y="Count")


# Creating Scatter Plots for all pairs of numeric values.
g <- ggloop(BANKINGrel.numeric, aes_loop(x = age:nr.employed, y = age:nr.employed))
  
g %L+% geom_point()

#Save Images/ Plots to a file
#ggsave(g,"g.png");
# ggsave("plot", g %L+% geom_point(), device = "png", path = "C:/Users/Robert/Pictures/Plots",
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE)


BANKINGrel.numeric <- BANKINGrel[,sapply(BANKINGrel, is.numeric)]

for(i in 1:ncol(BANKINGrel.numeric)){
  BANKINGrel.numeric[is.na(BANKINGrel.numeric[,i]), i] <- mean(BANKINGrel.numeric[,i], na.rm = TRUE)
}

M <- cor(BANKINGrel.numeric)
corrplot(M, type="upper",method = "shade")

# Z-Score
zscore.duration <-(BANKINGrel$duration - mean(BANKINGrel$duration ))/sd(BANKINGrel$duration )
summary(zscore.duration)

#Natural Log Transformation
natlog.duration <- log(BANKINGrel$duration)
summary(natlog.duration)

#Square Root Transformation
sqrt.duration <- sqrt(BANKINGrel$duration)
summary(sqrt.duration)

#Inverse Square Root Transformation
invsqrt.duration <- 1/sqrt(BANKINGrel$duration)
summary(invsqrt.duration)



SkewVPlot<- skewness(zscore.duration, na.rm = TRUE)
KurtVPlot<- kurtosis(zscore.duration, na.rm = TRUE)

SkewSqrtPlot<- skewness(sqrt.duration, na.rm = TRUE)
KurtSqrtPlot<- kurtosis(sqrt.duration, na.rm = TRUE)

SkewInVPlot<- skewness(invsqrt.duration, na.rm = TRUE)
KurtInvPlot<- kurtosis(invsqrt.duration, na.rm = TRUE)

SkewNatPlot<- skewness(natlog.duration, na.rm = TRUE)
KurtNatPlot<- kurtosis(natlog.duration, na.rm = TRUE)

SkewVPlot
KurtVPlot

SkewSqrtPlot
KurtSqrtPlot

SkewInVPlot
KurtInvPlot

SkewNatPlot
KurtNatPlot

#Create a vector of values for binning
xdata = BANKINGrel[,10]
#get the sample size of the variable
n <- length(xdata)
#Declare number of bins and bin indicator
nbins <- 10
whichbin <- c(rep(0,n))
whichbin
range.xdata <- max(xdata) - min(xdata) +1
binwidth <- round(range.xdata/nbins)
for(i in 1:nbins) {
  for(j in 1:n) {
    if ((i-1)*binwidth < xdata[j] && xdata[j] <= (i)*binwidth)
      whichbin[j] <- i
  }
}
whichbin
xdata

#kmeansclustering <- kmeans(xdata,centers=nbins)
#whichbin <- kmeansclustering$cluster
#whichbin
#xdata





