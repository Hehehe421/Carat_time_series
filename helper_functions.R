#Some user defined function

#IQR outlier identifier
FindIQROutliers <- function(data_var, multiplier) {
  #var_v = eval(substitute(var),eval(data))
  lowerq = quantile(data_var, na.rm = T)[2]
  upperq = quantile(data_var, na.rm = T)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  
  # we identify multiplier outliers
  multiplier.threshold.upper = (iqr * multiplier) + upperq
  multiplier.threshold.lower = lowerq - (iqr * multiplier)
  result <- which(data_var > multiplier.threshold.upper | data_var < multiplier.threshold.lower)
  
}


#STD outlier identifier
FindSTDOutliers <- function(data,multiplier) {
  mean = mean(data, na.rm=TRUE)
  std = sd(data, na.rm=TRUE)
  # we identify multiplier outliers
  multiplier.threshold.upper = (std * multiplier) + mean
  multiplier.threshold.lower = mean - (std * multiplier)
  result <- which(data > multiplier.threshold.upper | data < multiplier.threshold.lower)
}


#Plot the boxplot and histogram with/without Outliers
PlotOutliers <- function(data, var, outlier){
  var_v = eval(substitute(var),eval(data))
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_v, main="With outliers")
  hist(var_v, main="With outliers", xlab=NA, ylab=NA)
  
  var_name <- eval(substitute(var),eval(data[!outlier,]))
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
}

library(lubridate)
v <- data.frame(Date_hour = seq(from = as.POSIXct("2016-06-01 00:00"), 
         to = as.POSIXct("2016-08-15 00:00"), by = "2 hour"))

library(prophet)
require(ggpubr)

p1 = qplot(x = 1, y = log_act, data = df_uuid, xlab = "", geom = 'boxplot') + 
  coord_flip() + theme_bw()
p2 = ggplot( data = df_uuid, aes(x=log_act)) + geom_histogram(binwidth = 1,fill = "skyblue2", color = "dodgerblue2") + theme_bw()
ggarrange(p2, p1, heights = c(2, 1), align = "hv", ncol = 1, nrow = 2)

p1 = qplot(x = 1, y = act, data = df_uuid, xlab = "", geom = 'boxplot') + 
  coord_flip() + theme_bw()
p2 = ggplot( data = df_uuid, aes(x=act)) + geom_histogram(binwidth = 100000,fill = "skyblue2", color = "dodgerblue2") + theme_bw()
p3 = qplot(x = 1, y = act, data = result, xlab = "", geom = 'boxplot') + 
  coord_flip() + theme_bw()
p4 = ggplot( data = result, aes(x=act)) + geom_histogram(binwidth = 40000,fill = "skyblue2", color = "dodgerblue2") + theme_bw()
#ggarrange(p2, p1,p4,p3, heights = c(2, 2), align = "hv", ncol = 2, nrow = 2)

grid.arrange(p2,p4,p1,p3, ncol = 2)

p4

