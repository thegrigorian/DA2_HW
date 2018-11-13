
# CLEAR MEMORY
rm(list=ls())

# loading all the required packages
library(plyr)
require(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(data.table)
library(grid)
library(lspline)
library(gridExtra)
library(scales)
library(grid)
library(RColorBrewer)

########################################################
# setting the directory
dir <-  "/Users/user/Desktop/DA2/"

# functions for inout and output data
data_in <- paste0(dir,"cases_studies_public/hotels/clean/")
data_out <- paste0(dir,"textbook_work/hw1/hotels/")
output <- paste0(dir,"textbook_work/hw1/hotels/output/")

#########################################################
#LOADING DATA

# hotel_price.csv is in datau and hotels_features.csv is df2
# we join df2 to datau
datau <- read.csv(paste0(data_in,"hotels_price.csv"),
                  stringsAsFactors = F)
df2 <- read.csv(paste0(data_in,"hotels_features.csv"),
                stringsAsFactors = F)
datau <- join(datau, df2, by = "hotel_id")
rm(df2)  

#########################################################
# FILTERING

# filter for hotels and hostels with at least 2 starts in Barcelona
datau <- datau[(datau$accommodation_type == "Hotel" | datau$accommodation_type == "Hostel") & datau$stars >= 2, ]
datau<-subset(datau,city=="Barcelona")

datau[datau$accommodation_type=="Hostel",]

# create the log for price and distance for logarithmic transformations
datau$lnprice <- log(datau$price)
datau$lndistance<-log(datau$distance+0.1)



#########################################################
# PRICE

# summary of the price distribution
summary(datau$price)

# plot the distribution

ggplot(data = datau, aes(x = price)) +
  
  # draw the histogram
  geom_histogram(aes(y = ..density..), binwidth = 20, center=12.25, closed="left", fill = "blue", color = "black", alpha = 0.8) +
  
  # add the labels and the title 
  labs(x = "Hotel price (EUR)", y = "Frequency") +
  ggtitle("Distribution of Hotel Prices in Barcelona") +
  
  # setting limits and grid on the coordinate system 
  coord_cartesian(xlim = c(0, 400)) +
  background_grid(major = "y", minor = "y", size.major = 0.2) +

  # fitting the kernel density curve and adding quartiles, mean and median
  geom_density() +
  geom_vline(aes(xintercept = median(price, na.rm = T)),
             colour = "red", linetype ="longdash", size = .8) +
  annotate("text", x = 158.0, y = 0.005, label ="median") +
  
  geom_vline(aes(xintercept = summary(datau$price)[2]),
           colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 101.0, y = 0.006, label ="Q1") +
  
  geom_vline(aes(xintercept = summary(datau$price)[5]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 259.0, y = 0.006, label ="Q3") +
  
  geom_vline(aes(xintercept = summary(datau$price)[4]),
             colour = "yellow", linetype ="longdash", size = .8) +
  annotate("text", x = 218.1, y = 0.004, label ="mean") 

# mean > mean, highly skewed, which can be corrected by using a logarithmic transformation

#save the plot
ggsave(paste0(output, "hw1_histprice_R.png"), width=10, height=7.5, device='png')


# summary of the distribution
summary(datau$lnprice)

# plot of the distribution logarithmic transformation of price
# because price distribution was highly skewed

ggplot(data = datau, aes(x = lnprice)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, closed="left", fill = "blue", col = "black", alpha = 0.8) +
  labs(x = "Hotel price in logs", y = "Frequency") +
  ggtitle("Distribution of Hotel Prices (ln) in Barcelona") +
  
  #adding the quartiles, mean and median
  
  geom_vline(aes(xintercept = median(lnprice, na.rm = T)),
             colour = "red", linetype ="longdash", size = .8) +
  annotate("text", x = 5.063, y = 0.5, label ="median") +
  
  geom_vline(aes(xintercept = summary(datau$lnprice)[2]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 4.615, y = 0.6, label ="Q1") +
  
  geom_vline(aes(xintercept = summary(datau$lnprice)[5]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 5.557, y = 0.6, label ="Q3") +
  
  geom_vline(aes(xintercept = summary(datau$lnprice)[4]),
             colour = "yellow", linetype ="longdash", size = .8) +
  annotate("text", x = 5.134, y = 0.4, label ="mean") 
  
ggsave(paste0(output, "hw1_histlnprice_R.png"), width=10, height=7.5, device ="png")

#the shape is much closer to normal distribution 

#########################################################
# DISTANCE

# summary of distribution
summary(datau$distance)

# draw and save the histogram of distance 
ggplot(data = datau, aes(x = distance)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.8) +
  labs(x = " Distance (km)", y = "Frequency") +
  ggtitle("Distribution of Hotel Distances in Barcelona" ) +
  
  geom_vline(aes(xintercept = median(distance, na.rm = T)),
             colour = "red", linetype ="longdash", size = .8) +
  annotate("text", x = 1.200, y = 0.5, label ="median") +
  
  geom_vline(aes(xintercept = summary(datau$distance)[2]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 0.600, y = 0.6, label ="Q1") +
  
  geom_vline(aes(xintercept = summary(datau$distance)[5]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 2.000, y = 0.6, label ="Q3") +
  
  geom_vline(aes(xintercept = summary(datau$distance)[4]),
             colour = "yellow", linetype ="longdash", size = .8) +
  annotate("text", x = 1.756, y = 0.4, label ="mean") 

ggsave(paste0(output, "hw1_histdist"), width=10, height=7.5, device = "png")

# the distribution is right-skewed due to the extreme values 
# the extreme values are not due to error

# doing logarithmic transformation of distance
# not interpretable

# summary of distribution
summary(datau$lndistance)

#draw and save the histogram of distance in logs
ggplot(data = datau, aes(x = lndistance)) +

  # adding labels and titles
  geom_density() +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.8) +
  labs(x = " Distance (km)", y = "Frequency")+
  ggtitle("Distribution of Hotel Distances (ln) in Barcelona" ) +
  
  # adding the quartiles, mean and median
  geom_vline(aes(xintercept = median(lndistance, na.rm = T)),
             colour = "red", linetype ="longdash", size = .8) +
  annotate("text", x = 0.2624, y = 0.5, label ="median") +
  
  geom_vline(aes(xintercept = summary(datau$lndistance)[2]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = -0.3567, y = 0.6, label ="Q1") +
  
  geom_vline(aes(xintercept = summary(datau$lndistance)[5]),
             colour = "green", linetype ="longdash", size = .8) +
  annotate("text", x = 0.7419, y = 0.6, label ="Q3") +
  
  geom_vline(aes(xintercept = summary(datau$lndistance)[4]),
             colour = "yellow", linetype ="longdash", size = .8) +
  annotate("text", x = 0.2625, y = 0.4, label ="mean") 

# save the plot
ggsave(paste0(output, "hw1_histlndist"), width=10, height=7.5, device = "png")


# cutoff is the 3rd quartile, as very far or not far
datau$distfar <- as.numeric(datau$distance>=2)

#########################################################
# REGRESSION

# regress price on distfar binary variable (level-level regressions)
reg1 <- lm(price ~ distfar, data=datau)
summary(reg1)

ggplot(data = datau, aes(x = distfar, y = price)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_smooth(method="lm", colour="black", se=F, size=1.5)+
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Far from city center (more than 2km)",y = "Hotel price (EUR)")
ggsave(paste0(output, "hw1_linreg1"), width=10, height=7.5, device = "png")

rsqr1 <- 0.01441
pvalue1 <-1.377e-12

# regress lnprice on distfar binary variable (log-level regressions)
reg2 <- lm(lnprice ~ distfar, data=datau)
summary(reg2)

ggplot(data = datau, aes(x = distfar, y = lnprice)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_smooth(method="lm", colour="black", se=F, size=1.5)+
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Far from city center (more than 2km)",y = "Hotel price (EUR)")

ggsave(paste0(output, "hw1_linreg2"), width=10, height=7.5, device = "png")


rsqr2 <- 0.03137
pvalue2 <-12.2e-16

#lowless nonparametric regression of price on distance

reg3 <- loess(price ~ distance, datau)
reg3
summary(reg3)

ggplot(data = datau, aes(x = distance, y = price)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 1000)) +
  labs(x = "Distance to city center (km)",y = "Hotel price (EUR)") +
  geom_smooth(method="loess", color="black", se = F, size = 1) +
  background_grid(major = "y", minor="y", size.major = 0.2) 

ggsave(paste0(output, "hw1_loess"), width=10, height=7.5, device = "png")


# simple linear regression of price on distance

reg4 <- lm(price ~ distance, data=datau)
summary(reg4)

ggplot(data = datau, aes(x = distance, y = price)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_smooth(method="lm", colour="black", se=F, size=1.5)+
  labs(x = "Far from city center (more than 2km)",y = "Hotel price (EUR)")
ggsave(paste0(output, "hw1_linreg4"), width=10, height=7.5, device ="png")
rsqr4 <- 0.01005
pvalue4 <-3.416e-09


# simple linear regression of lnprice on distance

reg5 <- lm(lnprice ~ distance, data=datau)
summary(reg5)

ggplot(data = datau, aes(x = distance, y = lnprice)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_smooth(method="lm", colour="black", se=F, size=1.5)+
  labs(x = "Far from city center (more than 2km)",y = "Hotel price (EUR)")
ggsave(paste0(output, "hw1_linreg5"), width=10, height=7.5, device = "png")

rsqr5 <- 0.03213
pvalue5 <-2.2e-16

# linear regression with splines

reg6 <- lm(price ~ lspline(distance, c(1,2,4)), data=datau)
summary(reg6)
datau$pred3<-predict(lm(price ~ lspline(distance, c(1,2,4)), data=datau))

ggplot(data = datau, aes(x = distance, y = price)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_line(data = datau, aes(x = distance, y = pred3), colour = "black", size = 1.5)+ 
  geom_vline(xintercept = 1, color="grey", size=1) +
  geom_vline(xintercept = 4, color="grey", size=1) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 400)) +
  scale_x_continuous(limits=c(0,7), breaks=0:7) +
  labs(x = "Distance to city center (km)",y = "Hotel price (EUR)")+
  background_grid(major = "y", minor="y", size.major = 0.2) 
ggsave(paste0(output, "hw1_nonlinreg6.png"), width=10, height=7.5)

rsqr6 <- 0.02432
pvalue6 <-2.2e-16

#polynomials
datau$distance_sq<-datau$distance^2
datau$distance_cub<-datau$distance^3


#QUADRATIC
reg7 <- lm(price ~ distance + distance_sq, data=datau)
summary(reg7)
datau$pred_quad<-predict(lm(price ~ distance + distance_sq, data=datau))

ggplot(data = datau, aes(x = distance, y = price)) +
  geom_point(size = 2, fill = "#0000FF", color = "blue", shape = 4, stroke = 2) +
  geom_line(data = datau, aes(x = distance, y = pred_quad), colour = "black", size = 1.5)+ 
  geom_vline(xintercept = 1, color="grey", size=1) +
  geom_vline(xintercept = 4, color="grey", size=1) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 400)) +
  scale_x_continuous(limits=c(0,7), breaks=0:7) +
  labs(x = "Distance to city center (km)",y = "Hotel price (EUR)")+
  background_grid(major = "y", minor="y", size.major = 0.2) 
ggsave(paste0(output, "hw1_quadreg7.png"), width=10, height=7.5)

rsqr7 <- 0.01989
pvalue7 <-8.178e-16

#CUBIC
reg8 <- lm(price ~ distance + distance_sq + distance_cub, data=datau)
summary(reg8)
datau$pred_cub<-predict(lm(price ~ distance + distance_sq + distance_cub, data=datau))

ggplot(data = datau, aes(x = distance, y = price)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_line(data = datau, aes(x = distance, y = pred_cub), colour = "black", size = 1.5)+ 
  geom_vline(xintercept = 1, color="grey", size=1) +
  geom_vline(xintercept = 4, color="grey", size=1) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 400)) +
  scale_x_continuous(limits=c(0,7), breaks=0:7) +
  labs(x = "Distance to city center (km)",y = "Hotel price (EUR)")+
  background_grid(major = "y", minor="y", size.major = 0.2) 
ggsave(paste0(output, "hw1_cubreg8.png"), width=10, height=7.5)


rsqr8 <- 0.01997
pvalue8 <-4.854e-15

#CUBIC log
reg9 <- lm(lnprice ~ distance + distance_sq + distance_cub, data=datau)
summary(reg9)
datau$pred_cub2<-predict(lm(lnprice ~ distance + distance_sq + distance_cub, data=datau))

ggplot(data = datau, aes(x = distance, y = lnprice)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 4, stroke = 2) +
  geom_line(data = datau, aes(x = distance, y = pred_cub2), colour = "black", size = 1.5)+ 
  coord_cartesian(xlim = c(0, 7), ylim = c(3.5, 6)) +
  scale_x_continuous(limits=c(0,7), breaks=0:7) +
  labs(x = "Distance to city center (km)",y = "Hotel price in logs")+
  background_grid(major = "y", size.major = 0.2) 
ggsave(paste0(output, "hw1_lncubreg6.png"), width=10, height=7.5)


rsqr9 <- 0.04172
pvalue9 <-2.2e-16

paste("Simple linear regression: level-level (price ~ distfar); R-square =", rsqr1, " P-value =", pvalue1)
paste("Simple linear regression: log-level (lnprice ~ distfar); R-square =", rsqr2, " P-value =", pvalue2)
paste("Simple linear regression: level-level (price ~ distance); R-square =", rsqr4, " P-value =", pvalue4)
paste("Simple linear regression: log-level (lnprice ~ distance); R-square =", rsqr5, " P-value =", pvalue5)
paste("Linear regression with splines: level-level (price ~ distance); R-square =", rsqr6, " P-value =", pvalue6)
paste("Linear regression with quadratic polynomial: level-level (price ~ distance); R-square =", rsqr7, " P-value =", pvalue7)
paste("Linear regression with cubic polynomial: level-level (price ~ distance); R-square =", rsqr8, " P-value =", pvalue8)
paste("Linear regression with cubic polynomial: log-level (lnprice ~ distance); R-square =", rsqr9, " P-value =", pvalue9)

# simple linear regression (log-level) and regression with cubic function (log-level) have higher r-square 
# and more significance (lower p-value)