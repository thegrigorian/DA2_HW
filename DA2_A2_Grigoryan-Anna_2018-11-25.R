rm(list=ls())

# LOADING REQUIRED LIBRARIES
library(haven)
library(lspline)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(plyr)
library(latticeExtra)
library(countrycode)
library(sjmisc)
library(stargazer)
library(arm)
library(readr)
library(dplyr)
library(lmtest)
library(sandwich)
library(segmented)


# CHANGE IT TO YOUR WORKING DIRECTORY
dir <-  "/Users/user/Desktop/DA2/"

data_in <- paste0(dir,"cases_studies_public/worldbank/clean/")
data_out <- paste0(dir,"textbook_work/hw2/worldbank/")
func <- paste0(dir, "textbook_work/ch00_tech_prep/")
output <- paste0(dir,"textbook_work/hw2/worldbank/output/")

lifeexp <- read.csv(paste0(data_in,"lifeexp.csv"),
                    stringsAsFactors = F)

gdppercap <- read.csv(paste0(data_in,"gdppercap.csv"),
                      stringsAsFactors = F)

data <- join(lifeexp, gdppercap, by = "Country.Code")

rm(lifeexp)
rm(gdppercap)



# CLEAN AND TIDY THE DATA

data<-data[!duplicated(data),]
data <- data[-c(3, 4, 6:8)]
names(data) <- c("countryname", "countrycode", "lifeexp", "gdppc")
data<- as.data.frame(data)
data <- subset(data, countrycode!="")
data$lifeexp <- as.numeric(data$lifeexp)
data$gdppc <- as.numeric(data$gdppc)

# REMOVING ALL THE MISSING VALUES
data<- subset(data, !is.na(data$lifeexp) & !is.na(data$gdppc))

# ADDING THE CONTINENT VARIABLE TO THE DATA
data$continent <- factor(countrycode(sourcevar = data[, "countryname"],
                                     origin = "country.name",
                                     destination = "continent"))

# REMOVING THE ROWS WITH MISSING CONTINENTS
data<- subset(data, !is.na(data$continent))

# TAKING LOG OF GDP PER CAPITA
data$lngdppc <- log(data$gdppc)

##################### LOESS ##################### 

# ESTIMATE A LOESS REGRESSION OF LIFE EXPECTANCY ON LN GDP PER CAPITA
reg_loess <- loess(lifeexp ~ lngdppc, data, control = loess.control(surface = "direct"))
summary(reg_loess)
data$pred_loess <- predict(reg_loess, data)

# VISUALIZE LOESS
ggplot(data = data, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 2, fill = "blue", color = "blue", shape = 1, stroke = 1) +
  geom_smooth(method="loess", colour="black", se=F, size = 1.5)+
  labs(x = "GDP per capita in logs",y = "Life expectancy in years")+
  theme_classic() +
  background_grid(major = "y", minor = "y", size.major = 0.2) 
ggsave(paste0(output, "plot_loess.png"), width=12, height=7.5)


##################### LSPLINE ##################### 

# ESTIMATE A LINEAR REGRESSION OF LIFE EXPECTANCY WITH LINEAR SPLINE 
reg_spline<-lm(lifeexp ~ lspline(lngdppc, c(10.4)),data)
summary(reg_spline)
# R-square is 0.6785
# represents a model that explains 67.85% of the variation in the life expectancy around its mean.

data$pred_spline<-predict(reg_spline,data)
z<-predict(reg_spline,data,se.fit=TRUE)
data$pred_splineSE<- z[[2]]
data$pred_splineCIUP<-data$pred_spline + 2*data$pred_splineSE
data$pred_splineCILO<-data$pred_spline - 2*data$pred_splineSE

ggplot(data = data, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1, fill = "blue", color = "blue", shape = 1, stroke = 1) +
  geom_line(data = data, aes(x = lngdppc, y = pred_spline),  size = 1.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_splineCIUP), linetype = 2,size=0.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_splineCILO), linetype = 2,size=0.5)+
  geom_vline(xintercept = 10.4, color="grey", size=1) +
  labs(x = "GDP per capita in logs",y = "Life expectancy in years") +
  scale_color_manual(name = "", values=c("black"), 
                     labels = c( "Piecevise linear spline")) +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  theme(legend.position="bottom",
        legend.text = element_text(size=16),
        axis.text = element_text(size = 16, color = "black"))
ggsave(paste0(output, "plot_spline.png"), width=12, height=7.5)


coeff_intercept<- reg_spline$coefficients[[1]]

coeff_0_10 <- reg_spline$coefficients[[2]]
# The estimated slope coefficient is 5.8.
# The coefficient indicates that for every additional percent up to 10.4% in GDP per capita
# on average of 5.8 years longer life is expected

coeff_10_12 <- reg_spline$coefficients[[3]]
# The estimated slope coefficient is 2.14.
# The coefficient indicates that for every additional percent from 10.4% in GDP per capita
# on average of 2.14 years longer life is expected

se_coeff_0_10 <- summary(reg_spline)$coefficients[2, 2]
coeff_0_10CIUP <- coeff_0_10 + 2*se_coeff_0_10
coeff_0_10CILO <- coeff_0_10 - 2*se_coeff_0_10
paste(coeff_0_10CILO, coeff_0_10CIUP)

# with 95% confidence for every additional percent up to 10.4% in GDP per capita
# on average 5.13 to 6.49 years longer life is expected
# the confidence interval does not include zero
# with 95% confidence the rate of change of the conditional mean of life expectancy
# with respect to gdp per capita in logs is estimated to be different from 0 

se_coeff_10_12 <- summary(reg_spline)$coefficients[3, 2]
coeff_10_12CIUP <- coeff_10_12 + 2*se_coeff_10_12
coeff_10_12CILO <- coeff_10_12 - 2*se_coeff_10_12
paste(coeff_10_12CILO, coeff_10_12CIUP)

# with 95% confidence for every additional percent from 10.4% in GDP per capita
# on average -1.25 to 2.14 years longer life is expected
# the confidence interval includes zero
# with 95% confidence the rate of change of the conditional mean of life expectancy
# with respect to gdp per capita in logs is estimated to be not significantly different from 0 


##################### QUADRATIC #####################

# ESTIMATE A LINEAR REGRESSION OF LIFE EXPECTANCY WITH POLYNOMIAL
data$lngdppc_sq<-data$lngdppc^2

reg_quad <- lm(lifeexp ~ lngdppc + lngdppc_sq , data=data)
summary(reg_quad)
# R-square is 0.676
# represents a model that explains 67.6% of the variation in the life expectancy around its mean.

data$pred_quad<-predict(lm(lifeexp ~ lngdppc + lngdppc_sq, data=data))
z<-predict(reg_quad,data,se.fit=TRUE)
data$pred_quadSE<- z[[2]]
data$pred_quadCIUP<-data$pred_quad + 2*data$pred_quadSE
data$pred_quadCILO<-data$pred_quad - 2*data$pred_quadSE

ggplot(data = data, aes(x = lngdppc, y = lifeexp)) +
  geom_point(size = 1, fill = "blue", color = "blue", shape = 1, stroke = 1) +
  geom_line(data = data, aes(x = lngdppc, y = pred_quad), colour = "black", size = 1.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_quadCIUP), linetype = 2,size=1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_quadCILO), linetype = 2,size=1)+
  labs(x = "GDP per capita in logs",y = "Life expectancy in years") +
  scale_color_manual(name = "", values=c("black"), 
                     labels = c( "Quadric polynomial regression")) +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  theme(legend.position="bottom",
        legend.text = element_text(size=16),
        axis.text = element_text(size = 16, color = "black"))
ggsave(paste0(output, "plot_quad.png"), width=12, height=7.5)

coeff_intercept_quad<- reg_quad$coefficients[[1]]

coeff_quad1 <- reg_quad$coefficients[[2]]
# The estimated slope coefficient is 11.9 and it is significant with 95% confidence.

coeff_quad2 <- reg_quad$coefficients[[3]]
# The estimated slope coefficient is -0.357.
# The coefficient indicates slight concavity, however it is not significant with 95% confidence. 

se_coeff_quad1 <- summary(reg_quad)$coefficients[2, 2]
coeff_quad1_CIUP <- coeff_quad1 + 2*se_coeff_quad1
coeff_quad1_CILO <- coeff_quad1 - (2*se_coeff_quad1)
paste(coeff_quad1_CILO,coeff_quad1_CIUP)
# the confidence interval does not include zero
# with 95% confidence the linear rate of change of the conditional mean of life expectancy
# with respect to gdp per capita in logs is estimated to be different from 0 

se_coeff_quad2 <- summary(reg_quad)$coefficients[3, 2]
coeff_quad2_CIUP <- coeff_quad2 + 2*se_coeff_quad2
coeff_quad2_CILO <- coeff_quad2 - (2*se_coeff_quad2)
paste(coeff_quad2_CILO,coeff_quad2_CIUP)
# the confidence interval includes zero
# with 95% confidence the concavity of rate of change of the conditional mean of life expectancy
# with respect to gdp per capita in logs is estimated to be not significant


##################### DIFFERENT IN AFRICA #####################

reg_Africa <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent=="Africa"))
reg_nonAfrica <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent!="Africa"))
stargazer(list(reg_Africa, reg_nonAfrica), 
             digits=3, 
            out=paste(output,"coontinentcomparison.html",sep=""))

data$pred_Africa <- predict(reg_Africa, data)
z <- predict(reg_Africa,data,se.fit=TRUE)
data$pred_AfricaSE <- z[[2]]
data$pred_AfricaCIUP <- data$pred_Africa + 2*data$pred_AfricaSE
data$pred_AfricaCILO <- data$pred_Africa - 2*data$pred_AfricaSE

data$pred_nonAfrica<-predict(reg_nonAfrica, data)
z<-predict(reg_nonAfrica,data,se.fit=TRUE)
data$pred_nonAfricaSE<- z[[2]]
data$pred_nonAfricaCIUP<-data$pred_nonAfrica + 2*data$pred_nonAfricaSE
data$pred_nonAfricaCILO<-data$pred_nonAfrica - 2*data$pred_nonAfricaSE


ggplot(data = data, aes(x = lngdppc, y = lifeexp)) +
  
  geom_line(data = data, aes(x = lngdppc, y = pred_Africa), color = "blue", size = 1.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AfricaCIUP), color = "blue", linetype = 2,size=0.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AfricaCILO), color = "blue", linetype = 2,size=0.5)+
  annotate("text", x = 10, y = 82, label ="African") +
  
  geom_line(data = data, aes(x = lngdppc, y = pred_nonAfrica), color = "black", size = 1.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_nonAfricaCIUP), color = "black", linetype = 2,size=0.5)+
  geom_line(data=data,aes(x = lngdppc, y = pred_nonAfricaCILO), color = "black", linetype = 2,size=0.5)+
  annotate("text", x = 11, y = 65, label ="non-African") +
  
  labs(x = "GDP per capita in logs",y = "Life expectancy in years") +
  background_grid(major = "y", minor = "y", size.major = 0.2) 

ggsave(paste0(output, "plot_comparison"), width=12, height=7.5)

##################### EXTRA ASSINGMENT #####################

reg_Asia <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent=="Asia"))
reg_Americas <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent=="Americas"))
reg_Oceania <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent=="Oceania"))
reg_Europe <- lm(lifeexp ~ lngdppc, data=subset(data,data$continent=="Europe"))
stargazer(list(reg_Africa, reg_Asia, reg_Americas, reg_Oceania, reg_Europe ), 
          digits=3, 
          out=paste(output,"coontinentstall.html",sep=""))
## ASIA

data$pred_Asia <- predict(reg_Asia, data)
z <- predict(reg_Asia,data,se.fit=TRUE)
data$pred_AsiaSE <- z[[2]]
data$pred_AsiaCIUP <- data$pred_Asia + 2*data$pred_AsiaSE
data$pred_AsiaCILO <- data$pred_Asia - 2*data$pred_AsiaSE

## AMERICAS

data$pred_Americas <- predict(reg_Americas, data)
z <- predict(reg_Americas,data,se.fit=TRUE)
data$pred_AmericasSE <- z[[2]]
data$pred_AmericasCIUP <- data$pred_Americas + 2*data$pred_AmericasSE
data$pred_AmericasCILO <- data$pred_Americas - 2*data$pred_AmericasSE

data$pred_Oceania <- predict(reg_Oceania, data)
z <- predict(reg_Oceania,data,se.fit=TRUE)
data$pred_OceaniaSE <- z[[2]]
data$pred_OceaniaCIUP <- data$pred_Oceania + 2*data$pred_OceaniaSE
data$pred_OceaniaCILO <- data$pred_Oceania - 2*data$pred_OceaniaSE

data$pred_Europe <- predict(reg_Europe, data)
z <- predict(reg_Europe,data,se.fit=TRUE)
data$pred_EuropeSE <- z[[2]]
data$pred_EuropeCIUP <- data$pred_Europe + 2*data$pred_EuropeSE
data$pred_EuropeCILO <- data$pred_Europe - 2*data$pred_EuropeSE


ggplot(data = data, aes(x = lngdppc, y = lifeexp)) +
  
  geom_line(data = data, aes(x = lngdppc, y = pred_Africa, color='black'),  size = 1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AfricaCIUP, color='black'),  linetype = 2,size=0.2)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AfricaCILO, color='black'),  linetype = 2,size=0.2)+
 
  
  geom_line(data = data, aes(x = lngdppc, y = pred_Americas, color='red'),  size = 1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AmericasCIUP, color='red'),  linetype = 2,size=0.2)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AmericasCILO, color='red'),  linetype = 2,size=0.2)+

  
  geom_line(data = data, aes(x = lngdppc, y = pred_Oceania, color='blue'),  size = 1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_OceaniaCIUP, color='blue'),  linetype = 2,size=0.2)+
  geom_line(data=data,aes(x = lngdppc, y = pred_OceaniaCILO, color='blue'),  linetype = 2,size=0.2)+

  
  geom_line(data = data, aes(x = lngdppc, y = pred_Europe, color='green'),  size = 1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_EuropeCIUP, color='green'), linetype = 2,size=0.2)+
  geom_line(data=data,aes(x = lngdppc, y = pred_EuropeCILO, color='green'),  linetype = 2,size=0.2)+

  geom_line(data = data, aes(x = lngdppc, y = pred_Asia, color='yellow'), size = 1)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AsiaCIUP, color='yellow'),  linetype = 2,size=0.2)+
  geom_line(data=data,aes(x = lngdppc, y = pred_AsiaCILO, color='yellow'), linetype = 2,size=0.2)+
  
  scale_colour_manual(values=c("black", "red", "yellow", "green", "blue"), 
                        name="Continents",
                        labels=c("Africa", "Americas", "Asia", "Europe", "Oceania")) +
  
  labs(x = "GDP per capita in logs",y = "Life expectancy in years") +
  background_grid(major = "y", minor = "y", size.major = 0.2) 
  
  



