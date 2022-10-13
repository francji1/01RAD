################################## 
##### 01RAD Exercise 04 ##########
##################################
#
# Introduction to Simple linear regression in R
#  - solution of minihomework from Exercise 03
#  - lm function
#  - t-test, confidence intervals
#  - residuals, test statistics, ....


######################
# get requirements for this exercise
list_of_packages <- c("tidyverse", "car", "lattice", "pwr", "MASS","GGally","colorspace")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)


######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)


############################################################
############################################################




# Task from the last exercise:
# Investigate a relationship between speed and stopping distance for cars
head(cars)
str(cars)
summary(cars)
#View(cars)

# Questions:
# 1) Display data set (scatter plot, histograms and density plots of speed and dist variables)
# 2) Try model with and without intercept
# 3) Compute "manually" OLS estimate of regression parameters and error variance
# 4) Compute "manually" variance of estimated parameters
# 5) Plot data with both estimated regression lines
# 6) Investigate output from the lm function
# 7) whats the estimated stopping distance for a car going 20mph? (use both models)
#    compare with the estimated stopping distance for a car going 30mph? (use both models, can we predict this?)
# 8) Is the simple linear model good approximation for these problems? Suggest next steps.




# Q1) Dispalay data set (scatter plot, histograms and density plots of speed and dist variables)

# Delete and reset plotting settings
#dev.off()
#par(mfrow = c(1, 1))



#simple R plot data
plot(cars$speed,cars$dist)
plot(cars$dist,cars$speed)

#The same simple plot, but different code
plot(cars$dist~cars$speed)
# tuned plot saved as plot_car.png into the ".../working_directory/plots/"
png('plots/plot_car2.png')
plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "red3", xaxs="i",yaxs="i",
     main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
dev.off()
# tuned plot by ggplot 
plot_car2 <- ggplot(cars, aes(x=speed, y=dist)) + 
    geom_point(size=2, alpha=0.8) + 
    theme_bw() +
    xlab("Speed of car") + 
    ylab("Stopping distance") + 
    ggtitle("Speed and Stopping Distances of Car (1920s)")
plot_car2
ggsave('plots/plot_car_2.png', plot_car2)


p1 <- ggplot(data=cars, aes(speed)) + 
    geom_histogram(aes(y =..density..), 
                   breaks=seq(0, 30, by = 1), 
                   col="red", 
                   fill="gray", 
                   alpha = .2,
                   na.rm = TRUE) + 
    geom_density(col = "red",
                 na.rm = TRUE)+
    labs(title="Histogram for speed") +
    labs(x="Speed (mph)", y="Density") +
    theme_bw()

p2 <- ggplot(data=cars, aes(dist)) + 
    geom_histogram(aes(y =..density..), 
                   breaks=seq(0, 125, by = 5), 
                   col="blue", 
                   fill="gray", 
                   alpha = .2,
                   na.rm = TRUE) + 
    geom_density(col = "blue", na.rm = TRUE)+
    labs(title="Histogram for speed") +
    labs(x="Stopping distance (ft)", y="Density") +
    theme_bw()

p3 <- ggplot(data=cars, aes(speed)) + 
    geom_histogram(breaks=seq(0, 30, by = 1),
                   col="red", 
                   fill="gray", 
                   alpha = .2,
                   na.rm = TRUE) + 
    geom_density(stat = "bin",
                 binwidth = 2,
                 col = "red",
                 na.rm = TRUE)+
    labs(title="Histogram for speed") +
    labs(x="Speed (mph)", y="Count") +
    theme_bw()

p4 <- ggplot(data=cars, aes(dist)) + 
    geom_histogram(breaks=seq(0, 125, by = 5), 
                   col="blue", 
                   fill="gray", 
                   alpha = .2,
                   na.rm = TRUE) + 
    geom_density(stat = "bin",
                 binwidth = 5,
                 col = "blue",
                 na.rm = TRUE)+
    labs(title="Histogram for speed") +
    labs(x="Stopping distance (ft)", y="Count") +
    theme_bw()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2)

# simple way how to plot it
ggpairs(cars)

cars$speed2 <- (cars$speed)^2
head(cars)
# 2) Try model with and without intercept

# simple linear model - simple regression without intercept
cars_lm1 <- lm(dist ~ -1 +speed , data = cars)
# simple linear model - simple regression with intercept
cars_lm2 <- lm(dist ~  speed , data = cars)
# simple linear model - with intercept and one regressor (square of speed)
cars_lm3 <- lm(dist ~  I(speed^2) , data = cars)
cars_lm3b <- lm(dist ~  speed2 , data = cars)

# multiple linear model - with intercept and two regressors (speed and square of speed)
cars_lm4 <- lm(dist ~  speed + I(speed^2) , data = cars)

summary(cars_lm1)
summary(cars_lm2)
summary(cars_lm3)
summary(cars_lm4)


coefficients(cars_lm1)
coefficients(cars_lm2)



# 3) Compute "manually" OLS estimate of regression parameters and error variance
# 4) Compute "manually" variance of estimated parameters

# Regression matrix X, Response variable Y
Y  = cars$dist
X1 = rep(1,times=length(Y))
X2 = cars$speed
X <- cbind(X1,X2)

# model  with intercept
# approach 1
beta_2_lm2 <- cor(X2,Y) * (sd(Y)/sd(X2))
beta_1_lm2 <- mean(Y) - beta_2_lm2*mean(X2)
cbind(beta_1_lm2,beta_2_lm2)
# approach 2
solve(t(X)%*%(X))%*%(t(X)%*%Y)
# approach 3 (from lm())
coefficients(cars_lm2)


# model  without intercept
solve(t(X2)%*%(X2))%*%(t(X2)%*%Y)
coefficients(cars_lm1)


#  variance of parameters
# manual computation of residuals
resid <-   Y - X%*%coefficients(cars_lm2)
summary(resid)
# same
residuals(cars_lm2)
sum(residuals(cars_lm2)==resid)

n <- length(Y)
p <-  2
sigma <- sqrt((1/(n-p))*sum(residuals(cars_lm2)^2))
summary(cars_lm2)
summary(cars_lm2)$sigma


# check
MSE  <- sum(resid^2)/(length(resid)) # MSE - biased MLE of residual variance
RMSE <- sqrt(MSE)                    # Root Mean Square Error (RMSE) 

# IN Regression:
# The term MSE can be used to refer to the unbiased estimate of error variance too: 
# Unbiased Pearson estimation of residual variance
sigma2  <- sum(resid^2)/(length(resid)-2)    
sigma   <- sqrt(sigma2) 


var_b2 <- sigma^2*(1/sum((X2 - mean(X2))^2))
sd_b2  <- sqrt(var_b2)
var_b2
sd_b2


var_b1  <- sigma^2*(sum(X2^2)/(n*sum((X2 - mean(X2))^2)))
sd_b1   <- sqrt(var_b1)
var_b1
sd_b1

# compare with 
summary(cars_lm2)$coefficients



# 5) Plot data with both estimated regression lines


plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20,
    col = "black", xaxs="i",yaxs="i",
    main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
abline(cars_lm1, col ="blue4")
abline(cars_lm2, col ="red4")
#lines(sort(cars$speed), fitted(cars_lm3)[order(cars$speed)], col='green')
lines(seq(0, 30, 0.5), predict(cars_lm3,data.frame(speed = seq(0, 30, 0.5))), col='blue')
lines(seq(0, 30, 0.5), predict(cars_lm4,data.frame(speed = seq(0, 30, 0.5))), col='red')
legend("topleft",legend = c("linear","intercept + linear",
                            "quadratic","intercept + linear + quadratic"),
                         lty = c(1,1,1,1),col = c("blue4","red4","blue","red"))




# 6) Investigate output from the lm function
summary(cars_lm2)
cars_lm2$model          # data used in the model
cars_lm2$df.residual    # Residuals degrees of freedom n−r
cars_lm2$fitted.values  # Fitted values of response varaible (here cars$dist)
cars_lm2$fitted.values + cars_lm2$residual == Y

#....
? summary.lm
? lm

# 7) whats the estimated stopping distance for a car going 20mph? (use both models)
#    compare with the estimated stopping distance for a car going 30mph? (use both models, can we predict this?)


coef(cars_lm2) # estimated coefficents
# \hat{\mu}(dist) = -17.58 + 3.93(speed).
# We estimated average stopping distance (in feet) for a car going (speed) mph.
# interpretation of the slope
#    Estimated slope \hat{\beta}_2 = 3.93 represents the increase in average stopping
#    distance for each mile per hour faster that the car drives.
# interpretation of the intercept
#     Estimated intercept \hat{\beta}_1 = -17.58 represents the mean stopping distance 
#     for a car traveling 0 mph (make no sense in our example !!!)
#     Extrapolating can be dangerous and can lead to  nonsensical results.

# estimated stopping distance for a car going 20mph for model with intercept
coef(cars_lm2)[1] + coef(cars_lm2)[2]*20


# estimated stopping distance for a car going 20mph for model without intercept
coef(cars_lm1)*20



# estimation of stopping distance with the same regressors as in the dataset
# all fitted response variables, regressors are the same we used in the estimation
fitted(cars_lm2) 
# comparison with manual fit
cars[1:5,]
hat_dist2a = coef(cars_lm2)[1] + coef(cars_lm2)[2]*cars[1:5,"speed"]   # manually from the definition
hat_dist2b = fitted(cars_lm2)[1:5]                                     # by R function
cbind(hat_dist2a, hat_dist2b)

# if we want to predict response with new data (first 5 are same as in X)
predict(cars_lm2, newdata = data.frame(speed = c(4, 4, 7, 7, 8, 1, 30)))

# investigate residuals
residuals(cars_lm2) # computed residuals
resid(cars_lm2)     # same = computed residuals
cars_lm2$residuals 
# ....                and so on as before 

mean(cars_lm2$residuals)
sqrt(var(cars_lm2$residuals))
sd(cars_lm2$residuals)


# compare build-in function and manual computation
resid_2a = cars[1:5,"dist"] - hat_dist2a   # manually from the definition
resid_2b = residuals(cars_lm2)[1:5]       # by R function
cbind(resid_2a, resid_2b)                  

# Mean Square Error and Standard Error
n = dim(cars)[1]
p = dim(cars)[2] - 1
MSE_2a = sum(residuals(cars_lm2)^2)/(n-p-1) # Mean square error  - manually from the definition
sigma_hat2a = sqrt(MSE_2a)                  # Residual standard error (estimation of standard deviation)
sigma_hat2b = summary(cars_lm2)$sigma       # by R function
cbind(sigma_hat2a, sigma_hat2b)   
sigma_hat = sigma_hat2a
# compare to summary(cars_lm2)


# Interval Estimates of the Parameters
coef(cars_lm2)
confint(cars_lm2)
conf_int = as.data.frame(cbind(confint(cars_lm2)[,1], coef(cars_lm2), confint(cars_lm2)[,2]))
names(conf_int) = c("lower CI 2.5 %", "estimate", "upper CI 97.5 %"  )
conf_int
summary(cars_lm2)$coef
#summary(cars_lm2)$coefficients

# another confidence interval - 99%
confint(cars_lm2, level = 0.99)



# t-test of significance of parameters
Sb_1 = summary(cars_lm2)$coef[1,2]
UCI_1 = coef(cars_lm2)[1]+qt(.975,df = n-2)*Sb_1
LCI_1 = coef(cars_lm2)[1]+qt(.025, df = n-2)*Sb_1

Sb_2 = summary(cars_lm2)$coef[2,2]
UCI_2 = coef(cars_lm2)[2]+qt(.975, df = n-2)*Sb_2
LCI_2 = coef(cars_lm2)[2]+qt(.025, df = n-2)*Sb_2

conf_int_manually = rbind( c(LCI_1,coef(cars_lm2)[1],UCI_1), c(LCI_2,coef(cars_lm2)[2],UCI_2))
conf_int_manually = as.data.frame(conf_int_manually)
names(conf_int_manually) = c("lower CI 2.5 %", "estimateed val.", "upper CI 97.5 %"  )
row.names(conf_int_manually) = c("Intercept","Speed")
conf_int_manually



# t-test of significance of parameters
# H0 hypothesis is beta_i = 0
beta_H0 = 0
tval_1 = (summary(cars_lm2)$coef[1,1]-beta_H0)/summary(cars_lm2)$coef[1,2]
tval_2 = (summary(cars_lm2)$coef[2,1]-beta_H0)/summary(cars_lm2)$coef[2,2]

pval_1 = 2*pt(abs(tval_1), cars_lm2$df.residual, lower.tail = FALSE)
pval_2 = 2*pt(abs(tval_2), cars_lm2$df.residual, lower.tail = FALSE)
coef_manually = rbind( c(coef(cars_lm2)[1], Sb_1, tval_1,pval_1),
                       c(coef(cars_lm2)[2], Sb_2, tval_2,pval_2))
coef_manually
#comapre with 
summary(cars_lm2)$coef


#  Confidence intervals vs. Prediction intervals 

# Confidence intervals tell us about how well we have determined the mean. 
# Prediction intervals tell us  where we can expect to see the next data point sampled.

new_speed =data.frame(speed = seq(0,30,0.1))
new_conf = predict(cars_lm2, newdata = new_speed, interval = "confidence")
new_pred = predict(cars_lm2, newdata = new_speed, interval = "prediction")
plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Speed and Stopping Distances of Car (1920s)",xlab="Speed", ylab="Stopping Distance")
lines(seq(0, 30, 0.1), new_pred[,1], col='black')
lines(seq(0, 30, 0.1), new_pred[,2], col='red')
lines(seq(0, 30, 0.1), new_pred[,3], col='red')
lines(seq(0, 30, 0.1), new_conf[,2], col='blue')
lines(seq(0, 30, 0.1), new_conf[,3], col='blue')
legend("topleft",legend=c("observed","fit","Confidence int","Prediction int"),
       pch=c(20,NA,NA,NA),lty = c(NA,1,1,1),col = c("black","black","blue","red"))

# faster - works only for simple linear regression (lm(y ~ x))
# install.packages("HH")
library(HH)
ci.plot(cars_lm2)


# By qqplot
model <- lm(dist ~ speed, data = cars)
predictions <- as.data.frame(predict(model, interval = "predict"))

p3 <- ggplot(data = cbind(cars,predictions), aes(x=speed, y=dist)) +
    geom_point() +
    geom_smooth(method=lm , color="red", se=TRUE) +
    geom_line( aes(y = lwr), col = "blue", linetype = "dashed") + #lwr prediction interval
    geom_line( aes(y = upr), col = "blue", linetype = "dashed") + #upr prediction interval
    theme_bw()  
p3
# install.packages("hrbrthemes")
#library(hrbrthemes)
   # theme_ft_rc() 
   # theme_bw()



##################################
## Residual analysis - will be covered later in details
#################################

#####
# Normality of residuls 

# Shapiro-Wilk test
shapiro.test(residuals(cars_lm2))
# Kolmogorov-Smirnov test
ks.test(residuals(cars_lm2),"pnorm", 0, sigma_hat2a)             # problem
ks.test(unique(residuals(cars_lm2)),"pnorm", 0, sigma_hat2a)     # problem solved, but bad approach, don't do it !!!!

install.packages("nortest")
library(nortest)
# Lilliefors test
lillie.test(residuals(cars_lm2))
# Anderson-Darling test 
ad.test(residuals(cars_lm2))

######
# Homoscedasticity -  Constant Variance Assumption
library(lmtest)

# Breusch-Pagan test statistic
bptest(cars_lm2)

# Score Test for Non-Constant Error Variance
ncvTest(cars_lm2)
# spreadLevelPlot(cars_lm2) 

cars[c(23,35,49),]

#dev.off()
#graphics.off()

opar <- par(mfrow=c(2,2))
plot(cars_lm3)
par(opar)

# QQplot
qqnorm(residuals(cars_lm2))
qqline(residuals(cars_lm2), col = 2)

#residuals vs. fitted
plot(residuals(cars_lm2) ~ fitted(cars_lm2))

# standartized residuals vs. fitted
plot(rstandard(cars_lm2) ~ fitted(cars_lm2))

# Cook distance 
cooks.distance(cars_lm2)

# Check possible transoframations of regressors
crPlots(cars_lm2)

##################################
## Comparison of R lm() with numerical linear algebra
## hat matrix, projection matrix, QR decomposition, ... 
## will be covered next time.
#################################




