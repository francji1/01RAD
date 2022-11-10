################################## 
##### 01RAD Exercise 8 ##########
#################################
#
# Today's exercise
# Checking Model Assumptions (recap + part 2.)
#  - Homoscedasticity / Heteroscedasticity
#  - Normality
#  - Linearity

######################
######################
# get requirements for this exercise (and previous ones)
list_of_packages <- c("tidyverse", "car", "lattice", "pwr", "MASS","graphics","ICSOutlier",
                      "GGally","colorspace","scatterplot3d","gridExtra","effects",
                      "nortest","lmtest","RColorBrewer","pracma","leaps","ISLR")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# delete graphics
#dev.off()
#par(mfrow = c(1, 1))
############################################################
############################################################

### Recap of the HW from the last exercise

# Imagine  that you are statistical consultants asked 
# to build a marketing plan for next year that will result in
# high product sales. On the basis of this data 
# and your final model answer following questions:

#  1 - Is there a relationship between advertising budget and sales?
#  2 - Which media contribute to sales, i.e. do all three media - TV, radio, and newspapers contribute to sales?
#  3 - Which media generate the biggest boost in sales?
#  4- How strong is the relationship between advertising budget and sales?
#  5 - How much increase in sales is associated with a given increase in TV advertising?
#  6 - How much increase in sales is associated with a given increase in Radio advertising?
#  7 - How accurately can we estimate the effect of each medium on sales?
#  8 - How accurately can we predict future sales?
#  9 - Is there synergy among the advertising media?
# 10 - Imagine you have 100k $, what is the best strategy how to spend it in advertising?
# 11 - How much more pruduct will we sell, if we spend 10k$ in TV and 20k$ in radio advertising?
# 12 - What is the 95% confidence interval of previous question?

# Problem described in the book:  An Introduction to Statistical Learning with Applications in R
#                                https://faculty.marshall.usc.edu/gareth-james/ISL/
# some answers for example of the solution: https://rstudio-pubs-static.s3.amazonaws.com/249959_d71491a56f8242909331dfee0e25b813.html


Advert <- read.table("data/Advert.csv", sep =",", header = T )
head(Advert)
summary(Advert)
str(Advert)
#pairs(Advertising, pch=".") 
ggpairs(Advert[,2:5])

# 3 simple linear regression models
model_tv <-  lm(sales ~ TV, data = Advert) 
summary(model_tv)
model_ra <-  lm(sales ~ radio, data = Advert) 
summary(model_ra)
model_np <-  lm(sales ~ newspaper, data = Advert) 
summary(model_np)

# 1-8
# Simple additive balanced model - all regressors are fixed.
# Each regression coeffcient can be estimated and tested separately.
model0 <-  lm(sales ~ TV*radio*newspaper, data = Advert) # model with all interactions
summary(model0)
confint(model0)
model1 <-  lm(sales ~ TV+radio+newspaper, data = Advert) # model without interactions
summary(model1)
confint(model1)
model2  <-  lm(sales ~ TV*radio, data = Advert) # good enough model ? (see last exercise)
summary(model2)
confint(model2)
# Exercise: compare models with simple mean

model_tv <- lm(sales ~ TV, data = Advert)
model_ra <- lm(sales ~ radio, data = Advert)
model_np <- lm(sales ~ newspaper, data = Advert)
new_data <- data.frame(TV = (seq(0,300,5)), radio = (seq(0,300,5)), newspaper =(seq(0,300,5)))
predictions_tv <- as.data.frame(predict(model_tv,  interval = "predict")) #%>% 
  #rename(fit_tv = fit,
  #       lw_tv = lwr,
  #       lwr_tv = upr)
predictions_ra <- as.data.frame(predict(model_ra,  interval = "predict"))
predictions_np <- as.data.frame(predict(model_np,  interval = "predict"))

ggplot(data = cbind(Advert,predictions_tv), aes(x=TV, y=sales)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE) +
  geom_line( aes(y = lwr), col = "blue", linetype = "dashed") + #lwr prediction interval
  geom_line( aes(y = upr), col = "blue", linetype = "dashed") + #upr prediction interval
  theme_bw()  



# Compute variance inflation factor (chceck Collinearity/Multicolinearity)
vif(model0)
# by hand (your turn)



# 9 -12
model2  <-  lm(sales ~ TV*radio, data = Advert)
summary(model2)
predict(model2, newdata=data.frame(TV=10,radio=20), interval="confidence")
predict(model2, newdata=data.frame(TV=10,radio=20), interval="prediction")

#variability explained by the interaction term.
(summary(model2)$r.squared - summary(model1)$r.squared)/(1 - summary(model1)$r.squared)
# recommendation: keep the hierarchy principle
model_tvra <-  lm(sales ~ TV:radio, data = Advert) 
summary(model_tvra)


#10
advert_tmp         <- list(TV=seq(0,100,by=1),radio=seq(0,100,by=1))
advert_new_data    <- expand.grid(advert_tmp)
advert_new_data$fit  <- predict(model2 ,advert_new_data)

library(ggplot2)

ggplot(advert_new_data, aes(TV, radio)) +
  geom_contour_filled(aes(z = fit),breaks=c(5:25)) +
  labs(title = "Contour plot of sales",
       x="TV",y="radio",
       level = "sales") 




# Back to simple car break to stop data used in Exercise - 4
# four models without intercept: 0 speed => 0 stopping distance
cars_lm1 <- lm(dist ~ -1 + speed,    data = cars)
cars_lm2 <- lm(dist ~ -1 + I(speed^2), data = cars)
cars_lm3 <- lm(dist ~ -1 + speed+ I(speed^2), data = cars)
cars_lm4 <- lm(dist ~ -1 + poly(speed, degree=3, raw=TRUE),data=cars)

summary(cars_lm1)
summary(cars_lm2)
summary(cars_lm3)
summary(cars_lm4)

# Plot Shape of dependence - linearity, polynomial regression, ...
plot(dist ~ speed, data = cars, xlim = c(0,30), ylim = c(0,130),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Speed and Stopping Distances of Car (1920s)",xlab="Speed (mph)", ylab="Stopping Distance (ft)")
abline(cars_lm1, col ="blue4")
lines(seq(0, 30, 0.5), predict(cars_lm2,data.frame(speed = seq(0, 30, 0.5))), col='green')
lines(seq(0, 30, 0.5), predict(cars_lm3,data.frame(speed = seq(0, 30, 0.5))), col='blue')
lines(seq(0, 30, 0.5), predict(cars_lm4,data.frame(speed = seq(0, 30, 0.5))), col='red')
legend("topleft",legend = c("linear","quadratic",
                            "linear+quadratic","polynomial with degree=3"),lty = c(1,1,1,1),col = c("blue4","green","blue","red"))



######
# Repeating measurement - how model with factors looks like? 
with(cars,table(speed))
anova(m1_f<-lm(dist~factor(speed),data=cars))

lm(dist~factor(speed),data=cars)
# factor speed is significant, but we will use speed as covariate !!


# Homoscedasticity: for groups data !!!
# to demonstrate: we will modify data cars
cars2 = cars
cars2$fspeed = cut(cars$speed, breaks=c(0,10,20,30),labels = c("slow","moderate","fast"))
summary(cars2)


# Boxplot
Boxplot(dist~fspeed,col = c(2:4),data=cars2)
mtext("Boxplot of distance for speed as factor", side=3, outer=TRUE, line=-3, font=1, cex=1.6)
anova(m2_f<-lm(dist~fspeed,data=cars2))


# Violin plot
ggplot(data = cars2, aes(x=fspeed, y=dist,fill=fspeed)) + geom_violin(trim=FALSE) +
  geom_point() +
  stat_summary(fun.y=median, geom="point",  size=2, color="red")+
  stat_summary(fun.y=mean,   geom="point",  size=2, color="blue")


# For groups leveneTest, bartlett.test and fligner.test
with(cars2,leveneTest(dist~fspeed)) 
with(cars2,bartlett.test(dist,fspeed)) 
with(cars2,fligner.test(dist,fspeed)) 
##########



# Let choose model 3 and Validate Model Assumptions
# Homoscedasticity 

# graphics tools
# Homoscedasticity 
par(mfrow = c(2, 2))
plot(cars_lm3, pch = 20, col = "blue4", lwd = 2)

par(mfrow = c(2, 2))
plot(cars_lm3, which = 1, pch = 20, col = "blue4", lwd = 2)
plot(cars_lm3, which = 3, pch = 20, col = "blue4",  lwd = 2)
abline(lm(sqrt(abs(rstandard(cars_lm3)))~fitted.values(cars_lm3)), col = "blue", lwd = 2)
plot(cars2$speed, residuals(cars_lm3), pch = 20, col = "blue4",  xlab = "Speed (mph)", ylab = "Residuals",
     main = "Residuals vs Covariate")
lines(lowess(cars2$speed, residuals(cars_lm3)), col = "red3", lwd = 2)
plot(cars2$speed, sqrt(abs(rstandard(cars_lm3))), pch = 20, col = "blue4", bg = "skyblue",
     xlab = "Speed (mph)", ylab = as.expression(substitute(sqrt(abs(yL)), list(yL = as.name("Standardized residuals")))),
     main = "Scale-Location (Speed)")
lines(lowess(cars2$speed, sqrt(abs(rstandard(cars_lm3)))), col = "red3", lwd = 2)
abline(lm(sqrt(abs(rstandard(cars_lm3)))~cars2$speed), col = "blue", lwd = 2)


# for dependence on mean value (residual variance on the response expectation)
# Breusch-Pagan test statistic - library lmtest
bptest(cars_lm3)
bptest(cars_lm3,varformula = ~fitted(cars_lm3),studentize = T)
bptest(cars_lm3,varformula = ~fitted(cars_lm3),studentize = FALSE)
# Score Test for Non-Constant Error Variance (Breusch-Pagan test) library:car
ncvTest(cars_lm3)

# check dependence on covariate 
with(cars,bptest(cars_lm3,varformula = ~speed,studentize = FALSE))
ncvTest(cars_lm3, var.formula = ~speed)


# Normality 

# shapiro.test
# lillie.test
# ad.test
# library(ICSOutlier) jarque.test, anscombe.test

par(mfrow = c(1, 1))
plot(cars_lm3, which = 2, pch = 20, col = "blue4", lwd = 2)
library(nortest)
lillie.test(residuals(cars_lm3))   # Lilliefors test
ks.test(residuals(cars_lm3),"pnorm",0,15)
shapiro.test(resid(cars_lm3))      # Shapiro-Wilk test
shapiro.test(residuals(cars_lm3))  # same
ad.test(resid(cars_lm3))

summary(cars_lm3)

# Independence (Checking uncorrelated errors)
# Assume that data are measured from one car - step-by-step
# check if there is any independence (driver is tired, breaks abraded, ...)

# bgtest library(lmtest)
bgtest(cars_lm3)
dwtest(cars_lm3) # One-sided 
dwtest(cars_lm3, alternative = "two.sided")  # Two-sided 

set.seed(420)
durbinWatsonTest(cars_lm3, alternative = "positive") # One-sided 
durbinWatsonTest(cars_lm3)  # Two-sided 
 

#Plot of delayed residuals
plot(resid(cars_lm3)[-length(resid(cars_lm3))],resid(cars_lm3)[-1],pch = 20, col = "blue",
     xlab = expression(r[i-1]), ylab = expression(r[i]),cex = 1.5) # no autocorelation
lines(lowess(resid(cars_lm3)[-length(resid(cars_lm3))],resid(cars_lm3)[-1]), col = "red3", lwd = 2)
abline(h = 0, col = "grey")




# OLS regression models assume the response variable to be (approximately) normal distributed.
# If non-normality of residuals appears, we can
#  -> transform data to obtain normality and use OLS
#  -> use GLM
###### 



### Exercise
# Read carsdata2.csv from the  exercise 6
#
cars04 <- read.table("data/carsdata2.csv", sep =";",header = T )
cars04 <- na.omit(cars04)

cars04$consumption <- rowMeans(cbind(cars04$CityMPG,cars04$HwyMPG))
cars04$consumption <- 100/(1.60934*(cars04$consumption/3.7854))

cars04$type <- "sedan"
cars04$type[cars04$Sports==1]  <- "sport"
cars04$type[cars04$SUV==1]     <- "suv"
cars04$type[cars04$Wagon==1]   <- "minivan"
cars04$type[cars04$Minivan==1] <- "minivan"
cars04$type[cars04$Pickup==1]  <- "minivan"
cars04$type <- factor(cars04$type)
summary(cars04)

ggpairs(cars04[,c("Weight","HP","EngineSize")])

# With carsdata2.csv data set answer following questions:
#   
#   a) Build regression model and investigate 
#     1. Dependence of Weight on Enginesize (simple regression)
#     2. Dependence of Weight on HP  (simple regression)
#     3. Dependence of Weight on Enginesize and HP (multivariate regression)
#   b) Check Model Assumptions - run all tests and plots mentioned during the erxercise 07
#     - Plot Residuals vs. fitted and covariate in all three models
#     - Plot residual QQ plots to confirm/reject normality of residuals.
#     - Perform statistical hypothesis test to confirm/reject homoscedasticity, normality, independence of residuals.
#   c) Is there any indication of heteroscedasticity?
#   d) Is there any correlation of Enginesize and HP?
#   e) Write final conclusion about model 3.
