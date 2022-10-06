################################## 
##### 01RAD Exercise 03 ##########
#################################
#
# Introduction to Simple linear regression in R
#  - visuallisation of data
#  - residuals, test statistics, ....
#

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


### Read required dataset
fsdata = read.csv('data/fsdata.csv', sep = ",",header = T)
# dataset from the package "UsingR"
# library(UsingR)
# data(father.son)
# write.csv(father.son,'fsdata.csv')


#### Check out the structure of the data set
head(fsdata)
str(fsdata)
summary(fsdata)
dim(fsdata)
# other usually checks
# names(fsdata)
# View(fsdata)
colSums(is.na(fsdata))

# define color palette from library(colorspace)
color_6 <- rainbow_hcl(6)
color_1 <- rainbow_hcl(1) 

### Visualize data

hist(c(fsdata$son,fsdata$father), freq = F, col=color_1, breaks = 20,
        main="Histogram and Kernel density plot",xlab="Height", ylab="Density")
lines(density(fsdata$son,na.rm=TRUE),col="red")
lines(density(fsdata$father,na.rm=TRUE),col="blue")
legend("topright",legend = c("son","father"),lty = c(1,1),col = c("red","blue"))


fsdata_long <- fsdata %>% 
    pivot_longer(cols = c("father","son"),
                 names_to = "relation",
                 values_to = "height") %>% 
    rename(family = X)

ggplot(data=fsdata, aes(x=father, y=son)) +
    geom_point() 

ggplot(fsdata_long,aes(x = height, colour=relation)) + 
    geom_histogram(aes(y = ..count.., fill=relation),binwidth = 2,alpha=0.4,position="identity")

ggplot(fsdata_long,aes(x = height, fill=relation, colour=relation)) + 
    geom_histogram(aes(y = ..density.., fill=relation),binwidth = 2,alpha=0.3,position="identity") +
    geom_density(kernel = "gaussian", alpha = 0.2) +
    geom_vline(data= fsdata_long %>% group_by(relation) %>% summarise(mean = mean(height)),
               aes(xintercept=mean,colour=relation),
               linetype="dashed", size=1)
# check ?density parameters to compute reasonable kernel density estimates


### (play around) What if data are not paired?
histogram(~ height | as.factor(relation), data = fsdata_long, type = "density", col = color_1, ylab = "Density")
# The relation ship between fathers and sons height can't be determined without family information 


# lets take data as a generation problem (no relation between son-father)
# Has fathers and sons same variance
# F test 
var.test(fsdata$father,fsdata$son)  
# F test manual
test_stat<- var(fsdata$father)/var(fsdata$son)
df1      <- length(fsdata$father)-1
df2      <- length(fsdata$son)-1
p_val    <- pf(test_stat,df1=df1-1,df2=df2-1)
2*(min(p_val,1-p_val)) # compare with var.test function result

# Is fathers generation tall as sons generation?
t.test(fsdata$father,fsdata$son, paired = F, var.equal = T)
# Are fathers taller than their sons?
t.test(fsdata$father,fsdata$son, paired = T, var.equal = T)

# Question: What are the assumptions for using the t-test?
# Question: # Are sons  taller than fathers
t.test(fsdata$father,fsdata$son, alternative = c("less"),
 paired = T, var.equal = T)

    
    
### Continue plotting

# Basic scatterplot matrix
pairs(fsdata[,2:3])
# Nicer by GGally package
ggpairs(fsdata[,2:3])
ggpairs(fsdata_long)


# Scatterplot of variables of interest by simple plot function
plot(father ~ son,fsdata,
     main="Father and Son Height - Dalton dataset",
     xlab="Height of Son", ylab="Height of Father")


# Use ggplot library: 
ggplot(fsdata, aes(x=father, y=son))  +
    geom_point(size=1, alpha=0.7) +
    xlab("Height of Father") +
    ylab("Height of Son") +
    theme_bw() + 
    ggtitle("Father and Son Height - Dalton dataset")

# Guess regression line (now by "blackbox" function geom_smoots)
ggplot(data=fsdata, aes(x=father, y=son)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", se = T) +
    geom_smooth(method = "lm", formula = y ~ -1 +x, se = T,col = "red")

# Estimate regression line (now by "blackbox" function geom_smoots)
ggplot(data=fsdata, aes(x=father, y=son)) +
    geom_point() +
    geom_smooth(method = "lm", se = T)

# plot including some "now lackbox" fit lines, marginal box plots (need car library)
scatterplot(fsdata$father,fsdata$son)
scatterplotMatrix(fsdata[,2:3])




### Question: Is there close relationship between father and son height?


# back to old scatter plots 
plot(son ~ father,
     fsdata,
     main="Father and Son Height - Dalton dataset",
     xlab="Height of Son",
     ylab="Height of Father")
abline(0,1)


# Change plot: add line starting in (0,0) and force R to start x,yaxis in 0
plot(son ~ father,fsdata,xlim = c(0,90), ylim = c(0,90),pch=".", xaxs="i",yaxs="i",
     main="Father and Son Height - Dalton dataset",xlab="Height of Son", ylab="Height of Father")
abline(0,1)



############################
### Simple Regression ######
############################

##################################
#### Analyze data by "manually" ###

# mean deviations of the father and son heights
s_mean  <- mean(fsdata$son)
s_mean  == sum(fsdata$son)/(length(fsdata$son)) # same as manually from the definition of sample mean
f_mean  <- mean(fsdata$father)
c(s_mean,f_mean)

# standard deviations of the father and son heights
s_sd   <- sd(fsdata$son)
f_sd   <- sd(fsdata$father)

# correlation of fsdata data (father x son is in interest) 
cor(fsdata$son, fsdata$father)
fs_cor  <- cor(fsdata)[2, 3] 
fs_cor2 <- cov(fsdata$son, fsdata$father)/(s_sd*f_sd)
fs_cor3 <- (((fsdata$son - s_mean)%*%(fsdata$father - f_mean))/(length(fsdata$son)-1))/(s_sd*f_sd)
c(fs_cor,fs_cor2,fs_cor3)

# slope lecture
b1_hat <- ((fsdata$son - s_mean)%*%(fsdata$father - f_mean))/(sum(fsdata$father^2)-length(fsdata$father)*f_mean^2)
b1_hat
# slope alternative 
b1_hat <- fs_cor*(s_sd / f_sd)
b1_hat
# and finally it's the same as
b1_hat <- cov(fsdata$father,fsdata$son)/(sd(fsdata$father))^2
       #   cov(fsdata$father,fsdata$son)/var(fsdata$father)
       #  cor(fsdata$father,fsdata$son)*(sd(fsdata$son)/sd(fsdata$father))
b1_hat

# intercept 
b0_hat <- s_mean - b1_hat*f_mean
b0_hat

# print regression line parameters
beta_hat = rbind(b0_hat,b1_hat)
beta_hat

# do the same with different notation
intercept <- rep(1,length(fsdata$son))
X         <- (fsdata$father)
IX        <- cbind(intercept,X)
Y         <- (fsdata$son)
head(cbind(Y,intercept,X))

b1_hat    <- cor(X,Y) * (sd(Y)/sd(X))
b1_hat
b0_hat    <- mean(Y) - b1_hat*mean(X)
b0_hat



# find residuals
res =  Y - IX%*%beta_hat
summary(res)
# estimate variance of disturbances sigma^2
n=dim(fsdata)[1] # number of observations
p=1              # number of regression coefficients - No. of DF
sigma <- sqrt((1/(n-1-p))*sum((Y - IX%*%beta_hat)^2)) # estimtion of sigma, s_n from the lecture

# same diff notation
MSE  <- sum(res^2)/(n-2)  #  unbiased estimation of sigma^2
RMSE = sqrt(MSE)          #  (biased) estimation of sigma, E[s] <= sigma

#  variance of parameters
S_xx <- sum((X - mean(X))^2)
var_b1_hat <- sigma^2/S_xx
         # <- sigma^2*(1/((n-1)*var(X))) #the same
sd_b1_hat  <- sqrt(var_b1_hat)
var_b1_hat
sd_b1_hat


var_b0_hat  <- sigma^2*(sum(X^2)/(n*sum((X - mean(X))^2)))
sd_b0_hat   <- sqrt(var_b0_hat)
var_b0_hat
sd_b0_hat


##############################
### OLS estimate #############
# A * B 	Element-wise multiplication
# A %*% B 	Matrix multiplication 

# without intercept
beta_hat_wo = solve(t(X)%*%(X))%*%(t(X)%*%Y)
beta_hat_wo
# with intercept
beta_hat_w = solve(t(IX)%*%(IX))%*%(t(IX)%*%Y)
beta_hat_w
################

#############################################
# Compute coefficient of determination  (Was it aready covered on lectures?)
# R-squared = Explained variation / Total variation
SS_tot = sum((Y - mean(Y))^2)
SS_reg = sum((IX%*%beta_hat - mean(Y))^2)
SS_res = sum(res^2)
SS_tot == SS_res+SS_reg

R2 = 1-SS_res/SS_tot
R2
R2 = SS_reg/SS_tot
R2
R2 = (SS_tot - SS_res)/SS_tot
R2
#############################################
SS_tot_wo = sum((fsdata$son)^2)
SS_reg_wo = sum((fsdata$father%*%beta_hat_wo)^2)
res_wo =  fsdata$son - fsdata$father%*%beta_hat_wo
SS_res_wo = sum(res_wo^2)
SS_tot_wo == SS_res_wo+SS_reg_wo

R2_wo = 1-SS_res_wo/SS_tot_wo
R2_wo
R2_wo = SS_reg_wo/SS_tot_wo
R2_wo
R2_wo = (SS_tot_wo - SS_res_wo)/SS_tot_wo
R2_wo
################################### 



#################################
#### Use R lm function !!! ######
#################################
# We will use it all the time !!!

# model without intercept
model0 = lm(son ~ -1 + father ,fsdata)
summary(model0)

# model with intercept
model1 = lm(son ~ father,fsdata)
summary(model1)

# study summary function for lm
help(summary.lm)

## direct print of given variables from lm - objects
## examples - in details next lessons
coef(model1)
model1$coefficients
residuals(model1)
hist(fitted.values(model1))
fitted(model1)
sigmaHat(model1)
# ......


# Q: why R-squared differ in both approaches?
#    Which approach is the right one?

# plot data with regression line
plot(son ~ father,fsdata,
     main="Father and Son Height - Dalton dataset",xlab="Height of Father", ylab="Height of Son ")
abline(model0,col ="blue")
abline(model1, col ="red")
abline(a=mean(fsdata_long$height),b=0)
(mean(fsdata_long$height))
legend("topleft",legend = c("without intercept","with intercept"),lty = c(1,1),col = c("blue","red"))



###### verify assumptions ###########
# Will be covered in details later###
#####################################
# plot residuals - 
layout(matrix(1:4,2,2))
plot(model0)

opar    = par(mfrow=c(1,1))
plot(model1)
par(opar)

qqnorm(residuals(model1))
qqline(residuals(model1), col = 2)

#dev.off()
#graphics.off()

# Post hoc analysis - hypothesis testing 
ks.test(residuals(model1),"pnorm")
shapiro.test(residuals(model1))
###############################


###############################
#### Display results by ggplot library

# minimum and maximum father height
f_min <- min(fsdata$father)
f_max <- max(fsdata$father)

# equally space points between from the min-max height interval
xdat <- (f_max - f_min) * seq(0, 1, 0.01) + f_min
ydat <- b0_hat + b1_hat*xdat

# regression line data frame
regressionLine <- data.frame(xdat, ydat)
names(regressionLine) <- c("son", "father")

# plot of data set with regression line
ggplot(fsdata, aes(x=father, y=son)) + 
    geom_point(size=1, alpha=0.7) + 
    xlab("Height of father") + 
    ylab("Height of son") + 
    ggtitle("Father-son Height Data")  +
    geom_line(data=regressionLine, aes(x=son, y=father), lwd=1.5, color="red")
#    geom_smooth(method = "lm", se = F) +
#   geom_abline(intercept = b0_hat, slope = b1_hat,color = "green") 


### Funny question: What if 2 fathers have a liliput sons?
#fsdata = read.csv('data/fsdata.csv', sep = ",")
#head(fsdata)
#dim(fsdata)
fsdata[dim(fsdata)[1]+1,] = c(dim(fsdata)[1]+1,70,10)
fsdata[dim(fsdata)[1]+1,] = c(dim(fsdata)[1]+1,80,20)
scatterplotMatrix(fsdata[,2:3])


############### Exercise - Mini Homework 03 ###########

# Investigate a relationship between speed and stopping distance for cars
summary(cars)
?cars
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


