################################## 
##### 01RAD Exercise 06 ##########
##################################
#
# Today's exercise
#   Residual Analysis and Diagnostic Tools
#   Model Selection
#   Post-Hoc Analysis


######################
# get requirements for this exercise
list_of_packages <- c("tidyverse", "ggpubr", "car", "lattice", "pwr", "MASS",
                      "GGally","colorspace","scatterplot3d","gridExtra",
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
############################################################
############################################################

# delete graphics
#dev.off()
#par(mfrow = c(1, 1))





##############################
### Multiple variable linear regression
#############################

# Investigation of the "cars" data, from 2004 Cars and Trucks
# Source: The Journal of Statistics Education - datasets archive

cars04 <- read.table("data/carsdata2.csv", sep =";",header = T )
summary(cars04)
cars04[c(406,394,167),]

dim(cars04)
cars04 <- na.omit(cars04)
summary(cars04)

head(cars04)
# Instead of 6 variables of cars type create one factor variable with names
#       labels=(c("sedan","sport","suv","minivane","wagon","pickup"))
#       merge multivan and wagon and pickup groups to have only 4 types

# Create new variable consumption
#    - Compute it as an average of city and highway consumption
#    - convert it from MPG to l\100km

cars04$type <- "sedan"
cars04$type[cars04$Sports==1]  <- "sport"
cars04$type[cars04$SUV==1]     <- "suv"
cars04$type[cars04$Wagon==1]   <- "minivan"
cars04$type[cars04$Minivan==1] <- "minivan"
cars04$type[cars04$Pickup==1]  <- "minivan"
cars04$type <- factor(cars04$type)

cars04$consumption <- rowMeans(cbind(cars04$CityMPG,cars04$HwyMPG))
cars04$consumption <- 100/(1.60934*(cars04$consumption/3.7854))
summary(cars04)
cars04 <- na.omit(cars04)

# Explore how consumption depends on drive (front+rear vs. 4x4),
#       use box plots
#       make a t-test to check if the difference is significant

# cars04$FoRWD <- as.numeric(!(cars04$AWD)) we can use AWD only

par(mfrow = c(1, 2))
plot(cars04$consumption~as.factor(cars04$AWD),xlab="Presence of AWD", ylab ="Consumption l/100km")
plot(cars04$consumption~(cars04$AWD),xlab="Presence of AWD", ylab ="Consumption l/100km")


plot1 <- ggplot(cars04 %>% mutate(AWD = as.factor(AWD)),
                         aes(x=type, y=consumption, fill=AWD)) +
  geom_boxplot(size = 1, notch = T) +
  geom_jitter(aes(type,consumption),
              position=position_jitter(width=0.3,height=0),
              alpha=0.4,
              size=1,
              show.legend=F)+
  labs(title="Consumption vs Type", y="Consumption", x="Type") 
plot1

plot2 <- plot1 +
  theme_bw() +  
   labs(fill="AWD\nAll-wheel drive vehicle",
        title="Consumption vs Type",
        subtitle="Distinguish between AWD and non-AWD",
        caption="version: 01RAD2020") 
plot2

# t.test
t.test(cars04$consumption[cars04$AWD],cars04$consumption[!(cars04$AWD)] )


# Plot the relationship between consumption and other variables (where its meaningful)
names(cars04)
#scatterplotMatrix(cars04[c("EngineSize","DealerCost","Cyl","HP","Weight","WheelBase","consumption")])
ggpairs(cars04[c("EngineSize","DealerCost","Cyl","HP","Weight","WheelBase","consumption")])

xyplot(consumption ~ Len    | type , data=cars04 , pch=20 , cex=1 , col="blue" )
xyplot(consumption ~ Width  | type , data=cars04 , pch=20 , cex=1 , col="blue" )
xyplot(consumption ~ Weight | type , data=cars04 , pch=20 , cex=1 , col="blue" )


table(cars04$Cyl)
cars04  <-cars04[cars04$Cyl>3 & cars04$Cyl<10 ,]
cars04$Cyl2 <- cut(cars04$Cyl, breaks=c(0,4.5,6.5,15),labels = c("4","6","8")) 
summary(cars04)
with(cars04,plot(type,consumption))


PCH  <- c(15, 16, 17, 18)
COL  <- heat_hcl(4)      
BGC  <- diverge_hcl(4)

par(mfrow = c(1, 1))
plot(consumption ~ Weight, data = cars04, 
     pch = PCH[type], col = COL[type], bg = BGC[type],
     xlab = "Weight", ylab = "Consumption", main="Dependence of Trees Volume on Trees Height")
legend(2000,21, legend = levels(cars04[, "type"]), pch = PCH, col = COL, pt.bg = BGC, y.intersp = 1.2)



# Find suitable model describing the consumption. As a regressors use:
#        Dealer Cost, Engine Size, Number of Cylinders, Horsepower,	Weight,
#        Wheel Base, Length, Width and drive (front+rear vs. 4x4)

cars_lm1     <- lm(consumption ~ type+EngineSize+Weight+HP+WheelBase+Width:Len, data = cars04)  
summary(cars_lm1)

par(mfrow = c(2, 2))
plot(cars_lm1)

cars04_sedan <- cars04 %>% filter(Sedan ==1) %>% filter(consumption > 5.7)

cars_lm1_sedan     <- lm(consumption ~ EngineSize+I(sqrt(Weight))+HP+WheelBase, data = cars04_sedan)
summary(cars_lm1_sedan)

par(mfrow = c(2, 2))
plot(cars_lm1_sedan)



cars_lm1     <- lm(consumption ~ DealerCost*EngineSize*HP + Weight + type + WheelBase +  Cyl + Width:Len + as.factor(AWD), data = cars04)  
summary(cars_lm1)

cars_lm2_BIC  <- stepAIC(cars_lm1, k=log(dim(cars04)[1]))
summary(cars_lm2_BIC)

cars_lm2_AIC  <- stepAIC(cars_lm1)
summary(cars_lm2_AIC)

anova(cars_lm2_BIC,cars_lm2_AIC)

dim(cars04)
cars04[c(384),]
# Check: the assumptions of OLS (by hypothesis testing and graphical diagnostic tools):
#     constant variance - homoscedasticity, missing autocorrelation
#     plot: residuals against each explanatory variable, against order of measurement,
#                 against fitted values 
#     normality of error terms, normal probability plot of the residuals.
#     identify outliers, remove them and run the analysis again 

par(mfrow = c(2, 2))
plot(cars_lm2_AIC)

shapiro.test(resid(cars_lm2_AIC))

# other plots as previous lectures
# How to pick up the best model (we learn it the whole life)





## Advert data set
Advert <- read.table("data/Advert.csv", sep =",", header = T )
head(Advert)
summary(Advert)

model1 <-  lm(sales ~ TV*radio*newspaper, data = Advert) # model with all interactions
summary(model1)
    
model2 <-  lm(sales ~ TV*radio, data = Advert) 
summary(model2)

anova(model1,model2)

pairs(Advert)
n = nrow(Advert)

model_step0 <- step(model1)   # what is step function doing?
summary(model_step0)

anova(model1,model2)
anova(model1,model_step0)
anova(model2,model_step0)


# BIC
model_step1 <- stepAIC(model1, k=log(n))
summary(model_step1)

# AIC
model_step2 <- stepAIC(model1, k=2)
summary(model_step2)
model_step2 <- stepAIC(model1, direction="both")
model_step2$anova

# compare obtained model from BIC and AIC step functions
anova(model_step1,model_step2)

# Drop 1 predictor from full model
# which one?
dropterm(model1, test = "F")  # 'arg' should be one of “none”, “Chisq”, “F”

# Add 1 predictor to null model (model with intercept only)
# which one?
with(Advert, add1(lm(sales~TV),sales~TV+radio+newspaper, test = "F"))

AIC = matrix(0,8,2)
BIC = matrix(0,8,2)

# can do it better
# write loop - add one variable per step and save AIC
AIC[1,]= extractAIC(lm(sales~1, data = Advert))
AIC[2,]= extractAIC(lm(sales~TV, data = Advert))
AIC[3,]= extractAIC(lm(sales~TV+radio, data = Advert))
AIC[4,]= extractAIC(lm(sales~TV*radio, data = Advert))
AIC[5,]= extractAIC(lm(sales~TV*radio+newspaper, data = Advert))
AIC[6,]= extractAIC(lm(sales~TV*radio+TV*newspaper, data = Advert))
AIC[7,]= extractAIC(lm(sales~(.)^2, data = Advert[,2:5]))
AIC[8,]= extractAIC(lm(sales~(.)^3, data = Advert[,2:5]))


BIC[1,] = extractAIC(lm(sales~1, data = Advert), k =log(n))
BIC[2,] = extractAIC(lm(sales~TV, data = Advert), k =log(n))
BIC[3,] = extractAIC(lm(sales~TV+radio, data = Advert), k =log(n))
BIC[4,] = extractAIC(lm(sales~TV*radio, data = Advert), k =log(n))
BIC[5,] = extractAIC(lm(sales~TV*radio+newspaper, data = Advert), k =log(n))
BIC[6,] = extractAIC(lm(sales~TV*radio+TV*newspaper, data = Advert), k =log(n))
BIC[7,] = extractAIC(lm(sales~(.)^2, data = Advert[,2:5]), k =log(n))
BIC[8,] = extractAIC(lm(sales~(.)^3, data = Advert[,2:5]), k =log(n))

# make it nicer

plot(AIC[,1],AIC[,2],type = "l",col = "red")
lines(BIC[,1],BIC[,2],col = "blue")

head(Advert)
leaps(x=Advert[,2:4], y=Advert[,5],
      names=names(Advert)[2:4], method="Cp")

leaps<-regsubsets(sales~TV+radio+newspaper,data=Advert,nbest=10)
summary(leaps)
plot(leaps,scale="Cp")


# plot statistic by subset size
subsets(leaps, statistic="cp") 

#Other options for plot( ) are bic, Cp, and adjr2.
#Other options for plotting with subset( ) are bic, cp, adjr2, and rss


head(Advert)
summary(Advert)

# Advertising data displays sales (in thousands of units) for a particular product
# as a function of advertising budgets (in thousands of dollars) for TV, radio, and newspaper media.
# Solve following problems and questions 

# Display Advertising data - make several plots

# Try to find "best" model to explain how can advertising influence sales

# Use AIC, BIC, C_2, R^2, Adj-R^2, F-test to choose your model.
# Start with full model with all interactions, or with null model with intercept only
# Display AIC, BIC, C_2 statistics in dependence on number of parameters

# Validate your final model - normality, homoscedasticity, outliers

# With final model answer following questions:
#  - Is there a relationship between advertising budget and sales?
#  – Which media contribute to sales, i.e. do all three media—TV, radio, and newspaper—contribute to sales?
#  – Which media generate the biggest boost in sales?
#  - How strong is the relationship between advertising budget and sales?
#  – How much increase in sales is associated with a given increase in TV advertising?
#  – How much increase in sales is associated with a given increase in Radio advertising?
#  - How accurately can we estimate the effect of each medium on sales?
#  - How accurately can we predict future sales?
#  - Is there synergy among the advertising media?
#  - Imagine you have 100k $, what is the best strategy how to spend it in advertising?
#  - How much more pruduct will we sell, if we spend 10k$ in TV and 20k$ in radio advertising?
#  - What is the 95% confidence interval of previous question?



