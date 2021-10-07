################################## 
##### 01RAD Exercise 02 ##########
#################################
#
# Continue with rapid intro into R
#  - conditions, loops, functions
#  - data_frame
#  - factor variables
#  - introduction to graphics and plots (boxplot, scatterplot, qqplot, ...)
#  - regression line


######################
# get requirements for this exercise
# command for install required package is: install.packages("name_of_package")
list_of_packages <- c("tidyverse", "car", "lattice", "pwr", "MASS","GGally")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
print(.libPaths())
print(sessionInfo())
print(version)


##### Sometimes, you can meet #####

# remove all (environments, values, data, plots, ...)
# rm(list = ls())
# do not use it in "Project-oriented workflow"
# https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

# set woring directory (unnecessary, if you use relative paths)
# setwd("M:/01RAD/")  

######### Before we start #######
# Questions to the solution of Ex1


#### Loops ######

# Flip the fair coin simulation (T = tail, H = head)
x <- sample(c(0,1), 100, rep=T)
class(x)
sum(x)
sum(!x)                # x is 0,1 -> can be converted to FALSE, TRUE
table(x)               # number of observations of unique values
summary(x)             # if x is numeric
summary(as.factor(x))  # if x is factor


# Just play with the variable x
x_char <- (ifelse(x==0, "T", "H"))  # 
is.factor(x_char)
class(x_char)
summary(x_char)
x_factor <- factor(x_char)
summary(x_factor)


as.logical(x_factor)  # T as TRUE, F as FALSE, other NA
xxx <- as.numeric(as.logical(x_factor))
is.na(xxx)
na.omit(xxx)

x_factor2 <- (ifelse(x==0, "T", "F"))
as.numeric(as.logical(x_factor2))



# Flip the coin function: P(H) = p
toss <- function(n = 5,
                 p = 0.5){
        y = sum(rbinom(n, 1, p))
        return(y)
       }

toss()
toss(20)
toss(20, 0.75)
toss(p=0.75, n= 10)

replicate(10, toss(n=100))
rbinom(10, 100, 0.5)

sapply(4:16, toss)
sapply(4:16, sqrt)

# check the difference in apply functions:
# lapply and sapply: avoiding loops on lists and data frames
# tapply: avoiding loops when applying a function to subsets
# mapplay, rapplay
#as.numeric(Map("toss", n=10:20))


###################
### Loop playground
n = 10
fac_n = factorial(n)

# factorial with for loop 
fac1_n = 1
fac2_n = 1
j = 1
for (i in 1:n) fac1_n = fac1_n*i

# factorial with while loop
while (j<=n) {
  fac2_n = fac2_n*j
  j=j+1
}

print(c(fac1_n,fac2_n,fac_n))


my_factorial <- function(n){
    my_fac = 1
    for (i in 1:n) my_fac = my_fac*i
    my_fac
    }
my_factorial(10)  
  


########################  
### Task 1a ############

# vygenerujte nahodne 100 ostrouhlych trojuhelniku (3 strany a,b,c),
# kde kazda strana ma maximalne 10cm

### navrh reseni ###
n <- 100
a <- b <- c <- numeric(n)
for (i in 1:n) {
     a[i] <- runif(1,0,10)
     b[i] <- runif(1,0,10)
     c[i] <- runif(1,0,10)
     s <- sort(c(a[i],b[i],c[i])) # seradim strany podle velikosti
     while ((s[3]>=s[2]+s[1])&(s[3]^2>(s[2]^2+s[1]^2))){
       c[i] <- runif(1,0,10)
       s <- sort(c(a[i],b[i],c[i]))
     }
}
cbind(a,b,c) # zobrazime vysledek
#################################


### UKOL 1b ############
# vygenerujte nahodne 1000 trojuhelniku, strana max 10cm na cele cm.
# a priradte jim faktorovou promennou - druh podle uhlu (ostro, tupo, pravo)

### navrh reseni###
n <- 1000
a <- b <- c <- numeric(n)
for (i in 1:n) {
  a[i] <- ceiling(runif(1,0,10))
  b[i] <- ceiling(runif(1,0,10))
  c[i] <- sample(1:10,1)
  # udelame bez serazeni podle velikosti
   while ((c[i]>=a[i]+b[i])|(c[i]<=abs(a[i]-b[i]))) c[i] <- sample(1:10,1)
}
s0 <- cbind(a,b,c)

troj = rep("pravouhly",times = n)            # vsechny trojuhelniky oznacim jako pravouhle
s1   = t(apply(s0, 1, sort))
troj[s1[,3]^2<s1[,2]^2+s1[,1]^2] <- "ostrouhly"
troj[s1[,3]^2>s1[,2]^2+s1[,1]^2] <- "tupouhly" 
summary(as.factor(troj))  # vysledek




######################################
## create own dataset - dataframe ####

pet     = rep(c("cat","dog","fish","other","none"),times=8)
house   = rep(c("bungalow","villa", "row_house", "apartment"),each=10) 
is.factor(house)
members = rbinom(40,6,0.3)+1
income  = rexp(40,1/19)+11
area    = abs(rnorm(40,100,90))+20

# data frame
df    <- data.frame(pet,house,members,income,area)
is.factor(df$house)
dim(df)
summary(df)
View(df)
df$pet
class(df$pet)
str(df)

# tibble from dplyr
df_tidy <- tibble(pet,house,members,income,area)
df_tidy
as_tibble(df)
summary(df_tidy)


# matrix is not "data" object
data           <- matrix(c(pet,house,members,income,area), nc=5)
data$pet #try
data[ ,1]
colnames(data)
colnames(data) <- c("pet","house","members","income","area")
is.array(data)
is.data.frame(data)
data    <- as.data.frame(data)
summary(data)
class(data$income)



# Summarizing the dataset

# We use birthwt: dataset from the MASS package
?birthwt          # Description + info
head(birthwt)     # first lines

#Copy birthwt to another data.frame that we will change
birth = birthwt
summary(birth)  # summary (we can recognize factors with coding and numerical values)
str(birth)

sum(is.na(birth))
colSums(is.na(birth))
colMeans(birth)
apply(birth, 2, function(x) sum(is.na(x)))  # 2 - columnwise , 1 -rowwise
apply(birth, 2, function(x) mean(x))

dim(birth)[1]
# overall check

# Check again the description of each variable and recode them

#we can change one by one
summary(birth$race)
as.factor(birth$race)
summary(as.factor(birth$race))
birth$race  <- factor(birth$race, labels=c("White","Black","Other"))
summary(birth$race)
              
summary(birth)         
birth <- within(birth,{
    low   <- factor(low, labels=c("No","Yes"))
    race  <- factor(race, labels=c("White","Black","Other"))
    smoke <- factor(smoke, labels=c("No","Yes"))
    ui    <- factor(ui, labels=c("No","Yes"))
    ht    <- factor(ht, labels=c("No","Yes"))})
summary(birth) 

birth2 <- birth %>% 
  mutate(low  = factor(low,labels=c("No","Yes")),
         race = factor(race, labels=c("White","Black","Other")),
         smoke= factor(smoke, labels=c("No","Yes")),
         ui   = factor(ui, labels=c("No","Yes")),
         ht   = factor(ht, labels=c("No","Yes"))
         )
summary(birth2) 


num_varaibles <- sapply(birth, is.numeric)
sapply(birth, is.factor)
sapply(birth, class)

birth[,sapply(birth, is.numeric)]
birth[,num_varaibles]
birth[,c(2,3,6,9,10)]
birth[,c("age","lwt","ptl","ftv","bwt")]
#birth[,c(2,3,6,9,10)] == birth[,sapply(birth, is.numeric)]


#### PLOT = PLOTIME OBRAZKY ########
#par(mfrow=c(1,1))


### BOXPLOT - krabicove diagramy
boxplot(apply(birth[,num_varaibles], 2, scale)) #(z-scores)

boxplot(birth$age~birth$race)
plot(birth$age,birth$race)
plot(birth$race,birth$age)
with(birth,plot(race,age))

### boxplot by ggplots #####
ggplot(birth, aes(x=race, y=bwt, fill=smoke)) +
    geom_boxplot(size = 0.7, notch = T) +
    xlab("Race") +
    ylab("Birth weight") +
    theme_bw() +  
    geom_jitter(aes(race,bwt),
                position=position_jitter(width=0.3,height=0),
                alpha=0.6,
                size=1,
                show.legend=F) +
    stat_summary(fun=mean, geom="point", shape=23, size=3) #+coord_flip()
#### scatterplot
scatterplot(birth$bwt,birth$age)
ggplot(birth, aes(x=bwt, y=age,colour = smoke))+
  geom_point()+
  geom_smooth(method = lm)

#### pairs plots
pairs(birth[,num_varaibles])
splom(birth[,num_varaibles]) # podobny

#  ggpairs from GGally
ggpairs(birth)
ggpairs(birth[,num_varaibles]) 
ggpairs(birth[,!num_varaibles]) 

print_ggpairs = ggpairs(data=birth,
             columns=c(2,3,10,4), 
             upper = list(continuous = "density"),
             lower = list(combo = "facetdensity"),
             title="Risk Factors Associated with Low Infant Birth Weight")
print(print_ggpairs)



# ukazka dalsich moznosti prehledu dat, zde s balikem hmisc
install.packages("Hmisc")
library(Hmisc)
describe(birthwt)
summary(low ~ ., data=birth, method="reverse", overall=TRUE)
summary(bwt ~ ., data=birth)
summary(low ~ smoke + ht + ui, data=birth, fun=table)

summaryRc(bwt ~ age , bpplot='top', datadensity=FALSE, trim=.01,data=birth)
summaryRc(bwt ~ age , bpplot='top', datadensity=T, trim=.01,data=birth)
with(birth, plot(age,bwt))


# ukazka plotu pres skupiny
s <- summaryS(bwt ~ age  + race + smoke,  data=birth)
plot(s)  
plot(s, groups='race', datadensity=TRUE, scat1d.opts=list(lwd=.5, nhistSpike=0))


# histogramy
hist(birth$bwt)
hist(birth$bwt, breaks=seq(0, 5000, by=1000))
vaha1 = table(cut(birthwt$bwt, breaks=seq(0, 5000, by=1000), include.lowest=T, dig.lab=4))
vaha2 = as.factor(cut(birthwt$bwt, breaks=seq(0, 5000, by=1000), 
              include.lowest=T, dig.lab=4, labels = c("0-1000","1000-2000","2000-3000","3000-4000","4000-5000")))


Barvy <- c("yellow", "orange", "brown", "darkblue", "red")
# Vice oken pro obrazky
par(mfrow = c(2, 3), bty = "n")
hist(birth$bwt, breaks=seq(0, 5000, by=1000))
with(birth,hist(bwt, freq=F))
plot(vaha1, ylab = "Cetnost",col = Barvy)
plot(vaha2, ylab = "Cetnost",col = Barvy)
barplot(vaha1, ylab = "Cetnost", col=Barvy)
pie(vaha1,col=Barvy)


# pridani dalsich obrazku do jednoho okna
par(mfrow = c(1, 1))
hist(birth$bwt, freq=F, main="", xlab="Birth weight",xlim=c(0, 6000), ylim=c(0, 0.0006))
lines(density(birth$bwt), col="red", lwd=2)
x=seq(0,6000,length=1000)
y=dnorm(x,mean=mean(birth$bwt),sd=sd(birth$bwt))
lines(x,y,col="blue", lwd=2, lty = 1)

# qqplot
qqnorm(birth$bwt, main="")
qqline(birth$bwt)




y1      = birth$bwt[birth$race == "Black"]
y2      = birth$bwt[birth$race == "White"]
y       = c(y1,y2)
y.means = c(mean(y1),mean(y),mean(y2))
y.sd    = c(sd(y1),sd(y),sd(y2))
min(y)
max(y)
max.len = max(length(y1), length(y2))
y1 = c(y1, rep(NA,  length(y) - length(y1)))
y2 = c(y2, rep(NA,  length(y) - length(y2)))

opar    = par(mfrow=c(2,1),mar=c(5,7,4,2),las=1)
stripchart(data.frame(Black=y1,Together=y,White=y2),xlab=expression("Birth weight"),pch=19,xlim = c(1000,5000))
arrows(c(y.means,y.means[2]),c(1.5,1.5,2.5,2.5),c(y.means,y.means[2]),c(1.1,1.9,2.9,2.1),length=.1)
text(y.means,c(1.2,1.8,2.8),round(y.means,2),pos=4,cex=.8)
rd = rnorm(100,mean=y.means[2],sd=y.sd[2])        # if y is data frame: sd = as.numeric(apply(y,2,sd))
hist(y,xlab="Observations",ylab="Relative frequency", main="Histogram for Birth weight", breaks = 15, freq = FALSE, ylim=c(0,0.001), xlim = c(1000,5000),dig.lab=4 )
partition = seq(min(rd ), max(rd ), 0.01)
lines(partition, dnorm(partition, y.means[2], y.sd[2]), col = "red")
lines(partition, dnorm(partition, y.means[1], y.sd[1]), col = "blue")
lines(partition, dnorm(partition, y.means[3], y.sd[3]), col = "green")
par(opar)



bwt.by.race <- with(birth, tapply(bwt, race, median))
barplot(bwt.by.race, ylab="Median birth weight")


bwt.by.race.mean <- with(birthwt, tapply(bwt, race, mean))
bwt.by.race.sd <- with(birthwt, tapply(bwt, race, sd))
bp <- barplot(bwt.by.race.mean, ylab="Mean birth weight",
                   ylim=c(0,4000), col="coral", border=NA)
arrows(bp, bwt.by.race.mean-bwt.by.race.sd,
       bp, bwt.by.race.mean+bwt.by.race.sd,
       code=3, length=.1, angle=90)
# see also segments()


library(lattice)
with(birth,plot(age, bwt, col=race))
with(birth,xyplot(bwt ~ age, groups=race))
with(birth,xyplot(bwt ~ age | race,  layout=c(3,1)))



dev.off()
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
#opar <- par(mfrow=c(2,1))
#par(opar)

mu    = seq(5,6,by=0.05)
sigma = seq(1,2,by=0.05)
data_test = data.frame(mu,sigma)
x     = 2

plot(dnorm(2,mu,sigma))



######################
# UKOL Ex2
######################
# nactem data ustrice
# zdroj dat: http://ww2.amstat.org/publications/jse/jse_data_archive.htm

# upravte si pripadne txt s daty aby sel nacist
ustrice <- read.table("data/30oysters.dat.txt", sep = "", header = TRUE, as.is = 1)
ustrice <- read.table("data/30oysters2.txt", sep = ";", header = TRUE, as.is = 1)
head(ustrice)
summary(ustrice)
#upravte soubor a ulozte ho do csv a nastete pomoci prikazu 
write.table(ustrice,"data/30oysters.csv", sep = ";",  row.names = F, col.names = T)
ustrice <- read.csv("data/30oysters.csv",header =T, sep= ";")
# use function from readr to build tibble
ustrice2 <- read_csv2("data/30oysters2.csv")
head(ustrice2)

plot(ustrice$Oyster_Weight_g,ustrice$Oyster_Volume_cc,
     xlab="Vaha ustrice",
     ylab="Objem ustrice")



# UKOLY
# vykreslete histogramy, krabicove diagramy, scatterploty, kolacove diagramy,  pairs atd pro data ustrice
# rozdelete ustrice do 3 skupin podle vahy 0-9,9-12,12-20 a vykreslete boxploty a barploty objemu pro tyto skupiny
# rozdelte ustrice do 4 skupin podle hmotnosti - kvantilove a opakujte predchozi vykresleni obrazku
# prolozte histogramy hmotnosti i objemu odhadem hustoty prislusneho normalniho rozdeleni
# (nechci jadrovy odhad s gaussovskym jadrem, ale husotu normalniho rozdleeni, kde si parametry mean a sd odhadnete zvlast)


