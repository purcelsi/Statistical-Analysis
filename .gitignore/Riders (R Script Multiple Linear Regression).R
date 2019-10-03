#set working directory
setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Advanced Business Data Analysis/CA1/CA1 Final Workfolder/Final R Scripts and Outputs")


library(readr)
#importing Riders.csv file from EXCEL into R.
Riders <- read.csv("Riders.csv", header = T,  sep=",")
View(Riders)
str(Riders)

#####################################
#CLEAN THE DATASET
#######################################

#Removing the "date" variable and repeat variables from dataset
Riders = (Riders[,-c(1,2,6,7,11,13)])


#########################################
#  DATA  EXPLORATION AND DESCRIPTION   ##
#########################################

# get quick information on everything
summary(Riders)
glimpse(Riders)
str(Riders)

#proportions of day contributions
prop.table(table(Riders$day))
#proportions of high temp contributions
prop.table(table(Riders$highT))
#proportions of low temp contributions
prop.table(table(Riders$lowT))
#proportions of precipitation contributions
prop.table(table(Riders$precip))
#proportions of cloud contributions
prop.table(table(Riders$clouds))
#proportions of weekday contributions
prop.table(table(Riders$weekday))

par(mfrow=c(1,1))
#It seems reasonable that the high temperature for the day (highT) might
#be  a quantitative explanatory variables and could be related to ridership so we will explore their relationship first.
#We will also compare the realtionship between riders and other 

#plotting continuous variables against the dependent variable
#hightemp
plot(Riders$riders~Riders$highT, main = "riders vs high temp", xlab = "high temp", ylab = "riders")
abline(lm(Riders$riders~Riders$highT), col="2")
#lowtemp
plot(Riders$riders~Riders$lowT, main = "riders vs low temp", ylab = "riders", xlab = "low temp")
abline(lm(Riders$riders~Riders$lowT), col="2")
#precipitation
plot(Riders$riders~Riders$precip, main = "riders vs precipitation", ylab = "riders", xlab = "precipitation")
abline(lm(Riders$riders~Riders$precip), col="2")
#clouds
plot(Riders$riders~Riders$clouds, main = "riders vs cloud cover", ylab = "riders", xlab = "clouds")
abline(lm(Riders$riders~Riders$clouds), col="2")


#comparing categorical variables and riders
#day
boxplot(Riders$riders ~ Riders$day, main = "Riders vs. day", ylab="riders", xlab = "day", las = 1, col = c(2,3,4,5))
#weekday
boxplot(Riders$riders ~ Riders$weekday, main = "Riders vs weekday", ylab="rider", xlab = "weekday", las = 1, names=c("weekend/holiday", "non-holiday weekday"), col = c(2,3))
?boxplot()
pairs(Riders)


#assessing normality of dependent variable
hist(Riders$riders, main = "count", xlab = "number of total rentals", ylab= "frequency")
qqnorm(Riders$riders)
qqline(Riders$riders, col = "green")
#perform Shapiro_wilks test for normality
shapiro.test(Riders$riders)
# as our p value is > than 0.05, we cannot reject Null for normality.
#Therefore the distribution of depedant variable is normal.


######################
#Creating Models
#######################

#First model (#Overlay a simple linear regression model on the realtionship between riders and highT)
#Multiple regression with a categorical variable
#create first model

model1 <- lm(formula = Riders$riders ~highT, data = Riders)
coef(model1)

summary(model1)

#With a p-value of less than 0.05 we can confirm that there is significant differnece between riders and highT
#Furthermore, the regression model has a multiple R2 value of 0.3298.
#We can also say that the regression model based on average daily temperature explained about 33%
#of the variation in daily ridership

# REPORT THE RESULTS ABOVE FOR MODEL1 (APA Style)
#model1: Riders =  B0 + B1highT
#model1: Riders - 24.7468 + 0.33*highT (P<alpha), R2 =0.33
#P=value of model1 is p<alpha - F(1,88) =43.31. P<0001


#Now suppose that we want to improve our model for ridership by considering not only
#the average temperature as predictors, but also the amount of precipitation 
#(rain or snow, measured inches) as well as the weekday, day and clouds
#We can do this in R by simply adding these variables as predictors for the number of riders to our regression model.

#Second model
model2 <- lm(formula = Riders$riders ~highT +precip + weekday + day +clouds, data = Riders)
coef(model2)

summary(model2)


#REPORT THE RESULTS ABOVE FOR MODEL2 (APA Style)

#model2: riders = -175.93 +5.55 *highT -92.59precip -147.2692weekdayY  -2.07dayMonday  -116.83 daySaturday -125.89 daySunday 36.2914 dayThursday  -4.8890dayTuesday -13.9960dayWednesday -7.6175clouds  
#(P<alpha), R2 =0.48
#P=value of model2 is p<alpha - F(10,79) = 9.39, P>0.001, R= 0.48

#Model shows that non-holiday weekdays (Y), precipitation and cloud cover have a significant effect
#We will therefore maintain these in our third model and exlude day variable

#Third model

model3 <- lm(formula = Riders$riders ~highT +precip + weekday +clouds, data = Riders)
summary(model3)

#REPORT THE RESULTS ABOVE FOR MODEL3 (APA Style)
#model3 : Riders = 66.96 + 5.89*highT -104.82precip -35.65weekdayY -9.03clouds
#P<alpha), R2 =0.47 - F(4, 85) = 21.03,

#Let's see if exluding weekday makes a better predictive model?
mod_excludeweekday <- lm(formula = Riders$riders ~highT +precip  +clouds, data = Riders)
summary(mod_excludeweekday) # With a lower R-squared value this does not appear to be the case


#Forth model (Could we maybe see if we can improve this by exploring a potential interaction beween rainfall of days of the week?)
model4 <- lm(formula =Riders$riders ~ highT + clouds+precip*weekday, data = Riders)
summary(model4)

#Plot the residuals of the final model
plot(residuals(model4))
#Here we see that the data in our selected model appear homoscedastic as they are randomly scattered around the x-axis
#thus,  difference between the observed value and the predicted value, are equal across all values of our predictor variable.

#Check if it meets the assumption of normality
qqnorm(residuals(model4))
qqline(residuals(model4))
# despite some divergence from noramlity at the upper and lower tail ends
# the distribution of the data in our model appears relatively normal

#REPORT THE RESULTS ABOVE FOR MODEL4 (APA Style)
#model4 : Riders = 66.96 + 5.89*highT -7.97 -377.79precip -55.97weekdayY +296.25precip:weekdayY 
#P<alpha), R2 =0.50 - F(5, 84) = 18.93

##########################
#HYPOTHESIS TESTING TO COMPARE REGRESSION MODELS  (Which model is a better fit and most explains the dependant variable?)
############################

#We can use anova() function to further quantify the extent to which the quadratic fit
#is superior to the linear fit

###MODEL2 VS MODEL1
anova(model2, model1)
Null hypothesis : Mode12 = Model1
alternate hypothesis: Model2 > Model1

###MODEL 3 VS MODEL1
#With  p value of less than alpha (p=0.0002), there is evidence that theres's a significant differnece between model2 and model
#model2 is a better model than 1 and adding an extra variable helps with predicting the number of riders
Null hypothesis : Mode13 = Model1
alternate hypothesis: Model3 > Model1
anova(model3, model1)
#Similarly, With  p value of less than alpha (p=0.0001), there is evidence that theres's a significant differnece between model2 and model1
#model3 is more significant in being a better model than  model 1 


###MODEL 3 VS MODEL2
anova(model3, model2)
Null hypothesis : Mode12 = Model3
alternate hypothesis: Model3 > Model2
#The p value is not less than 0.05 so there isn't evidence that theres's a differnece between model2 and model3
#Therefore we fail to reject the null hypothesis
#Therfore we conclude that model3 isn't a  signicantly better predictive model than model2

#MODEL 4 vs MODEL2
anova(model4, model2)
Null hypothesis : Mode14 = Model2
alternate hypothesis: Model4 > Model2
#The p value greater than 0.05 so there is no evidence that theres's a differnece between model4 and model2
#Therefore we cannot reject the null hypothesis
#Therfore we cannot conclude that model4 is significantly better predictive model than model 2 


###However when we test to see if model4 is better than model3
###MODEL 4 VS MODEL3
anova(model4, model3)
Null hypothesis : Mode14 = Model3
alternate hypothesis: Model4 > Model3
#With  p value of less than alpha (p=0.0001), there is evidence that 
#model4 is a significantly better predictive model than  model 3

#The p value greater than 0.05 so  we can rejust the H0 and state that there is  evidence that theres's a differnece between model4 and model3
#Therefore we can state that interactive effects of rainfall and it being a weekday significantly introdruces more predictive power to the model
#set working directory
setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Advanced Business Data Analysis/CA1/CA1 Final Workfolder/Final R Scripts and Outputs")


library(readr)
#importing Riders.csv file from EXCEL into R.
Riders <- read.csv("Riders.csv", header = T,  sep=",")
View(Riders)
str(Riders)

#####################################
#CLEAN THE DATASET
#######################################

#Removing the "date" variable and repeat variables from dataset
Riders = (Riders[,-c(1,2,6,7,11,13)])


#########################################
#  DATA  EXPLORATION AND DESCRIPTION   ##
#########################################

# get quick information on everything
summary(Riders)
glimpse(Riders)
str(Riders)

#proportions of day contributions
prop.table(table(Riders$day))
#proportions of high temp contributions
prop.table(table(Riders$highT))
#proportions of low temp contributions
prop.table(table(Riders$lowT))
#proportions of precipitation contributions
prop.table(table(Riders$precip))
#proportions of cloud contributions
prop.table(table(Riders$clouds))
#proportions of weekday contributions
prop.table(table(Riders$weekday))

par(mfrow=c(1,1))
#It seems reasonable that the high temperature for the day (highT) might
#be  a quantitative explanatory variables and could be related to ridership so we will explore their relationship first.
#We will also compare the realtionship between riders and other 

#plotting continuous variables against the dependent variable
#hightemp
plot(Riders$riders~Riders$highT, main = "riders vs high temp", xlab = "high temp", ylab = "riders")
abline(lm(Riders$riders~Riders$highT), col="2")
#lowtemp
plot(Riders$riders~Riders$lowT, main = "riders vs low temp", ylab = "riders", xlab = "low temp")
abline(lm(Riders$riders~Riders$lowT), col="2")
#precipitation
plot(Riders$riders~Riders$precip, main = "riders vs precipitation", ylab = "riders", xlab = "precipitation")
abline(lm(Riders$riders~Riders$precip), col="2")
#clouds
plot(Riders$riders~Riders$clouds, main = "riders vs cloud cover", ylab = "riders", xlab = "clouds")
abline(lm(Riders$riders~Riders$clouds), col="2")


#comparing categorical variables and riders
#day
boxplot(Riders$riders ~ Riders$day, main = "Riders vs. day", ylab="riders", xlab = "day", las = 1, col = c(2,3,4,5))
#weekday
boxplot(Riders$riders ~ Riders$weekday, main = "Riders vs weekday", ylab="rider", xlab = "weekday", las = 1, names=c("weekend/holiday", "non-holiday weekday"), col = c(2,3))
?boxplot()
pairs(Riders)


#assessing normality of dependent variable
hist(Riders$riders, main = "count", xlab = "number of total rentals", ylab= "frequency")
qqnorm(Riders$riders)
qqline(Riders$riders, col = "green")
#perform Shapiro_wilks test for normality
shapiro.test(Riders$riders)
# as our p value is > than 0.05, we cannot reject Null for normality.
#Therefore the distribution of depedant variable is normal.


######################
#Creating Models
#######################

#First model (#Overlay a simple linear regression model on the realtionship between riders and highT)
#Multiple regression with a categorical variable
#create first model

model1 <- lm(formula = Riders$riders ~highT, data = Riders)
coef(model1)

summary(model1)

#With a p-value of less than 0.05 we can confirm that there is significant differnece between riders and highT
#Furthermore, the regression model has a multiple R2 value of 0.3298.
#We can also say that the regression model based on average daily temperature explained about 33%
#of the variation in daily ridership

# REPORT THE RESULTS ABOVE FOR MODEL1 (APA Style)
#model1: Riders =  B0 + B1highT
#model1: Riders - 24.7468 + 0.33*highT (P<alpha), R2 =0.33
#P=value of model1 is p<alpha - F(1,88) =43.31. P<0001


#Now suppose that we want to improve our model for ridership by considering not only
#the average temperature as predictors, but also the amount of precipitation 
#(rain or snow, measured inches) as well as the weekday, day and clouds
#We can do this in R by simply adding these variables as predictors for the number of riders to our regression model.

#Second model
model2 <- lm(formula = Riders$riders ~highT +precip + weekday + day +clouds, data = Riders)
coef(model2)

summary(model2)


#REPORT THE RESULTS ABOVE FOR MODEL2 (APA Style)

#model2: riders = -175.93 +5.55 *highT -92.59precip -147.2692weekdayY  -2.07dayMonday  -116.83 daySaturday -125.89 daySunday 36.2914 dayThursday  -4.8890dayTuesday -13.9960dayWednesday -7.6175clouds  
#(P<alpha), R2 =0.48
#P=value of model2 is p<alpha - F(10,79) = 9.39, P>0.001, R= 0.48

#Model shows that non-holiday weekdays (Y), precipitation and cloud cover have a significant effect
#We will therefore maintain these in our third model and exlude day variable

#Third model

model3 <- lm(formula = Riders$riders ~highT +precip + weekday +clouds, data = Riders)
summary(model3)

#REPORT THE RESULTS ABOVE FOR MODEL3 (APA Style)
#model3 : Riders = 66.96 + 5.89*highT -104.82precip -35.65weekdayY -9.03clouds
#P<alpha), R2 =0.47 - F(4, 85) = 21.03,

#Let's see if exluding weekday makes a better predictive model?
mod_excludeweekday <- lm(formula = Riders$riders ~highT +precip  +clouds, data = Riders)
summary(mod_excludeweekday) # With a lower R-squared value this does not appear to be the case


#Forth model (Could we maybe see if we can improve this by exploring a potential interaction beween rainfall of days of the week?)
model4 <- lm(formula =Riders$riders ~ highT + clouds+precip*weekday, data = Riders)
summary(model4)

#Plot the residuals of the final model
plot(residuals(model4))
#Here we see that the data in our selected model appear homoscedastic as they are randomly scattered around the x-axis
#thus,  difference between the observed value and the predicted value, are equal across all values of our predictor variable.

#Check if it meets the assumption of normality
qqnorm(residuals(model4))
qqline(residuals(model4))
# despite some divergence from noramlity at the upper and lower tail ends
# the distribution of the data in our model appears relatively normal

#REPORT THE RESULTS ABOVE FOR MODEL4 (APA Style)
#model4 : Riders = 66.96 + 5.89*highT -7.97 -377.79precip -55.97weekdayY +296.25precip:weekdayY 
#P<alpha), R2 =0.50 - F(5, 84) = 18.93

##########################
#HYPOTHESIS TESTING TO COMPARE REGRESSION MODELS  (Which model is a better fit and most explains the dependant variable?)
############################

#We can use anova() function to further quantify the extent to which the quadratic fit
#is superior to the linear fit

###MODEL2 VS MODEL1
anova(model2, model1)
Null hypothesis : Mode12 = Model1
alternate hypothesis: Model2 > Model1

###MODEL 3 VS MODEL1
#With  p value of less than alpha (p=0.0002), there is evidence that theres's a significant differnece between model2 and model
#model2 is a better model than 1 and adding an extra variable helps with predicting the number of riders
Null hypothesis : Mode13 = Model1
alternate hypothesis: Model3 > Model1
anova(model3, model1)
#Similarly, With  p value of less than alpha (p=0.0001), there is evidence that theres's a significant differnece between model2 and model1
#model3 is more significant in being a better model than  model 1 


###MODEL 3 VS MODEL2
anova(model3, model2)
Null hypothesis : Mode12 = Model3
alternate hypothesis: Model3 > Model2
#The p value is not less than 0.05 so there isn't evidence that theres's a differnece between model2 and model3
#Therefore we fail to reject the null hypothesis
#Therfore we conclude that model3 isn't a  signicantly better predictive model than model2

#MODEL 4 vs MODEL2
anova(model4, model2)
Null hypothesis : Mode14 = Model2
alternate hypothesis: Model4 > Model2
#The p value greater than 0.05 so there is no evidence that theres's a differnece between model4 and model2
#Therefore we cannot reject the null hypothesis
#Therfore we cannot conclude that model4 is significantly better predictive model than model 2 


###However when we test to see if model4 is better than model3
###MODEL 4 VS MODEL3
anova(model4, model3)
Null hypothesis : Mode14 = Model3
alternate hypothesis: Model4 > Model3
#With  p value of less than alpha (p=0.0001), there is evidence that 
#model4 is a significantly better predictive model than  model 3

#The p value greater than 0.05 so  we can rejust the H0 and state that there is  evidence that theres's a differnece between model4 and model3
#Therefore we can state that interactive effects of rainfall and it being a weekday significantly introdruces more predictive power to the model
