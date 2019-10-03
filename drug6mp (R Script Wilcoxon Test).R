# Dataset on Remission Duration from a Clinical Trial for Acute Leukemia


# Testing the effects of a clinical drug (6-MP) vs control in 42 children patients with leukemia 
# n=21 in each treatment group
# 21 patients recieve plecebo drug
# 21 received 6-MP drug
#Time to relapse in months was measured for each group after while reciving treatment

#Install and load necessary packages
install.packages("pwr")
library(pwr)
install.packages('effsize')
library('effsize')

#set working directory
setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Advanced Business Data Analysis/CA1/CA1 Final Workfolder/Final R Scripts and Outputs")


#read in data
library(readr)
drug6mp <- read_csv("drug6mp.csv")


#Explore dataset
View(drug6mp)
names(drug6mp)
str(drug6mp)


################################################
#Brief exploratory analysis of data distribution
################################################
par(mfrow=c(1,2))

#Using boxplot
boxplot(drug6mp$t1, col ="3", main = "Time to relapse for patients on placebo", xlab = "Placebo", ylab ="Time to relapse(months)")
    
#Using boxplot
boxplot(drug6mp$t2, col ="6", main = "Time to relapse for patients on 6-MP", xlab = "6-MP", ylab ="Time to relapse(months)")


par(mfrow=c(1,1))
#Visaulise group differences between the effects of all treatments on the median number of hours drug6mp
boxplot(drug6mp$t1,drug6mp$t2, main = "Time to relapse for patients on treatment", ylab="Time to relapse(months)", xlab = "Treatments", las = 1,
        col = c(3,6),  names=c("Placebo", "6-MP"))

#From the boxplot there looks like a big difference between control and drug6MP
#Both boxplots look reltively normal in terms of distribution


#The median time for relapse in the placebo group was 8 months
median(drug6mp$t1)
#The median time for relapse in the test drug group was 16 months
median(drug6mp$t2)



 par(mfrow=c(1,2))
#Using histograms
#% Male poverty rates is not very normally distributed
hist(drug6mp$t1, main ="Distribution of time to relapse in placebo patients", col ="3", ylab="Frequency", xlab = "Placebo patients")

#%Female poverty rates is not very normally distributed
hist(drug6mp$t2, main ="Distribution of time to relapse in 6-MP patients", col ="6",  ylab="Frequency", xlab = "6-MP patients")


########################################
#Test for normality
########################################

par(mfrow=c(1,2))
qqnorm(drug6mp$t1, main = "Normal Q-Q Plot of relapse time for patients on placebo")
qqline(drug6mp$t1, col = "blue")

qqnorm(drug6mp$t2, main = "Normal Q-Q Plot of relapse time for patients on 6-MP drug")
qqline(drug6mp$t2, col = "red")


#perform Shapiro_wilks test for normality

#HO: null-hypothesis of this test is that the population is normally distributed
#HA: alternate hypothesis of this test is that at least one of the populations is not normally distributed 

shapiro.test(drug6mp$t1) # normal distribution (p value > 0.05)
shapiro.test(drug6mp$t2) # not normal distribution (p value <0.05)

# as our p value <0.05 for one of the population treatment groups we can reject null for normality.
#we will therefore conduct a non parametric test knoen as wilxcoon rank sum test as an alternative
#to the parametric independent t-test nonparametric tests for comparing the time differences between our treatment groups 


#WILCOXIN TEST
#H0:Time taken to relapse for patients recieving placebo and patient receiving antileukemic test drug (6MP) are equal
#HA:Time taken to relapse for patients recieving placebo and patient receiving antileukemic test drug (6MP) are different

drug6mptest<-wilcox.test(drug6mp$t1, drug6mp$t2, paired =FALSE)
drug6mptest # W =106.5, P=0.004

#Calculate z score
qnorm(drug6mptest$p.value/2) #z=-2.859118, p =0.004


#Getting the p= 0.004. Therefore, with a small p-value would indidate that we can reject the null hypothesis
#Therefore, there is a differnece between the the time to relapse between these patients receiving treatments
#Time to relapse was remarkably different between both treatment groups (N=42, Z=-2.860, p=0.004)
#More specifically, those recieving the anti-leukemic drug take a significantly longer time to relapse before patients on placebo
#There is strong evidence to believe that 6-MP drug is a viable treatment solution for children suffering from leukemia


################################################################
#Determine Effect Size for Wilcoxon test with non-normal data ##
################################################################


# Effect size 
install.packages('effsize')
library('effsize')

# 1. check the current power (is it okay or under-powered?)
# 2. propose a more definite study with:
#    95% power, alpha=0.001 & ctrl group; assume a dropout rate of 30%


# 1. To find power, we must determine the effect size
#Effect sizes can also be interpreted in terms of the percent of nonoverlap of the treated group's scores with those of the placebo groups score,
#Revisiting the boxplots from earlier we can see that there is some degree of overlap between the Interquartile ranges (IQR) of both population distributions 
#As we can see the median line of 6-MP box lies outside of the Placebo box and can
#therefore interpret from this that there is likely to be a difference between the two groups
#however, the lenght of the whiskers for both boxplots would
#suggest considerable variablity in the data which a researcher should be a aware of as well.

#Alternatively we can caluclate the effect size ourselves which is simply the difference in two groups means divideded by
#the average of their standard deviations

mean(drug6mp$t1)     #The mean time for relapse in the placebo group was 8.7 months
mean(drug6mp$t2)    #The mean time for relapse in the test drug group was 17.1 months
sd(drug6mp$t1)      #The sd for the treated population group is 6.47
sd(drug6mp$t2)      #The sd for the untreated population group is 9.99
averagesd <- (sd(drug6mp$t1) +sd(drug6mp$t2))/2
meansdifference <-mean(drug6mp$t2)-mean(drug6mp$t1)

d= meansdifference/averagesd
d  #1.023 which would be considered to be a large effect

#or else we can use the effsize package 
library(effsize)
Placebo <- drug6mp$t1
Drug6mp <-drug6mp$t2
cohen.d(Placebo, Drug6mp)

# Therfore, using our calculated cohen's d, the  current power of this study at a significance level of 0.001 is currently
#We have 21 patients in each group (n=21)
pwr.t.test(d=1.001, n=21, sig.level =0.001)	#40% This study is currently quite powerful

#This would tell us that we have a 89%% chance of finding a large effect size difference (d=1.023) in our sample size of n=42
#However, to further improve the design and power of this clincal study we could increase sample size
#This initial study found a signifcant differnce between control and treatment group with a power of 89% and 42 patient participants)
# While the Power is reasonable it could also be undermining what this study found, in other the p-value. That is why we are requesting 
# a larger study to find a more significant difference.

pwr.t.test(d=1.001, power=0.95, sig.level=0.001)
plot(pwr.t.test(d=1.023, power=0.95, sig.level=0.001))
#In order to appreciate a large difference between the placebo and 6MP drug with a power of 95%
# and a p-value greater than a signifcance level of 0.001 we would need to recruit N=50 persons in each group


#How many patients should we recruit for future studies?
2*51 # at least 102 patients should be recruited
#Additionally, given the clinical nature of this study, if we assume a drop out rate of 30%:
2*51*1.3 #We should start off with at least 132.6 volunteers in order to increase our chances of still having 102 participants left at the end of the study

