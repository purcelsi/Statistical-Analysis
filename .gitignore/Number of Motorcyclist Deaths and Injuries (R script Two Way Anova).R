#set working directory
setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Advanced Business Data Analysis/CA1/CA1 Final Workfolder/Final R Scripts and Outputs")

#read in data
library(readxl)
MotorcylistsIncidents<- read_excel("Number of Motorcylist Deaths and Injuries.xlsx", 
                            sheet = "Preprocessed data for R")

str(MotorcylistsIncidents)
View(MotorcylistsIncidents)
names(MotorcylistsIncidents)
attach(MotorcylistsIncidents)


#Convert variables into factors
Age <- as.factor(Age)
Gender  <- as.factor(Gender)

# Verify the number of factors
levels(Gender) # returns: Male, Female
levels(Age)   # returns: 18 - 20, 21 - 24, 25 - 34
# Correct: this is a 3x2 factorial design


# visualise 2x3 factorial design
boxplot(Deaths ~ Gender, data=MotorcylistsIncidents, names=c("Female", "Male"), col = c(2,4))
boxplot(Deaths ~ Age, data=MotorcylistsIncidents, col= "grey")


# some normality tests for males
Deaths_Male = Deaths[Gender=='Male']
qqnorm(Deaths_Male,  main = "Normal Q-Q Plot For Male Motorcylist Fatalities"); qqline(Deaths_Male, col="blue")
shapiro.test(Deaths_Male)
ks.test(scale(Deaths_Male), 'pnorm')

# some normality test for females
Deaths_Female = Deaths[Gender=='Female']
qqnorm(Deaths_Female,  main = "Normal Q-Q Plot for Female Motorcylist Fatalities"); qqline(Deaths_Female, col = "red")
shapiro.test(Deaths_Female)
ks.test(scale(Deaths_Female), 'pnorm')
Deaths_Female

# Two-way ANOVA (without interactions)
m1 = aov(Deaths ~ Gender + Age)
summary(m1)
qqnorm(residuals(m1)); qqline(residuals(m1))

# Two-way ANOVA (with interactions)
m2 = aov(Deaths ~ Gender + Age + Gender:Age)
summary(m2)
TukeyHSD(m2)

# Post-hoc for significant differences between n>2 groups
Motorcyclistincidents_AgeGroup = TukeyHSD(m2)
Motorcyclistincidents_AgeGroup
plot(Motorcyclistincidents_AgeGroup)
#According to p-value, YES THERE APPEARS TO BE AN INTEARACTION


# Plot the interactions
interaction.plot(Gender, factor(Age), Deaths)
interaction.plot(factor(Age),Gender, Deaths)

#REMOVE ATTACHED DATA
rm(list=ls())

