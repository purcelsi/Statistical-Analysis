#set working directory
setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Advanced Business Data Analysis/CA1/CA1 Final Workfolder/Final R Scripts and Outputs")


library(readxl)
RoadCasualties<- read_excel("Number of Killed and Injured Road Casualties.xlsx", 
                            sheet = "Preprocessed data for R")
View(RoadCasualties)

# Want to see if there is a difference in the poverty rates between 2016 and 2017

#Checking for normality
#Using histograms
#The distrubtion of people travelling by each mode of transport is not even across dublin
names(RoadCasualties)

par(mfrow=c(1,3))
hist(RoadCasualties$'12:00 AM- 7:00 AM', prob= T, ylim = c(0, 0.01), main = "12:00 AM- 7:00 AM", xlab = "No. of Killed and Injured Road Casualties", ylab="Freq", las =1, col ="blue")
hist(RoadCasualties$'8:00 AM-16:00 PM', prob= T, ylim = c(0, 0.01), main = "8:00 AM-16:00 PM", xlab = "No. of Killed and Injured Road Casualties", ylab="Freq", las =1, col="green")
hist(RoadCasualties$'17:00 PM-11:00 PM',prob= T, ylim = c(0, 0.01), main = "17:00 PM-11:00 PM", xlab = "No. of Killed and Injured Road Casualties", ylab="Freq", las =1, col="salmon")

#medians of groups
median(RoadCasualties$'12:00 AM- 7:00 AM')
median(RoadCasualties$'8:00 AM-16:00 PM')
median(RoadCasualties$'17:00 PM-11:00 PM')

#Means of groups
mean(RoadCasualties$'12:00 AM- 7:00 AM')
mean(RoadCasualties$'8:00 AM-16:00 PM')
mean(RoadCasualties$'17:00 PM-11:00 PM')


par(mfrow=c(1,1))
# Visualise Using boxplot (visually there appears to be a differnce in that
#(visually there appears to be a differnce in that 19-24s make up most of reported injuries
boxplot(RoadCasualties$'12:00 AM- 7:00 AM', 
        RoadCasualties$'8:00 AM-16:00 PM',
        RoadCasualties$'17:00 PM-11:00 PM',  xlab = "time of day (hrs)", ylab= "No. of Casualties", names = c( "12:00 AM- 7:00 AM", "8:00 AM-16:00 PM", "17:00 PM-11:00 PM"), 
        col =c("blue","green","salmon"))


#here we can the distribution of points for each crime is not normal for each offense
par(mfrow=c(1,3))
qqnorm(RoadCasualties$'12:00 AM- 7:00 AM', main = "Normal Q-Q Plot of 12:00 AM- 7:00 AM", col ="blue")
qqline(RoadCasualties$'12:00 AM- 7:00 AM', col = '2')

qqnorm(RoadCasualties$'8:00 AM-16:00 PM', main = "Normal Q-Q Plot of 8:00 AM-16:00 PM", col="green")
qqline(RoadCasualties$'8:00 AM-16:00 PM', col = '2')

qqnorm(RoadCasualties$'17:00 PM-11:00 PM', main = "Normal Q-Q Plot of 17:00 PM-11:00 PM", col="salmon")
qqline(RoadCasualties$'17:00 PM-11:00 PM', col = '2')



# Test for Normality
shapiro.test(RoadCasualties$'12:00 AM- 7:00 AM') #Normal
shapiro.test(RoadCasualties$'8:00 AM-16:00 PM') #Normal
shapiro.test(RoadCasualties$'17:00 PM-11:00 PM') #Not normal



#Remove the year title
RoadCasualties <- RoadCasualties[,-c(1)]
str(RoadCasualties)
# run K-W test
#
RoadCasualties.kw = kruskal.test(RoadCasualties)  
RoadCasualties.kw 
# With t a p<0.05, we can reject the null hypothesis that the median values are the 
#At least one of the populations are different

#################################
#5 Perform Post hoc testing in order to see WHICH one of the groups is different
# need to install a dunn.test package
install.packages("dunn.test")
library(dunn.test)
dunn.test(RoadCasualties, kw=FALSE, method ='bh')

#Alternatively, you can run a  multiple wilcoxin tests (pair-wise test) between each pairs
wilcox.test( RoadCasualties$`12:00 AM- 7:00 AM`, RoadCasualties$`8:00 AM-16:00 PM`) #Significant difference between night and morning hours
wilcox.test( RoadCasualties$`12:00 AM- 7:00 AM`, RoadCasualties$`17:00 PM-11:00 PM`) #Significant differnce bewtween night and evenining hours
wilcox.test( RoadCasualties$`8:00 AM-16:00 PM`, RoadCasualties$`17:00 PM-11:00 PM`) # No Significant difference between morning and evening hours




