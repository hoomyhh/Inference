library(readxl)
setwd("C:/Users/homa0012/Desktop/PhD_Hoomaan/Teaching/ComputerLab2/")
smoking <- read_excel("smoking.xls")
set.seed (20)
attach ( smoking )
SmokYes = split( smoking , Smoke )$Yes ## community of smokers ( Population )
data1 = SmokYes [ sample (dim(SmokYes)[1] , 50, replace = FALSE ),]
#set.seed (45)
#data2 = SmokYes [ sample (dim( SmokYes )[1] , 50, replace = FALSE ),]

sum1 <-summary(data1)
data1[,10]=as.numeric(unlist(data1[,10]))
summary(data1)
data1[,11]=as.numeric(unlist(data1[,11]))
summary(data1)

samplesize <- 50
attach ( data1 )
female_smokers = split(data1,Sex)$"Female"
male_smokers = split(data1,Sex)$"Male"

female_smokers2 = as.data.frame(female_smokers[,1])
male_smokers2 = as.data.frame(male_smokers[,1]) 

female_proportion <- length(female_smokers2[,1])/samplesize
male_proportion <- length(male_smokers2[,1])/samplesize
gender_proportion = 

barplot(c(female_proportion,male_proportion),ylab ="Proportion", xlab="Gender",names.arg = c("Female","Male"),axisnames = TRUE,axis.lty = 3,col = c("BLUE","RED"))

hist(as.numeric(unlist(data1[,2])),freq=FALSE,main = "Probability Density Function of Age",xlab = "Age",probability = TRUE)        
lines(density(as.numeric(unlist(data1[,2]))))

counter <-0
for (i in seq(samplesize)){
  if (data1[i,3]=="Married"){counter<-counter+1}
  
}
Married <- counter

counter <-0
for (i in seq(samplesize)){
  if (data1[i,3]=="Separated"){counter<-counter+1}
  
}
Separated <- counter

counter <-0
for (i in seq(samplesize)){
  if (data1[i,3]=="Divorced"){counter<-counter+1}
  
}
Divorced <- counter

counter <-0
for (i in seq(samplesize)){
  if (data1[i,3]=="Widowed"){counter<-counter+1}
  
}
Widowed <- counter

counter <-0
for (i in seq(samplesize)){
  if (data1[i,3]=="Single"){counter<-counter+1}
  
}
Single <- counter

piepercent = 100*c(Married,Separated,Divorced,Widowed,Single)/sum(c(Married,Separated,Divorced,Widowed,Single),1)
pie(c(Married,Separated,Divorced,Widowed,Single),labels=piepercent, col = rainbow(5))
legend("topright",c("Married","Separated","Divorced","Widowed","Single"), fill=  rainbow(5)) 

boxplot(female_smokers$Age,male_smokers$Age,main = "Female and Male Smokers", at = c(1,2),names = c("Female","Male"),col = c("orange","red"))


Average_Smoking_Age= mean(data1$Age)
var_data = 1/49*sum((data1$Age-Average_Smoking_Age)**2)
Loweralpha=-1.65


LowerBound = Loweralpha * sqrt(var_data/50)+Average_Smoking_Age


Average_Smoking_Female= mean(female_smokers$Age)

#install.packages("car")
#x= rnorm(100)
#car::qqPlot(x)


#Question 2
Week_average = mean(data1$`Amount Weekends`+data1$`Amount Weekdays`)
var_data_week = 1/49*sum((data1$`Amount Weekends`+data1$`Amount Weekdays`-Week_average)**2)
Upperalpha = -1.65
UpperBound  = Week_average-sqrt(var_data_week/50)*Upperalpha


# Question 3 Using Binary data analysis
Lower_interval=-1.96*sqrt(female_proportion*(1-female_proportion)/50)+female_proportion
Upper_interval=1.96*sqrt(female_proportion*(1-female_proportion)/50)+female_proportion


#Question 4
# H_0 :{p<0.5},H_a :{p>0.5}if the proportion is less than 0,5 then 
#there are more men smoking than women. 
# p'~N(p,p(1-p)/n) and the worst case is p=0,5 since the variance is maximized
# Now condition on H_0, the p-value should be calculated as P(p'<0,36)=0,0244
#since alpha= 0,05>p-value thus H_0 is rejected and with this sample data and
#significance level there are more women as smokers than men.
