rm(list=ls(all=TRUE))
setwd("/Users/Ravi/Desktop/acm_data_analysis/dataanalysis/R/data/")
getwd()

# Reading data from a particular sheet of an 
#xls file and reading the 
# data from specified columns and rows 
#and handling of dates

library(XLConnect)
yieldData<- readWorksheetFromFile("DataRTraining.xlsx",
                                  sheet = "data", 
                                  startRow = 3, endRow = 323,
                                  startCol = 1, endCol = 6)
head(yieldData)
attach(yieldData)
colnames(yieldData)=c("Week", "Yield.1", "Yield.2", 
                      "Input", "Price1", "Price2")
Input = yieldData$Input
summary(Input)
str(Input)

# creating columns on quantity for 2 comodities

qnty.1 <- Yield.1*Input
qnty.2 <- Yield.2*Input

# Adding the 2 quantity variables to the data
yieldData2 <- data.frame(yieldData,
                         qnty.1,qnty.2)

head(yieldData2, 10L)
yieldData2[20:50,2:4]
yieldData2[20:50,-c(2:4)]
names(yieldData2)
summary(yieldData2)
str(yieldData2)

attach(yieldData)
library(lubridate)
year <- as.factor(year(Week))
quarter <- as.factor(paste(quarter(Week),"-",year))
month <- as.factor(paste(month(Week),"-",year))
yieldData100 <- data.frame(yieldData2,month,quarter,year)
head(yieldData100)

#Reading data from a text file
#Reading the data from a text file and replacing the blanks 
#with NA's adding columns

rm(list=ls(all=TRUE))

txt.data <- read.table("univBankT.txt", header=T, 
                       dec=".",
                       col.names=c("ID", "age",	"exp",	"inc",	
                                   "zip",	"family",	"ccavg",	
                                   "edu",	"mortgage"),
                       na.strings="NA",sep="\t")

#A few initial checks with data sets

edit(txt.data)
attach(txt.data)
names(txt.data)
dim(txt.data)
summary(txt.data)
str(txt.data)

head(txt.data, 20)
tail(txt.data)

is.na(txt.data)
sum(is.na(txt.data))

#Reading from a CSV file

rm(list=ls(all=TRUE))
univ=read.table('univBank.csv', 
                header=T,sep=',',
                col.names=c("ID", "age",  "exp",	"inc",	
                            "zip",	"family",	
                            "edu",	"mortgage"))
head(univ)
sum(is.na(univ))

loanCalls <- read.table("loanCalls.csv", header=T, sep=",",
                        col.names=c("ID", "infoReq", "loan"),
                        dec=".", na.strings="NA")

cc <- read.table("cc.csv", header=T, sep=",",
                    col.names=c("ID", "Month", "Monthly"),
                    dec=".", na.strings="NA")

otherAccts <- read.table("otherAccts.csv", header=T, sep=",",
                         col.names=c("ID", "Var", "Val"),
                         dec=".", na.strings="NA")

#Demonstrate tapply, reshape, merge, innerjoin, outerjoin, 
#commands

#We have the monthly credit card spending over 12 months.
#We need to compute monthly spendings

summary(cc)
cc$ID <- as.factor(cc$ID)
cc$Month <- as.factor(cc$Month)
summary(cc)

meanNA <- function(x){
  a <-mean(x, na.rm=TRUE)
  return(a)
}

ccAvg <- data.frame(seq(1,5000), 
                    tapply(cc$Monthly, cc$ID, meanNA))
names(ccAvg)
colnames(ccAvg) <- c("ID", "ccAvg")
ccAvg$ID <- as.factor(ccAvg$ID)
otherAccts$ID <- as.factor(otherAccts$ID)
summary(ccAvg)

# Other accounts need to be transposed
summary(otherAccts)
otherAccts$Val <- as.factor(otherAccts$Val)
summary(otherAccts)

library(reshape) 
otherAcctsT=data.frame(cast(otherAccts,
                            ID~Var,value="Val"))
summary(otherAcctsT)

# Merging
univComp <- merge(univ,ccAvg,
                  by.x="ID",by.y="ID",
                  all=TRUE)  #inner join

univComp <- merge(univComp, otherAcctsT, 
                  by.x="ID", by.y="ID",
                  all=TRUE)

univComp <- merge(univComp, loanCalls, 
                  by.x="ID", by.y="ID",
                  all=TRUE)

# merge(data1,data2,by.x="id1",by.y="id2",all.x=TRUE) #left (outer) join
# merge(data1,data2,by.x="id1",by.y="id2",all.y=TRUE) #right (outer) join
# merge(data1,data2,by.x="id1",by.y="id2",all=TRUE)   #(full) outer join

summary(univComp)
attach(univComp)
univComp$ID <- as.factor(ID)
univComp$family <- as.factor(family)
univComp$edu <- as.factor(edu)
univComp$loan <- as.factor(loan)
summary(univComp)

#Apply family of function: 1 for row and 2 for columns
#http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/

library(VIM)
aggr(univComp)
matrixplot(univComp, sortby="age")

#First thing, can we omit all 
#rows with missing values?  
#It is fairly straight forward.  But, may not be 
#feasible most times.

sum(is.na(univComp))/75000
nrow(univComp[!complete.cases(univComp),])

#We can omit missing values.  using the following command.
#But, in general it is a bad practice if there are too many
#of them.

univ2 = na.omit(univComp)

#Let us identify rows where more than 50% (2) attributes are missing
#We can do it using apply function or for loop.  But, DMwR offers a 
#nice function

library(DMwR)
length(manyNAs(univComp, 0.3))

#univComp = univComp[-manyNAs(univComp, 0.5), ] Let us not apply this for the study

#Replacing with a central statistic.  Let us replace with median and mode.
#Let us create an error metric for agr (root mean square error)

rmse=0
univ2=centralImputation(univComp)

#If there is justice in the universe, univ2 should not have
#NULL

sum(is.na(univ2))
sum(is.na(univComp))

#Based on similarities.  We noticed that age and experience are 
#highly correlated.  Let us use that factor.

#Build linear observations.  lm stands 
#for linear regression

summary(univ2)
cor(univ2[,c(2,3,4,8,9)], )
lm(age~exp, data=univComp)

#A function to compute the missing value

fillAge <- function(exp) {
  if (is.na(exp)) return(NA)
  else return(25.3587 + 0.9937 * exp)
}

#Apply function to execute it on all values of a row
attach(univ2)
age1=sapply(univComp[is.na(age),"exp"], fillAge)
univ2[is.na(age), "age"] <- age1

#KNNimputation

univ2=knnImputation(univComp, 
                    k = 10, meth = "median")
univ2=knnImputation(univComp, k = 10)

sum(is.na(univ2))
sum(is.na(univComp))
#univComp <- subset(univComp, select=-c(16:19))

#standardize and KNN
#Discretize and DT

univNumeric <- subset(univ2, select=c(1,2,5,6))
cor(univNumeric)

library(vegan)


univ3=decostand(univNumeric,"standardize")
summary(univ3)
univKNN <- cbind(univ2[,-c(1,2,5,6)], univ3)

#Binning for DT
library(infotheo)
age=discretize(univ2$age, disc="equalfreq", 
               nbins=10) #Discretizing the variable 'age'
age=as.factor(age$X)

#The two below will be binned into equal width
#as equal frequency would lead to highly unequal bins
#in their cases.  Refer to their histograms

inc=discretize(univ2$inc, disc="equalfreq", 
               nbins=10) #Discretizing the variable 'inc'
inc=as.factor(inc$X)

ccavg=discretize(univ2$ccAvg, 
                 disc="equalwidth", 
                 nbins=10) #Discretizing the variable 'age'
ccavg=as.factor(ccavg$X)

mortgage=discretize(univ2$mortgage, 
                    disc="equalwidth", nbins=5) #Discretizing the variable 'age'
mortgage=as.factor(mortgage$X)

# *** Removing the numerical variables from the original data and
# ***  adding the catoegorical forms of them
summary(univ2)
univDT=subset(univ2,select=-c(1,2,3,4,8,9)) 
univDT=cbind(age,inc,ccavg, mortgage, univDT)
univDT$zip <- as.factor(univDT$zip)
summary(univDT)

library(C50)

dtC50= C5.0(loan ~ ., 
            data = univDT, 
            rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

a=table(univDT$loan, predict(dtC50, newdata=univDT, 
                               type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
rcTrain

library(class)

pred=knn(univKNN, univKNN, univKNN$loan, k = 1)
a=table(univKNN$loan, pred)

rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
rcTrain


a=table(univ2$loan, predict(dtC50, newdata=univ2, 
                               type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
rcTrain