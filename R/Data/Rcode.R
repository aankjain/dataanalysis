#R as a calculator. Let us do a variety of problems

2.89314*3.21456
7%%3
7%/%3

7^2
exp(-9)

#log (x, base)
log(10,10)
log(10,exp(1))
log2(2)

abs(-10)
sqrt(49)

#Rounding off
ceiling(3.47)
floor(3.47)
round(3.4756789,2)

#trigonometric functions
sin(90)
sin(pi/2)

library(aspace)
sin_d(90)

#Character manipulation
a="I like programming in R"
toupper(a)
tolower(a)
substr(a,3,7)
substr(a,3,7) <- "hike"
a

strsplit(a,"")
strsplit(a,"h")

paste(a,"I too")
paste(a,"I too", sep=". ")
rm(a)

#Generating sequences and help
?seq
example(seq)

seq(-3,3, length.out=30)
x <- pretty(c(-3,3), 30)

gl(3,5,labels=c("red","blue",
                "green"))

0.99999932^10000

#probability distributions
runif(10,0,1)
rnorm(100,0,1)

y <- dnorm(x)
plot(x, y, type='l', xlab="Normal Deviate", ylab="Density", yaxs="i") 

pnorm(1,0,1)
#68, 95, 99 rule
pnorm(1,0,1)-pnorm(-1,0,1)
pnorm(2,0,1)-pnorm(-2,0,1)
pnorm(3,0,1)-pnorm(-3,0,1)
pnorm(4.5,0,1)-pnorm(-4.5,0,1)
0.9999932^10000
pnorm(5.5,0,1)-pnorm(-5.5,0,1)
pnorm(6,0,1)-pnorm(-6,0,1)
0.9999999932^3000000000

qnorm(0.98)
qnorm(0.9999999999)

#For all the above you have dbinom, dpois etc.

#Generating a few points randomly
runif(10,0,1)
rnorm(10,0,1)
sample(1:10, 5, replace=T)
sample(1:10, 5, replace=F)

#How do we generate same random numbers
sample(1:10,5)
sample(1:10,5)

set.seed(123)
sample(1:10,5)

set.seed(123)
sample(1:10,5)

#Vectors
#library(forecast)

x=seq(1,100, by=2)
y=seq(2,100, by=2)
z=c(x,y)
mode(z)
z=c(1,2,3,4,5)

#Basic operations on vectors
length(z)
sum(z)
max(x)
min(y) 
sort(z)
diff(z, lag=2)
scale(z, center=TRUE, scale=TRUE)

#Statistical operations
mean(x)
mean(x, trim=0.30,na.rm=FALSE)
median(y)
quantile(y,probs=c(0.3))
range(x)
sd(y)
var(x)
summary(y)

#Generating vectors

rep(5,10)
rep(1:3,6)
rep(1:3,c(6,6,6))
rep(c(1:4), c(1:4))

cut(y,5)
cut(y,5, labels=F)

rm(x,y,z)

#Vector operations
a=c(rep(2,5), seq(8,9, length=10))
b=rep(5,4)
a+4
a*2
sqrt(a)
a+b
a>b
names(b)=c("p", "q", "r", "s") 
names(b)=NULL

#Subsetting a vector

a
a[2]
a[2:5]
a[-1]
a[-c(2:6)]

#Subsetting using filtering

a[a*a > 6]
which(a*a>6)

x <- c(0, -3, 4, -1, 45, 90, -5)
x[x <= -2 | x > 5]

#Create a vector of length 25 containing 
#first 25 even numbers.  Now, using a single
#command randomize this vector


sample(2*seq(1:25))

diff(a, lag=1)

#Matrices

a=c(1,2,3)
b=c(5,7,9)
rbind(a,b)
cbind(a,b) 
c=rbind(a,b) 
d=rbind(c,c)

#Define from ground up

matrix(c(1,2,3,5,7,9), 
       byrow=T, nrow=2)

d=matrix(c(1,2,3,5,7,9,10,11,12), 
         nrow=3)

d[2,3]
d[,3]
d[1:2,3]
d[-c(1:2), 3]
which(d>3)
d[(d>2)]
which(d[,2]>2)

mean(d)
rowMeans(d)
colSums(d)

array(1:24, dim = c(4, 3, 2))
a[1, 3, 2]

#List
cust=list(name="Sara", 
          gender="F", age=35)
cust[1]
cust[2]
cust$gender
cust$marital= "y"
unlist(cust) 

#A list within a list
purchases=list(amount=300, 
               items=4)
cust$tran=purchases
cust[[5]]
cust[[5]][1]

#data frames

a=matrix(c(1,2,"a","b"), nrow=2)

b=c(1,2)
d=c("a", "b")
c=data.frame(b,d)

data(mtcars)
names(mtcars)
dim(mtcars)

#mtcars$mpg
attach(mtcars)
summary(mtcars)
str(mtcars)

mtcars$gear=as.factor(mtcars$gear)
mtcars$cyl=as.factor(mtcars$cyl)
mtcars$vs=as.factor(mtcars$vs)
mtcars$am=as.factor(mtcars$am)
mtcars$carb=as.factor(mtcars$carb)

summary(mtcars)
str(mtcars)

mtcars[2,3]
mean(mtcars$mpg)

subset(mtcars, mpg > 22)

mtcars1 <- edit(mtcars)
newdata<-edit(data.frame())

# Computing Pi 

#Let us define a vector that carries the 
#number of simulations we want to conduct.  As we want to see how the value converges, we shall 
#Conduct simulations where in each 
#simulation, we throw 10 or 100 or 
#1000 or 10000 or a million throws.  

sims=c(10,100,1000,100000,1000000) 

#Here, you learn a key concept in R programming namely a for loop.  It 
#creates a temporary variable called I and starts with the first value 
#in simulations (10).  Execute the entire code within the curly 
#brackets and then increments the value of "i" to next value in sims.  
#It stops when it executed the code for all values of 

for (i in sims) {

  #Defined two variables to keep the counts of those darts that fell 
  #within the circle and outside the circle (but within the square)
  
  within=outSide=0
  
  #The landing point of the dart can be explained by its x and y coordinates. 
  #We assume our circle to have a radius of 1 unit and center is origin.  
  #Then, the coordinates of the sides will be (0,0), (1,0), (0,1), (1,1).  
  #So, we simulate random landing of the dart by simulating random x and 
  #y coordinates with a restriction that coordinate falls within 0 and 1.
  
  x=runif(i,0,1)
  y=runif(i,0,1)
  
  #This is loop within loop.  Our goal here is to see if the dart fell 
  #within the circle or outside and update our within and outdate accordingly.
  
  for (j in 1:i) {
    
    #The following is another very important construct in R.  It is a 
    #conditional statement.  Essentially, if the conditional is true, the 
    #statement(s) following it will be executed.  Else, the statements 
    #following "else" will be executed.  We measure the distance between 
    #the point and the origin.  If the distance is less than the radius, 
    #then that dart fell within the circle.  Else, it fell outside the 
    #circle.
    
    if((x[j]^2+y[j]^2)<=1) {
      within=within+1
    }
    else outSide=outSide+1
  }
  
  #As agreed, pi is 4 times the ratios of darts as these are proportional 
  #to the areas.
  
  pi=4*within/(within+outSide)
  
  #cat is the command to print the output on the screen
  
  cat("For", i, "simulations, pi =", 
      pi, "\n")
}

#A better way to write the same program.  Avoid loops as much as 
#possible in R

#5 million simulations the previous way

within=0  
x=runif(5000000,0,1)
y=runif(5000000,0,1)

start_time=proc.time()
for (j in 1:5000000) {
  
  if((x[j]^2+y[j]^2)<=1) {
    within=within+1
  }
}

pi=4*within/5000000
exe_time=proc.time() - start_time
exe_time
pi

#5 million simulations without loops

start_time=proc.time()
pi=4*sum(x^2+y^2<=1)/5000000
exe_time=proc.time() - start_time
exe_time
pi

#We are defining a function called rolls which takes the possible options in A, 
#in B and number of simulations.  Functions are extremely powerful blocks of 
#code that can makes programming efficient.

rm(list=ls(all=TRUE))

rolls<- function(possibA,possibB,sims){
  i=0
  
  #A loop is needed to throw as many times as determined by simulations
  
  for(j in 1:sims){
    
    #Simulating a simultaneous throw.
    
    Aval=sample(1:possibA,1)
    Bval=sample(1:possibB,1)
    
    #If first dice gives higher value, the count is updated.
    
    if (Aval==Bval){i=i+1}
    
  }
  
  AisMore=i/sims
  
  #Function returns this value that return specifies.  Or function returns the last #value computed.
  return(AisMore)
}

rolls(12,6,10000)

x <- as.numeric (readline(prompt="How many options are there for the first die?")) 
y <-as.numeric(readline(prompt="How many options are there for the second die?"))

c=rolls(x,y, 100000)

cat("The first die shall give higher value with a probability of", c, "\n")

#######Monty Hall problem

#Let us start with two counter.

Switched=0
Stay=0

##We do 10000 simulations to get statistically reliable result

for (i in 1:10000) {
  
  ##Let us start the game by randomly generating a winning door
  
  WinningDoor=sample(1:3, 1)
  
  ##Let us pick a random door
  
  YourChoice=sample(1:3, 1)
  
  ##Now, the host opens the door
  ##If you choose a door where there is a goat, the host has only one option 
  ##and that is to open the third gate.  Say, you picked door 1, winning door 
  #is 2, then the host can only pick 3.  We can use simple mathematical trick 
  #from the fact that 1+2+3 = 6 that if you choose x, winning door is y, the 
  #host has to open 6=x-y
  
  if(WinningDoor!=YourChoice) 
  {OpenedDoor=6-WinningDoor-YourChoice}
  
  ##If you choose a door where there is a car, 
  ##the host can open any of the other two
  
  if(WinningDoor==YourChoice) {
    Doors=c(1,2,3)
    AvailableDoors=Doors[-WinningDoor]
    Random=sample(1:2,1)
    OpenedDoor=AvailableDoors[Random]
  }
  
  ##Now, is the crucial time.  If you want to switch, you will move to the 
  ##third door (other than opened door and your first choice)
  
  SwitchedChoice=6-OpenedDoor-YourChoice
  
  ##Let us see which one wins
  
  if(WinningDoor==SwitchedChoice) {Switched=Switched+1}
  if (WinningDoor==YourChoice)    {Stay=Stay+1}
  
}

r=c(Switched,Stay)

bp=barplot(r,names.arg=c("Switch","Stay"),
           col=c("darkblue","red"),
           main="The better choice")

text(bp, 0, round(r, 1),cex=1.5,pos=3, 
     col="white")

#Capital Market Modeling

##Let us define the returns matrix and initialize the investments at Rs. 1000
#As we do 1000 simulations again, we store them in arrays of same length

rm(list=ls(all=TRUE))
Returns=matrix(c(0.8, 0.06, 0.9, 0.9,
                 0.2, 1, 1.05, 1, 1, 
                 1.1, 3, 1, 1.2, 3, 
                 1, 1.4, 3, 1.1), 
               nrow=6, ncol=3, 
               byrow=TRUE)

Green=Red=Blue=1000

GreenOutcome=RedOutcome=BlueOutcome=
  rep(0, times=1000)

##We are preparing to conduct 1000 iterations

sims = 1

while (sims <= 1000) {
  
  ##We are dealing with a different looping construct to do 1000 simulations.  
  #Each iteration tests the returns of each stock after 20 years
  
  for (i in 1:20) {
    
    ##The exact outcome is modeled to be random
    
    Outcome = sample(1:6,1)
    
    Green=Returns[Outcome,1]*Green
    Red=Returns[Outcome,2]*Red
    Blue=Returns[Outcome,3]*Blue
    
  }
  
  ##Let us store the results of the jth iteration as jth value in each array
  
  GreenOutcome[sims]=Green
  RedOutcome[sims]=Red
  BlueOutcome[sims]=Blue
  
  ##Let us reinitialize and start a new iteration
  
  Green=Red=Blue=1000
  sims=sims+1
}

##We consider median and not mean because of the outliers
##Why?

AverageGreen=median(GreenOutcome)
AverageRed=median(RedOutcome)
AverageBlue=median(BlueOutcome)

barplot(c(AverageGreen,AverageRed, AverageBlue),names.arg=c("Green","Red", "Blue"),col=c("green","red", "blue"),main="The Performance")

#Add a 50-50 Green+Red

Returns=matrix(c(0.8, 0.06, 0.9, 0.9, 0.2, 1, 1.05, 1, 1, 1.1, 3, 1, 1.2, 3, 1, 1.4, 3, 1.1), nrow=6, ncol=3, byrow=TRUE)
Pink=Green=Red=Blue=1000

PinkOutcome=GreenOutcome=RedOutcome=BlueOutcome=rep(0, times=1000)

for (j in 1:1000) {
  
  for (i in 1:20) {
    Outcome = sample(1:6,1)
    Green=Returns[Outcome,1]*Green
    Red=Returns[Outcome,2]*Red
    Blue=Returns[Outcome,3]*Blue
    
    ##Every year the money is halved and invested equally in Red and Green
    
    Pink=(Returns[Outcome,1]*Pink/2)+
      (Returns[Outcome,2]*Pink/2)
  }
  
  GreenOutcome[j]=Green
  RedOutcome[j]=Red
  BlueOutcome[j]=Blue
  PinkOutcome[j]=Pink
  
  Pink=Green=Red=Blue=1000
  
}

AverageGreen=median(GreenOutcome)
AverageRed=median(RedOutcome)
AverageBlue=median(BlueOutcome)
AveragePink=median(PinkOutcome)


barplot(c(AverageGreen,AverageRed, AverageBlue, AveragePink),names.arg=c("Green","Red", "Blue", "Pink"),col=c("green","red", "blue", "Pink"),main="The Performance")

#Should we outsource or not?

rm(list=ls(all=TRUE))

maint = c(10,20)
lab= c(2,8)
rawMat = c(3,9)
prodLevel = c(15000,35000)
outSource = 26

#Let us assume that the values can take any value randomly 
#between upper and lower limits

maint= runif(1000,maint[1], maint[2])
lab= runif(1000,lab[1], lab[2])
rawMat= runif(1000,rawMat[1], rawMat[2])
prodLevel= runif(1000,prodLevel[1], prodLevel[2])

inHouseCost=(maint + lab + rawMat) * prodLevel
outSourceCost= prodLevel*outSource

totalSavings=inHouseCost-outSourceCost

Hist= hist(totalSavings)
mean(totalSavings)

#Project management and learning R programming;  

#What is the most 
#likely time to complete the project given 
#the following conditions

#Total modules: 10 to 12 if we get part of the job
#Total modules: 20 to 24 if we get the whole job
#There is a 90-10 chance that we get part or full job 
#Tasks per module: 50 to 100
#People per module: 3:7
#Time per task: 5 to 10 days

#Let us conduct 10, 100, 1000, 10000 simulations.  Then compute the average to estimate

#The function below is our own first R function.  
#It takes as input number of simulations and computes the timeNeeded

timeNeeded= function(numSims) {
  time=0
  
  for (i in 1:numSims) {
    
    partOrFull=runif(1,0,1)
    if (partOrFull<=0.9){
      totalModules=sample(10:12, 1)}
    else {totalModules=sample(20:24, 1)}
    
    #Two vectors are created randomly to represent tasks and people per module
    
    tasksPerModule=sample(50:100, totalModules, replace=T)
    peoplePerModule=sample(3:7, totalModules, replace=T)
    
    totalTasks=sum(tasksPerModule)
    
    j=1
    
    #Compute the time for all tasks.  Each task can take anywhere between 5 to 10 hours
    timeToDoTasks=0
    while (j<=totalTasks) {
      timeToDoTasks=timeToDoTasks+runif(1,5,10)
      j=j+1
    }
    
    #Time needed according to this simulation
    
    time[i]=timeToDoTasks/sum(peoplePerModule)
  }
  
  #The time vector is returned
  return(time)
}

simulations=c(10,100,1000)
par(mfrow=c(1,3))

for (i in simulations) {
  Time=timeNeeded(i)
  cat("Time Needed based on", i, "simulations= ", mean(Time), "\n")
  hist(Time, xlab=i, main="")  
}

par(mfrow=c(1,1))
