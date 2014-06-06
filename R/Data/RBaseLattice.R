rm(list=ls(all=TRUE)) 

#This file illustrates basic plots and lattice plots in R.  
#Try each and every one of these
#to get a feel.  We use two data sets for illustration.

#Randomly generated data
Index <- 1:260
#More systematic way of building sequences
#seq(1,260, by=1)

set.seed(4711)
Returns <- rnorm(260)
Price = cumsum(Returns)

#How do I pick a number with a 
#probability?

#A built in R dataset
attach(mtcars)

#Stripchart lays out the data in a 2D graph

stripchart(wt)
dotchart(wt)
plot(wt)

plot(Returns)
stripchart(Returns)
stripchart(Returns, 
           method="jitter", 
           jitter=0.2)
dotchart(Returns)

#Plot and identify is a good way to visually identify 
#outliers

plot(wt)
clicked.points <- identify(wt)
mtcars[clicked.points,]



#Histogram shows how a variable is distributed
hist(Returns, probability = TRUE)

#Let us see where deviation from bell curve is most
curve(dnorm(x), min(Returns), 
      max(Returns), 
      add = TRUE)

#Let us see how this density curve looks
dens<-density(Returns) #computes and displays kernel density estimates
plot(dens, main="Kernel Density Estimates")

#Histogram with values on Y and not probabilities
hist(Returns)

#Box plot shows the median, quantiles and outliers
boxplot(Returns) #produces a box-and-whisker plot
boxplot(mtcars$wt)
abline(h=mean(wt,na.rm=T))
abline(h=mean(wt,na.rm=T)+sd(wt,na.rm=T))
abline(h=mean(wt,na.rm=T)-sd(wt,na.rm=T))

#qqplot also tells how far away from normal 
#distribution the points are

qqnorm(Returns) #creates a normal quantile-quantile plot
qqline(Returns) #adds a line through the first and third quartiles

#Scatter plot
plot(wt,mpg)
plot(wt~mpg)
plot(mpg~wt)
abline(lm(mpg~wt))
scatter.smooth(mpg~wt)
lines(lowess(wt,mpg)) # lowess line (x,y)

#Line plot
plot(Index,Price)
plot(Index, Price, type = "l", main = "Line Plot")
plot(Index, Price, type = "o", main = "Line Plot")
plot(Index, Returns, type = "h", main = "Histogram-like Vertical Lines")
plot(Index, Price, type = "s", main = "Line Plot")
plot(Index, Price, type = "n", main = "Line Plot")


qqplot(Returns,Price) #produces a QQ plot of two datasets

#Bar and Pie charts are best way to compare categorical variables
#Bar plot can be used to visualize the distribution 
#of a categorical variable in the data

table(gear)
barplot(table(gear))
barplot(table(gear), horiz=TRUE)
barplot(Returns)

pie(table(gear), init.angle = 20)

#Dummy data
gend=c("male", "female", "children")
fract=c(30.5,29.5, 40)
pop=data.frame(gend, fract)

attach(pop)
barplot(fract)
pie(fract)


#Pretty charts (Look at power point)

plot(x = Index, y = Price,
     xlab = "Day", ylab = "Price", 
     main = "Time series of Price",
     pch = 15, col = "skyblue", las = 3, 
     font.main = 2, xlim=c(0,300),
     ylim=c(-30,10))

?plot

Colors = colors()
Colors[grep("blue", Colors)]

barplot(table(cyl), las = 2, offset = 0)

barplot(table(cyl), las = 2, 
        col = heat.colors(3), offset = 0)

barplot(table(cyl), horiz = TRUE, 
        las = 1, col = rainbow(3),
        space = 1)

barplot(table(cyl), las = 2, 
        col = terrain.colors(3), offset = 0)

barplot(table(cyl), las = 2, 
        col = topo.colors(3), offset = 0)

barplot(table(cyl), las = 2, 
        col = cm.colors(3), offset = 0)

#Rcolorbrewer is another powerful package for color selection

#Seq Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds
#YlGn YlGnBu YlOrBr YlOrRd
#Diverge BrBG, PiYG PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#Qual Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

library(RColorBrewer)
cols=brewer.pal(3, "Dark2")

barplot(table(cyl), las = 2, 
        col = cols, offset = 0)

#Logarithimic plots
plot(wt, mpg, log = "xy")
plot(wt, mpg, log = "y")
plot(wt, mpg, log = "x")

#Drawing multiple scatter plots on a single panel
#Matrix

pairs(~mpg+disp+drat+wt, data=mtcars, 
      main="Simple Scatterplot Matrix")

#Multiple plots on the same screen
par(mfrow = c(1, 2))
plot(wt~mpg)
plot(Returns~Index)
par(mfrow = c(1, 1))

#Then why do we need Lattice

library(lattice)
par(mfrow = c(1, 1))

#All the basic plots are available

histogram(~mpg, data = mtcars)
densityplot(~Returns)
stripplot(Returns, jitter=0.2)
xyplot(wt~mpg)
bwplot(~wt)
barchart(~table(gear))

#A few advanced plots for 3 dimensional visualization are available

cloud(mpg~wt*disp, data=mtcars)
cloud(mpg~wt*disp, data=mtcars, 
      screen = list(z = 105, x = -70), 
      panel.aspect = 0.75)


#But what makes lattice extremely useful is conditioning and group 
#plotting

histogram(~ mpg, data = mtcars)
histogram(~ mpg | factor(cyl), 
          data = mtcars)

densityplot(~ mpg | factor(cyl), data = mtcars,
            auto.key = TRUE, plot.points = FALSE)

densityplot(~ mpg | factor(cyl), 
            groups=factor(gear), 
            data = mtcars,
            auto.key = TRUE, 
            plot.points = FALSE)

bwplot(~ mpg | factor(gear), mtcars)

bwplot(factor(cyl) ~ mpg | factor(gear), 
       mtcars)

bwplot(factor(cyl) ~ mpg, mtcars) #Does not make sense

stripplot(mpg~ factor(cyl), data = mtcars,
          jitter.data = TRUE, alpha = 0.6,
          main = "Miles per gallon per cylinders",
          xlab = "Cylinders",
          ylab = "MPG")

stripplot(mpg~ factor(cyl), 
          groups=factor(am), 
          data = mtcars,
          jitter.data = TRUE, 
          alpha = 0.6,
          main = "Miles per gallon per cylinders",
          xlab = "Cylinders",
          ylab = "MPG", pch=19, cex=2, auto.key=TRUE)

dotplot(mpg~factor(gear) | factor(cyl), mtcars, 
         layout = c(3, 1))

dotplot(mpg~factor(gear), groups= factor(cyl), mtcars, 
        pch=19, cex=2, auto.key=TRUE)