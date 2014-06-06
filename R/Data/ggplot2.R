rm(list=ls(all=TRUE))
setwd("C:/Users/admin/Desktop/ElearningB2/Sess11")

# Introduction to ggplot2 and the mpg dataset
library(ggplot2)
library(lattice)

# Look at the data we're going to use
attach(mpg)
dim(mpg)
str(mpg)
summary(mpg)
head(mpg)
sum(is.na(mpg))

xyplot(hwy~displ, data = mpg, 
       cex=3, pch=15, 
       groups=class, 
       auto.key=TRUE)
histogram(hwy)
bwplot(hwy)
barchart(class)

xyplot(hwy~ factor(cyl), 
       groups=class, data = mpg,
          jitter.data = TRUE, alpha = 0.6, auto.key=TRUE)

qplot(data=mpg, 
      x=factor(cyl), y=hwy, 
      color=class, size=cty)

qplot(data=mpg, 
      x=factor(cyl), y=hwy, 
      color=class, size=displ, 
      shape=factor(drv), 
      geom="jitter")

qplot(data=mpg, 
      x=factor(cyl), y=hwy, color=factor(drv), size=displ, shape=fl, 
      geom="jitter")


xyplot(hwy~displ|factor(cyl)+drv, data = mpg)
qplot(displ, hwy, data = mpg) + facet_grid(drv ~ cyl)+geom(jitter)


qplot(displ, hwy, data = mpg) + facet_wrap(~ class)
qplot(displ, hwy, data = mpg) + facet_grid(~ class)

qplot(data = mpg, 
      x=cty, y=hwy, color=factor(year), 
      geom = "jitter")+
  geom_smooth(method = "lm")

xyplot(hwy~jitter(cty), data = mpg, groups=year,
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(lm(y ~ x))
       })

bestfit <- geom_smooth(method = "lm", se = T, 
                       colour = "red", size = 1)

qplot(data = mpg, 
      x=cty, y=hwy, color=year,
      geom = "jitter")+
  bestfit


bwplot(hwy~reorder(class, hwy, 
                   median), 
       data = mpg)

qplot(data = mpg, 
      x=class, y=hwy, 
      geom = "boxplot")

qplot(data = mpg, 
      x=reorder(class, hwy, median), y=hwy, 
      geom = "boxplot")

qplot(data = mpg, 
      x=reorder(class, hwy, median), y=hwy, 
      geom = "point")


qplot(data = mpg, 
      x=reorder(class, hwy, median), y=hwy, 
      geom = c("jitter", "boxplot"))

qplot(data = mpg, 
      x=reorder(class, hwy), y=hwy, color=cyl, shape=drv,
      geom = c("boxplot", "jitter"))

qplot(data = mpg,
      x=hwy, 
      geom="density", 
      bin=10)

qplot(data = diamonds, 
      x=carat, y=price, 
      alpha=I(1/10))

#GGPLOT function

qplot(data = mpg, 
      x=reorder(class, hwy, median), 
      y=hwy, color=cyl, shape=drv, 
      geom = c("boxplot", "jitter"))

qplot(data = mpg, 
      x=reorder(class, hwy, median), 
      y=hwy, color=cyl, shape=drv, 
      geom = c("jitter", "boxplot"))


ggplot(data=mpg, 
       aes(x=reorder(class, hwy, median), 
           y=hwy, 
           color=factor(cyl), shape=drv)) +
  geom_jitter(size=3)+ 
  geom_boxplot(aes(x=reorder(class, hwy, median), 
                   y=hwy, 
                   color=NULL, shape=NULL))+
  scale_x_discrete(name="Class")+
  scale_y_continuous(name="Highway mileage")+
  theme_bw()

