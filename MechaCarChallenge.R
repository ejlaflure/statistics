#read in datasets
mechaCar_mpg <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F)
suspension_coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F)

#Multiple Linear Regression

#generate multiple linear regression model
mlr <- lm(mpg ~ vehicle.weight + vehicle.length + ground.clearance + spoiler.angle,data=mechaCar_mpg)
#generate summary statistics
summary(mlr)

#non-random single linear regressions (for analysis comparison)

#generate single linear regression model
slr <- lm(mpg ~ vehicle.length,data=mechaCar_mpg)
#determine y-axis values from linear model
yvals <- slr$coefficients['vehicle.length']*mechaCar_mpg$vehicle.length + slr$coefficients['(Intercept)']
#import dataset into ggplot2, plot scatter and linear model
plt <- ggplot(mechaCar_mpg,aes(x=vehicle.length,y=mpg))
plt + geom_point() + geom_line(aes(y=yvals), color = "red")

#generate single linear regression model
slr <- lm(mpg ~ ground.clearance,data=mechaCar_mpg)
#determine y-axis values from linear model
yvals <- slr$coefficients['ground.clearance']*mechaCar_mpg$ground.clearance + slr$coefficients['(Intercept)']
#import dataset into ggplot2, plot scatter and linear model
plt <- ggplot(mechaCar_mpg,aes(x=ground.clearance,y=mpg))
plt + geom_point() + geom_line(aes(y=yvals), color = "red")

#Suspension Coil Summary

#generate summary statistics
summary(suspension_coil$PSI)

#Suspension Coil One Sample t-test

#randomly sample 50 data points
sample_suspension_coil_data <- suspension_coil %>% sample_n(50)
#compare sample versus population means
t.test(sample_suspension_coil_data$PSI,mu=mean(suspension_coil$PSI))