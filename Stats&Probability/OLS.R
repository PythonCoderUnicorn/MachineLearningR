
# Ordinary Least Squares - Linear Regression
# 
# Ordinary Least Squares (OLS) linear regression is a statistical technique 
# used for the analysis and modelling of linear relationships between 
# a response variable and one or more predictor variables. 
# If the relationship between two variables appears to be linear, 
# then a straight line can be fit to the data in order to model 
# the relationship. The linear equation (or equation for a straight line) 
# for a bivariate regression takes the following form:  y = mx + c

# y is dependent variable
# m is slope
# x is predictor (indep)
# x is the intercept

# Line of best fit calculation uses:
#   deviations of x & y
#   means of x & y
#   Pearson correlation coefficient of x & y

# use scatterplot to assess linearity

# slope = cor(x, y) * (sd(y) / sd(x))
# 
# intercept = mean(y) - (slope * mean(x))



# example -----------------------------------------------------------------


#  disp (displacement) = measures overall volume in engine as a factor of 
#       cylinder circumference depth and total number of cylinder.
#       {total power the engine can generate}

library(tidyverse)
mtcars = mtcars

mtcars %>% 
  ggplot( aes(x= disp, y= mpg))+
  geom_point()


# step 1 - look at strength of relationship with Pearson
cor(mtcars$disp, mtcars$mpg)
# = -0.8475514

# a strong negative correlation [ correlation != causation ]





# non-linear relationship -------------------------------------------------

# transform the response and predictor 
# coerce variables using log10, sqrt(), inverse transformations

# for this example strengthen the relationship
x = sqrt(mtcars$disp)
y = sqrt(mtcars$mpg)
cor(x, y )
# = -0.8929046

# -------------------------------------------------------------------------




# step 2 - statistically signif --------------------------------------------

# check the variance of data points on fitted line
#   use the RMSE root squared mean error metric
#   = variance in {response} var by {predictor} var
#   R^2 is Pearson correlation coefficient squared

#   rule of thumb: >= 20 data point == valid model
#   p-value is probability there being _no_relationship_ (null hypothesis)


# --- linear model
x_cars = sqrt(mtcars$disp)
y_cars = sqrt(mtcars$mpg)

mtcars %>% 
  ggplot( aes(x = x_cars, y= y_cars))+
  geom_point()+
  # 'lm' puts the straight line
  geom_smooth(method = 'lm', fill= NA, color='pink') 



# --- model object 

lin.model = lm( sqrt(mtcars$mpg) ~ sqrt(mtcars$disp), data = mtcars)

# - get the slope & intercept
lin.model$coefficients

# (Intercept) sqrt(mtcars$disp) 
# 6.5192052        -0.1424601 


#-- model summary 

summary(lin.model)

# Call:
#   lm(formula = sqrt(mtcars$mpg) ~ sqrt(mtcars$disp), data = mtcars)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.45591 -0.21505 -0.07875  0.16790  0.71178 
# 
# Coefficients:
#                     Estimate  Std. Error t value Pr(>|t|)    
#   (Intercept)        6.51921    0.19921   32.73  < 2e-16 ***
#   sqrt(mtcars$disp) -0.14246    0.01312  -10.86 6.44e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3026 on 30 degrees of freedom
# Multiple R-squared:  0.7973,	Adjusted R-squared:  0.7905 
# F-statistic:   118 on 1 and 30 DF,  p-value: 6.443e-12


# the p-value is 6.44e-12 ***
# R^2 is 0.7973

# take away: for every unit increase in sqrt(disp) == -0.14246 in sqrt(mpg)
# >>>> fuel efficiency decreases with increased engine displacement 









