
# Covariance - simple linear regression

# simple linear regression model considers the relationship 
# between two variables and in many cases more information will 
# be available that can be used to extend the model.

# categorical variable (sometimes known as a covariate) that 
# can be used to divide the data set to fit a separate linear 
# regression to each of the subsets. 

# Orange tree dataset
df = Orange

head(df)
str(df)
# Tree has factors => numeric type

df$Tree = factor( as.numeric(df$Tree))


#  model assumes that the relationship between circumference and age 
orange_model = lm( circumference ~ age, data = df)

# summary of model
summary(orange_model)


# -------------------------------------------------------------------------
# Call:
#   lm(formula = circumference ~ age, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -46.310 -14.946  -0.076  19.697  45.111 
# 
# Coefficients:
#              Estimate  Std. Error t value Pr(>|t|)    
# (Intercept) 17.399650   8.622660   2.018   0.0518 .  
# age          0.106770   0.008277  12.900 1.93e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 23.74 on 33 degrees of freedom
# Multiple R-squared:  0.8345,	Adjusted R-squared:  0.8295 
# F-statistic: 166.4 on 1 and 33 DF,  p-value: 1.931e-14
# -------------------------------------------------------------------------

# age is strong indicator of increase in circumference with age


# ::: does the age consistently between trees at same rates ??

orange_model2 = lm(circumference ~ age + Tree, data = df)
summary(orange_model2)

# -------------------------------------------------------------------------
# Call:
#   lm(formula = circumference ~ age + Tree, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -30.505  -8.790   3.737   7.650  21.859 
# 
# Coefficients:
#               Estimate  Std. Error t value   Pr(>|t|)    
#   (Intercept) -4.457493   7.572732  -0.589     0.5607    
#   age          0.106770   0.005321  20.066    < 2e-16 ***
#   Tree2        5.571429   8.157252   0.683     0.5000    
#   Tree3       17.142857   8.157252   2.102     0.0444 *  
#   Tree4       41.285714   8.157252   5.061 0.00002140 ***
#   Tree5       45.285714   8.157252   5.552 0.00000548 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.26 on 29 degrees of freedom
# Multiple R-squared:  0.9399,	Adjusted R-squared:  0.9295 
# F-statistic:  90.7 on 5 and 29 DF,  p-value: < 2.2e-16
# -------------------------------------------------------------------------

# tree 1 is the baseline to compare other trees

# compare the two models using an F-test for nested models using the anova function

anova(orange_model, orange_model2)


# -------------------------------------------------------------------------
# Analysis of Variance Table
# 
# Model 1: circumference ~ age
# Model 2: circumference ~ age + Tree
#   Res.Df     RSS Df  Sum of Sq      F      Pr(>F)    
# 1     33 18594.7                                    
# 2     29  6753.9  4     11841   12.711   0.000004289 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# -------------------------------------------------------------------------

#  4 degrees of freedom, the test comparing the 2 model is highly significant
#  strong evidence of difference in starting circumference between trees



#  extended this model further by allowing the rate of increase 
#  in circumference to vary between the five trees.

orange_model3 = lm(circumference ~ age + Tree + age:Tree, data= df)
summary(orange_model3)



# -------------------------------------------------------------------------
# Call:
#   lm(formula = circumference ~ age + Tree + age:Tree, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -18.061  -6.639  -1.482   8.069  16.649 
# 
# Coefficients:
#                 Estimate  Std. Error  t value  Pr(>|t|)    
#   (Intercept)  19.2035364   8.4582822   2.270  0.03206 *  
#   age           0.0811116   0.0081188   9.991 3.27e-10 ***
#   Tree2         5.2343103  11.9618174   0.438  0.66544    
#   Tree3       -10.4451918  11.9618174  -0.873  0.39086    
#   Tree4         0.7573670  11.9618174   0.063  0.95002    
#   Tree5        -4.5659162  11.9618174  -0.382  0.70590    
#   age:Tree2     0.0003656   0.0114818   0.032  0.97485    
#   age:Tree3     0.0299173   0.0114818   2.606  0.01523 *  
#   age:Tree4     0.0439502   0.0114818   3.828  0.00077 ***
#   age:Tree5     0.0540606   0.0114818   4.708 7.93e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.41 on 25 degrees of freedom
# Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9672 
# F-statistic: 112.4 on 9 and 25 DF,  p-value: < 2.2e-16

# -------------------------------------------------------------------------

#  strong evidence of a difference in the rate of 
#  change in circumference for the five trees.


library(lattice)
xyplot(circumference ~ age | Tree, data = df,
       panel = function(x, y, ...)
       {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       }
)


xyplot(resid(orange_model3) ~ fitted(orange_model3) | df$Tree,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residual Diagnostic Plot",
       panel = function(x, y, ...)
       {
         panel.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       }
)






