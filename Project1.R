####################################################################################################################################
# Stat 6021 Project 1
####################################################################################################################################
# Add library for boxcox plots
library(MASS)

data<- read.csv(file = 'diamonds4.csv', header=TRUE)
attach(data)
names(data)

plot(carat, price, xlab="Weight (in Carats)", ylab="Price of the Diamond (in Dollars)", main="Plot of Diamond Price vs. Carat Weight")

####################################################################################################################################
# SLR price ~ carat 
####################################################################################################################################

slrPriceCarat<-lm(price~carat)
summary(slrPriceCarat)

# Output:
# Call:
# lm(formula = price ~ carat)

# Residuals:
#  Min     1Q Median     3Q    Max 
# -49375  -5048   1867   4965 236711 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -13550.9      559.7  -24.21   <2e-16 ***
#  carat        25333.9      494.4   51.24   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 13560 on 1212 degrees of freedom
# Multiple R-squared:  0.6842,	Adjusted R-squared:  0.6839 
# F-statistic:  2625 on 1 and 1212 DF,  p-value: < 2.2e-16
  
plot(slrPriceCarat$fitted.values,slrPriceCarat$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

acf(slrPriceCarat$residuals, main="ACF of Residuals")

qqnorm(slrPriceCarat$residuals)
qqline(slrPriceCarat$residuals, col="red")

boxcox(slrPriceCarat, lambda = seq(0, .5, 0.2))

####################################################################################################################################
# SLR log(price) ~ carat 
####################################################################################################################################

price_log<-log(price)
slrPriceCarat.log_price<-lm(price_log~carat)
summary(slrPriceCarat.log_price)

# Output
# Call:
# lm(formula = price_log ~ carat)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0128 -0.4350  0.0067  0.4139  1.8178 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.43235    0.02490  258.37   <2e-16 ***
#   carat        1.45739    0.02199   66.27   <2e-16 ***

#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.6032 on 1212 degrees of freedom
# Multiple R-squared:  0.7837,	Adjusted R-squared:  0.7835 
# F-statistic:  4392 on 1 and 1212 DF,  p-value: < 2.2e-16

# Observations:
# R-squared value improved

plot(slrPriceCarat.log_price$fitted.values,slrPriceCarat.log_price$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

acf(slrPriceCarat.log_price$residuals, main="ACF of Residuals")

qqnorm(slrPriceCarat.log_price$residuals)
qqline(slrPriceCarat.log_price$residuals, col="red")


boxcox(slrPriceCarat.log_price, lambda = seq(-2, 10, .2))

####################################################################################################################################
# SLR log(price) ~ log(carat) 
####################################################################################################################################

carat_log<-log(carat)
slrPriceCarat.log_price_carat<-lm(price_log~carat_log)
summary(slrPriceCarat.log_price_carat)

# Output:
# Call:
# lm(formula = price_log ~ carat_log)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -0.96394 -0.17231 -0.00252  0.14742  1.14095 

# Coefficients:
#               Estimate Std. Error t value   Pr(>|t|)    
# (Intercept)   8.521208   0.009734   875.4   <2e-16 ***
#   carat_log   1.944020   0.012166   159.8   <2e-16 ***

#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2761 on 1212 degrees of freedom
# Multiple R-squared:  0.9547,	Adjusted R-squared:  0.9546 
# F-statistic: 2.553e+04 on 1 and 1212 DF,  p-value: < 2.2e-16

# Observations:
# R-squared value improved quite a bit

plot(carat_log, price_log, xlab="Log(Weight (in Carats))", ylab="Log(Price of the Diamond (in Dollars))", main="Plot of Log(Diamond Price) vs. Log(Carat Weight)")

plot(slrPriceCarat.log_price_carat$fitted.values,slrPriceCarat.log_price_carat$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

acf(slrPriceCarat.log_price_carat$residuals, main="ACF of Residuals")

qqnorm(slrPriceCarat.log_price_carat$residuals)
qqline(slrPriceCarat.log_price_carat$residuals, col="red")

boxcox(slrPriceCarat.log_price_carat, lambda = seq(-0.5, 1.5, .2))



