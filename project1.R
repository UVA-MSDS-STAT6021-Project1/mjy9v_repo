#!/usr/bin/env Rscript

# Drake Wagner
# dbw2tn
# dbw2tn@virginia.edu
setwd('~/git/stat-6021')                  # LINUX
# setwd('C:\\Users\\dwagn\\git\\stat-6021') # WINDOWS

library('ggplot2')

dia <- read.csv('diamonds4.csv')

ggplot(data=dia, aes(x=carat, y=price)) + 
  geom_point() + 
  geom_smooth(method='lm', formula= y~x, se=FALSE, col='red')

attach(dia)
slr_model_1 <- lm(price~carat)
summary(slr_model_1)

ggplot(data=dia, aes(x=slr_model_1$fitted.values, y=slr_model_1$residuals)) +
  geom_point() +
  labs(title='Residuals Against Fitted Values') +
  ylab('Residuals') +
  xlab('Fitted values')

log_price <- log(price)
slr_model_2 <- lm(log_price~carat)
summary(slr_model_2) # significant!

ggplot(data=dia, aes(x=slr_model_2$fitted.values, y=slr_model_2$residuals)) +
  geom_point() +
  labs(title='Log Residuals Against Fitted Values') +
  ylab('Log Res') +
  xlab('Log FV')

acf(slr_model_2$residuals, main="ACF of Res") # ACF of residuals


########
# Price in relation to carat and color
crt_col_int <- lm(price~carat*color)
summary(crt_col_int)
# All colors except for Color E are significant, suggesting that carat and color
# are jointly correlated with the price of the diamond.

acf(crt_col_int$residuals)
qqnorm(crt_col_int$residuals)
qqline(crt_col_int$residuals)
# All colors except for Color E are significant, suggesting that carat and color
# are jointly correlated with the price of the diamond.




plot(x=carat, y=price)
sub1<-subset(dia, color=='D')
sub2<-subset(dia, color=='E')
sub3<-subset(dia, color=='F')
sub4<-subset(dia, color=='G')
sub5<-subset(dia, color=='H')
sub6<-subset(dia, color=='I')
sub7<-subset(dia, color=='J')


reg1<-lm(price~carat, data=sub1)
reg2<-lm(price~carat, data=sub2)
reg3<-lm(price~carat, data=sub3)
reg4<-lm(price~carat, data=sub4)
reg5<-lm(price~carat, data=sub5)
reg6<-lm(price~carat, data=sub6)
reg7<-lm(price~carat, data=sub7)

abline(reg1, col="red")
abline(reg2, col="green")
abline(reg3, col="purple")
abline(reg4, col="orange")
abline(reg5, col="blue")
abline(reg6, col="cyan")
abline(reg7, col="brown")
abline(crt_col_int, col="blue", lty=2)

lm_3 <- lm(price~carat + color)
anova(slr_model_1, crt_col_int) 
anova(lm_3, crt_col_int) # carat*color vs carat+color


# H0 = The slope of the interaction terms is zero
# HA = At least one of the interaction terms has a slope other than zero

# We can see from our ANOVA table that the relationship between color and carat
# in regards to price is a significant interaction. We also see that the Adjusted 
# R-squared valued is 0,7694, leading us to believe that 75% of the variation in 
# our response variable is due to the predictors, which is a respectable number. 
# Therefore, we will reject our null hypothesis and keep the interaction terms in our reduced model. 


