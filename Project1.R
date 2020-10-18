data <- read.csv("diamonds4.csv")
#bluenile.com

View(data)
#carat
#clarity
#color
#cut
#price

attach(data)
##exploring 'cut' data
length(unique(cut) #4 unique values
unique(cut) #unique values
table(cut) #frequency by unique values
is.numeric(cut)

##subsetting
a1<-subset(data,cut=="Good") 
a2<-subset(data,cut=="Very Good") 
a3<-subset(data,cut=="Ideal") 
a4<-subset(data,cut=="Astor Ideal") 

##fit regression lines
reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

##scatterplot of data with different colors for cut
jpeg("price by carat, by cut.jpg")
plot(carat, price, main="Price by Carat, by Cut")
points(a2$carat,a2$price, pch=2, col="blue") 
points(a3$carat,a3$price, pch=6, col="green")
points(a4$carat,a4$price, pch=12, col="red")
legend("topleft", c("Good","Very Good","Ideal", "Astor Ideal"),pch=c(1,2,6,12), col=c("black","blue", "green", "red")) 
dev.off()

##scatterplot of data with different colors for cut (zoomed in)
jpeg("graph.jpg")
plot(carat, price, main="Price by Carat, by Cut (zoomed in)", ylim= c(1, 50000))
points(a2$carat,a2$price, pch=2, col="blue") 
points(a3$carat,a3$price, pch=6, col="green")
points(a4$carat,a4$price, pch=12, col="red")
legend("topleft", c("Good","Very Good","Ideal", "Astor Ideal"),pch=c(1,2,6,12), col=c("black","blue", "green", "red")) 
dev.off()

##plot with fitted regression lines
jpeg("graph.jpg")
plot(carat, price, main="Price by Carat, by Cut (zoomed in)", ylim= c(1, 50000))
points(a2$carat,a2$price, pch=2, col="blue") 
points(a3$carat,a3$price, pch=6, col="green")
points(a4$carat,a4$price, pch=12, col="red")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col="green")
legend("topleft", c("Good","Very Good","Ideal", "Astor Ideal"), lty=c(1,2,3,4), pch=c(1,2,6,12), col=c("black","blue", "green", "red")) 
dev.off()

##evaluating regession with interaction
result<-lm(price~carat*cut) 
summary(result)

##evaluating regession with no interaction
reduced<-lm(price~carat+cut) #???Why does this work when cut isn't numeric? Are indicator varibles assigned behind the scenes?
anova(reduced,result)

##ACF plot of residuals
acf(reduced$residuals)

##QQ plot of residuals
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="purple")

##additional assumption to check with categorical predictor. Is the variance of the response variable constant between all classes of the categorical predictor?
boxplot(price~cut, main="Boxplot of price by cut")
boxplot(price~cut, main="Boxplot of price by cut", ylim= c(1, 10000))

##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(price,cut)

summary(reduced)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(reduced, linfct = mcp(cut= "Tukey"))
summary(pairwise)

reduced$coef

##obtain the variance-covariance matrix of the coefficients
vcov(reduced)

##save violations of assumptions to a jpg file
jpeg("graph.jpg")
par(mfrow=c(1,3))
acf(result$residuals)
qqnorm(result$residuals)
qqline(result$residuals, col="purple")
boxplot(price~cut, main="Boxplot of price by cut")
dev.off()

##save violations of assumptions (reduced model) to a jpg file
jpeg("graph.jpg")
par(mfrow=c(1,3))
acf(reduced$residuals)
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="purple")
boxplot(price~cut, main="Boxplot of price by cut")
dev.off()
