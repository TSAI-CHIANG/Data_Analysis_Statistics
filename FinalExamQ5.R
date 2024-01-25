##Covariance & Correlation
install.packages("MVA")
install.packages("HSAUR2")
#
library(MVA)
library(HSAUR2)

#𝑃𝑜𝑝𝑢𝑙𝑎𝑟𝑖𝑡𝑦 𝑠𝑐𝑜𝑟𝑒=𝛼 + 𝛽1𝐻𝑜𝑡𝑛𝑒𝑠𝑠 + 𝛽2𝑃𝑒𝑟𝑖𝑜𝑑
##Regression
load("/Users/christine/Documents/NCCU/Course 112Fall/研究方法/R/R Data - Boy band data set.rdata")
attach(d)
#head(d)
#plot(d)

# Convert 'hotness' to a factor since it's a categorical variable??
#'d$hotness <- as.factor(d$hotness)

fit.1=lm(popularity_score~hotness+period_c, data=d)
#fit.1=lm(popularity_score~hotness+period_c, data=d)??
# Output the coefficients of the model
coef(fit.1)
# Show a summary of the model
summary(fit.1)


###quantile regression
install.packages("quantreg")
install.packages("AER")
#
library(quantreg)
library(AER)
#data("")
df_fit = popularity_score ~ hotness  + period_c
df_lad = rq(df_fit, data=d) #default=0.5 (τ=0.5, i.e., median or LAD (“least absolute deviations”) regression)

#We can compare it to the OLS regression on mean
df_ols=lm(df_fit, data=d)
summary(df_lad) 
summary(df_ols)

#20%, 40%, 60% and 80% quantiles
df_rq=rq(df_fit, tau=c(0.20, 0.40, 0.60, 0.80), data=d)
summary(df_rq)
#
df_rq20=rq(df_fit, tau=0.20, data=d)
df_rq40=rq(df_fit, tau=0.40, data=d)
df_rq60=rq(df_fit, tau=0.60, data=d)
df_rq80=rq(df_fit, tau=0.80, data=d)
#anova(df_rq20, df_rq80) 

#create scatterplot 
plot(popularity_score ~ hotness, data=d, pch=16, main="OLS vs QR Regression")
#add fitted regression line to scatterplot
abline(fit.1, col="blue")
abline(df_lad, col="light blue")
abline(df_rq20, col="orange")
abline(df_rq40, col="green")
abline(df_rq60, col="purple")
abline(df_rq80, col="red")
legend("bottomright", legend=c("OLS", "LAD", "20th quantile", "40th quantile",
                                "60th quantile", "80th quantile"), 
                               col=c("blue", "light blue", "orange", "green", 
                                     "purple","red"),lty=1:2)

#tau from 0.2 ~ 0.8
#https://www.youtube.com/watch?v=cm29i-n53SM
#We can further visualize the results from quantile regression modeling.
df_rqbig=rq(df_fit, tau=seq(0.20, 0.80, 0.20), data=d)
df_rqbigs=summary(df_rqbig)
plot(df_rqbigs)

#Multilevel regression
library(lme4)
# Run random intercept and slope model
model <- lmer(popularity_score ~ hotness + (1 + hotness | band_identifier) + period_c, data=d)
summary(model)



