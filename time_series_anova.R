############### SESYNC Research Support: Time Series Anova ########## 
##
##
## This is an example script using mixed models to test differences in time series
## for different locations/subjects. Autocorrelation structures is taken into account
## in the methods.
##
## DATE CREATED: 09/27/2018
## DATE MODIFIED: 10/01/2018
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Time series ANOVA
## ISSUE: 
## TO DO:
##
## COMMIT: checking code
##

## Very good reference:
#http://rpsychologist.com/r-guide-longitudinal-lme-lmer

###################################################
#

###### Library used

############
library(lme)
library(nlme)
library(TSA)
library(zoo)
library(xts)


data(Ovary)
names(Ovary)

plot(Ovary$Time)

View(Ovary)
plot(1:10)
plot(Ovary)
xyplot(follicles~Time|Mare,data=Ovary)

## linear model

mod1 <- lme(follicles~ Time,
           random= ~1|Mare,
           data=Ovary)

#plot(ACF(mod1$residuals),alpha=0.05)
plot(ACF(mod1),alpha=0.05)
acf(mod1$residuals)
pacf(mod1$residuals) #note that residuals have both fixed and random outputs

mod1_lm4 <- lmer(follicles~ Time + 1|Mare,
            data=Ovary)

## model with cyclic behavior
mod2 <- lme(follicles~sin(2*pi*Time)+cos(2*pi*Time),
                      random= ~1|Mare,
           data=Ovary)
plot(ACF(mod2),alpha=0.05) # We see temporal autocorrelation in the residuals

#### model 3: Add autocorrelation in the model
mod3 <- lme(follicles ~ sin(2*pi*Time)+cos(2*pi*Time),
            random= ~1|Mare,
            correlation=corARMA(q=2),
            data=Ovary)
anova(mod1,mod2)
anova(mod2,mod3)

plot(ACF(mod3),alpha=0.05) #checking autocorrelation

#### model 4: different autocorrelation model
mod4 <- lme(follicles ~ sin(2*pi*Time)+cos(2*pi*Time),
            random= ~1|Mare,
            correlation=corAR1(),
            data=Ovary)
anova(mod1,mod2)
anova(mod3,mod4)
anova(mod1,mod2,mod3,mod4)

plot(ACF(mod4),alpha=0.05)

#This suggests corAR1 might be better but the drop in AIC is low. Check warning sign about REML.
#
############################# End of script ###############################