setwd("C:/Users/admin/OneDrive/Desktop/SW-CLHLS/00-Recoded Data")
getwd()

#retrieve from local library
library(survival)
#require
require(survival)

# ------------------------------------------------------------------------------------------ #
###################################### import data of all waves ##############################
# ------------------------------------------------------------------------------------------ #
library(haven)
dat08_18 <- read_stata("dat08_18.dta")
dat11_18 <- read_stata("dat11_18.dta")
dat14_18 <- read_stata("dat14_18_1125.dta")
# ------------------------------------------------------------------------------------- #
###################################### Clean Heart Rate #################################
# --------------------------------------------------------------------------------------#
hr11_18 <- dat11_18 %>% mutate(hr = g7) %>% select(id, a1, trueage, hr)
hr14_18 <- dat14_18 %>% mutate(hr = g7) %>% select(id, a1, trueage, hr)
hr08_18 <- dat08_18 %>% mutate(hr = (g71 + g72)/2) %>% select(id, a1, trueage, hr)
# ------------------------------------------------------------------------------------- #
###################################### Clean Weight #################################
# --------------------------------------------------------------------------------------#
wgt08_18 <- dat08_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, trueage, weight)
wgt11_18 <- dat11_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, trueage, weight)
wgt14_18 <- dat14_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, trueage, weight)
# ------------------------------------------------------------------------------------- #
###################################### Clean Height #################################
# --------------------------------------------------------------------------------------#
hgt08_18 <- dat08_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, trueage, height)
hgt11_18 <- dat11_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, trueage, height)
hgt14_18 <- dat14_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, trueage, height)
# ------------------------------------------------------------------------------------- #
###################################### Run COX Regression Model #################################
# --------------------------------------------------------------------------------------#
Stan$Over40<- as.factor(Stan$Over40)
Stan$MisMatchLevel <- as.factor(Stan$MisMatchLevel)
summary(data1)
library(survival)
cox.mod1 <- coxph( Surv(data$trueage, data$censor18) ~ data$bmihr)
summary(cox.mod1)
cox.mod2 <- coxph( Surv(data$trueage, data$censor18) ~ data$bmi)
summary(cox.mod2)
cox.mod3 <- coxph( Surv(data$trueage, data$censor18) ~ data$hr)
summary(cox.mod3)

##exp(coef)= Hazard Ratio 1.68 means 68% higher risk to die
##concordance = se = area under the curve
cox.mod2 <- coxph( Surv(Time, Status) ~ Over40)
##compare models 
anova(cox.mod2, cox.mod, test="LRT")