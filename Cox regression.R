setwd("C:/Users/admin/OneDrive/Desktop/SW-CLHLS/00-Recoded Data")
getwd()

#retrieve from local library
library(survival)
#require
require(survival)
require(dplyr)
require(haven)
require(data.table)

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
################################Clean Socio-demographics Variables--Education ###########
# --------------------------------------------------------------------------------------#
ses08_18 <- dat08_18 %>%
    mutate(education = f1) %>%
    mutate(education = ifelse(education == 88 | education == 99, NA, education)) 
ses11_18 <- dat11_18 %>%
    mutate(education = f1) %>%
    mutate(education = ifelse(education == 88 | education == 99, NA, education))
ses14_18 <- dat14_18 %>%
    mutate(education = f1) %>%
    mutate(education = ifelse(education == 88 | education == 99, NA, education))
# ------------------------------------------------------------------------------------- #
###############################Clean Socio-demographics Variables--Income ###############
# --------------------------------------------------------------------------------------#
ses08_18 <- ses08_18 %>%
    mutate(income = f35) %>%
    mutate(income = ifelse(income == 99998 | income == 99999, NA, income)) 
ses11_18 <- ses11_18 %>%
    mutate(income = f35) %>%
    mutate(income = ifelse(income == 99998 | income == 99999, NA, income)) 
ses14_18 <- ses14_18 %>%
    mutate(income = f35) %>%
    mutate(income = ifelse(income == 99998 | income == 99999, NA, income)) 
# ------------------------------------------------------------------------------------- #
###############################Clean Socio-demographics Variables--Occupation ###########
# --------------------------------------------------------------------------------------#
ses08_18 <- ses08_18 %>% mutate(occupation = f2)
ses11_18 <- ses11_18 %>% mutate(occupation = f2) 
ses14_18 <- ses14_18 %>% mutate(occupation = f2) 
# ------------------------------------------------------------------------------------- #
###############################Clean Socio-demographics Variables--Ethnicity ############
# --------------------------------------------------------------------------------------#
ses08_18 <- ses08_18 %>% mutate(ethnicity = a2) %>% select(id, education, income, occupation, ethnicity)
ses11_18 <- ses11_18 %>% mutate(ethnicity = a2) %>% select(id, education, income, occupation, ethnicity)
ses14_18 <- ses14_18 %>% mutate(ethnicity = a2) %>% select(id, education, income, occupation, ethnicity)
# ------------------------------------------------------------------------------------- #
###############################Clean Lifestyle Variables-- Smoking ######################
# --------------------------------------------------------------------------------------#
lif08_18 <- dat08_18 %>% 
    mutate(smoking = d71)%>%
    mutate(smoking = ifelse(smoking == 9, NA, smoking)) 
lif11_18 <- dat11_18 %>% 
    mutate(smoking = d71)%>%
    mutate(smoking = ifelse(smoking == 9, NA, smoking)) 
lif14_18 <- dat14_18 %>% 
    mutate(smoking = d71)%>%
    mutate(smoking = ifelse(smoking == 9, NA, smoking)) 
# ------------------------------------------------------------------------------------- #
###############################Clean Lifestyle Variables--Drinking ######################
# --------------------------------------------------------------------------------------#
lif08_18 <- lif08_18 %>%
    mutate(drinking = d81) %>%
    mutate(drinking = ifelse(drinking == 9, NA, drinking)) 
lif11_18 <- lif11_18 %>%
    mutate(drinking = d81) %>%
    mutate(drinking = ifelse(drinking == 9, NA, drinking)) 
lif14_18 <- lif14_18 %>%
    mutate(drinking = d81) %>%
    mutate(drinking = ifelse(drinking == 9, NA, drinking)) 
# ------------------------------------------------------------------------------------- #
###############################Clean Lifestyle Variables-- Physical Activity ############
# --------------------------------------------------------------------------------------#
lif08_18 <- lif08_18 %>% 
    mutate(activity = d91) %>% 
    mutate(activity = ifelse(activity == 8 | activity == 9, NA, activity)) %>%
    select(id, smoking, drinking, activity)
lif11_18 <- lif11_18 %>% 
    mutate(activity = d91) %>% 
    mutate(activity = ifelse(activity == 8 | activity == 9, NA, activity)) %>%
    select(id, smoking, drinking, activity)
lif14_18 <- lif14_18 %>% 
    mutate(activity = d91) %>% 
    mutate(activity = ifelse(activity == 8 | activity == 9, NA, activity)) %>%
    select(id, smoking, drinking, activity)
# ------------------------------------------------------------------------------------- #
###################################### Bind dataset & Drop  #############################
# --------------------------------------------------------------------------------------#
# bind ses
data_ses <- ses08_18 %>% bind_rows(ses11_18,ses14_18)
# bind lifestyle
data_lif <- lif08_18 %>% bind_rows(lif11_18,lif14_18)
# bind hr
data_hr <- hr08_18 %>% bind_rows(hr11_18,hr14_18)
# change hr=999 -> NA
data_hr <- data_hr %>% mutate(hr = ifelse(hr == 888 | hr == 999, NA, hr))
# bind weight
data_wgt <- wgt08_18 %>% bind_rows(wgt11_18,wgt14_18)
# bind height
data_hgt <- hgt08_18 %>% bind_rows(hgt11_18,hgt14_18)
## drop Obs with problematic heart rate
data_hr <- data_hr %>% mutate(hr = ifelse(hr >= 200, NA, hr)) %>% filter(is.na(hr) == FALSE)
data_hr <- data_hr %>% mutate(hr = ifelse(hr <= 30, NA, hr)) %>% filter(is.na(hr) == FALSE)
# drop abnormal values of weight
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight < 30, NA, weight)) %>% filter(is.na(weight) == FALSE)
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight > 120, NA, weight)) %>% filter(is.na(weight) == FALSE)
# drop abnormal values of height
data_hgt <- data_hgt %>% mutate(height = ifelse(height < 100, NA, height)) %>% filter(is.na(height) == FALSE)
data_hgt <- data_hgt %>% mutate(height = ifelse(height > 200, NA, height)) %>% filter(is.na(height) == FALSE)

# calculate bmi
data_bmi<-left_join(data_hgt, data_wgt, by = c("id"="id", "trueage"="trueage")) %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id, trueage, bmi)
# calculate bmi*hr
data_bmihr <-left_join(data_bmi,data_hr,by = c("id"="id", "trueage"="trueage")) %>%
    mutate(bmihr =(bmi*hr))

# ------------------------------------------------------------------------------------- #
############################# Clean Abnormal Obs ########################################
# --------------------------------------------------------------------------------------#
## drop Obs with problematic bmi
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi > 60, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi < 10, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
summary(data_bmihr$bmi)
## drop Obs with NA bmi*hr
data <- data_bmihr %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)

# merge with survival time and status
data_surtime <- read_dta("idnum_survival18.dta")
data_surtime <- data_surtime[, c(1, 3:5)]
data <- data %>% left_join(data_surtime, by = "id")
## recode bmi*hr to quartile
summary(data_bmihr$bmihr)

# ------------------------------------------------------------------------------------- #
###################################### Combine data with ses & lifestyle #################################
# --------------------------------------------------------------------------------------#
library(survival)
data<-left_join(data, data_ses, by="id")
data<-left_join(data, data_lif, by="id")

# ------------------------------------------------------------------------------------- #
###################################### Run COX Regression Model #################################
# --------------------------------------------------------------------------------------#
cox.mod1 <- coxph( Surv(data$trueage, data$censor18) ~ data$bmihr+data$education+data$income+data$occupation+data$ethnicity)
summary(cox.mod1)
cox.mod2 <- coxph( Surv(data$trueage, data$censor18) ~ data$bmihr+data$smoking+data$drinking+data$activity)
summary(cox.mod2)
cox.mod3 <- coxph( Surv(data$trueage, data$censor18) ~ data$bmihr+data$education+data$income+data$occupation+data$ethnicity+data$smoking+data$drinking+data$activity)
summary(cox.mod3)

#Conduct `backward and forward selection` for cox model
cox.step <- step(cox.mod3, direction = "both")
summary(cox.step) 

##exp(coef)= Hazard Ratio 1.68 means 68% higher risk to die
##concordance = se = area under the curve
cox.mod2 <- coxph( Surv(Time, Status) ~ Over40)
##compare models 
anova(cox.mod2, cox.mod, test="LRT")
