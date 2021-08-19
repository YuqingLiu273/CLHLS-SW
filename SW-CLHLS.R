#retrieve from local library
library(tidyverse)
library(data.table)
library(haven)
library(sjlabelled)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
#require
require(tidyverse)
require(data.table)
require(haven)
require(sjlabelled)
require(survival)
require(survminer)
require(ggplot2)
require(dplyr)

setwd("E:/Jenny/Downloads/Recoded Data")
getwd()
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
hr11_18 <- dat11_18 %>% mutate(hr = g7) %>% select(id, a1, hr)
hr14_18 <- dat14_18 %>% mutate(hr = g7) %>% select(id, a1, hr)
hr08_18 <- dat08_18 %>% mutate(hr = (g71 + g72)/2) %>% select(id, a1, hr)
# ------------------------------------------------------------------------------------- #
###################################### Clean Weight #################################
# --------------------------------------------------------------------------------------#
wgt08_18 <- dat08_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, weight)
wgt11_18 <- dat11_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, weight)
wgt14_18 <- dat14_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, a1, weight)
# ------------------------------------------------------------------------------------- #
###################################### Clean Height #################################
# --------------------------------------------------------------------------------------#
hgt08_18 <- dat08_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, height)
hgt11_18 <- dat11_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, height)
hgt14_18 <- dat14_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, a1, height)
# ------------------------------------------------------------------------------------- #
###################################### Bind dataset & Drop  #############################
# --------------------------------------------------------------------------------------#
# bind hr
data_hr <- hr08_18 %>% bind_rows(hr11_18,hr14_18)
# change hr=999 -> NA
data_hr <- data_hr %>% mutate(hr = ifelse(hr == 888 | hr == 999, NA, hr))

#separate male and female
hr_m <- subset(data_hr,a1=="1",select=c(id, a1, hr))
hr_f <- subset(data_hr,a1=="2",select=c(id, a1, hr))

# Boxplot of male heartrate before dropping
box_hr1 <- boxplot(hr_m$hr, main="boxplot of male heartrate before dropping", ylab="heartrate",col=c("coral1"))

## drop Obs with problematic heart rate
hr_m <- hr_m %>% mutate(hr = ifelse(hr >= 200, NA, hr)) %>% filter(is.na(hr) == FALSE)
hr_m <- hr_m %>% mutate(hr = ifelse(hr <= 30, NA, hr)) %>% filter(is.na(hr) == FALSE)

# Boxplot of male heartrate after dropping
box_hr2 <- boxplot(hr_m$hr, main="boxplot of male heartrate after dropping", ylab="heartrate",col=c("coral1"))

# Boxplot of female heartrate before dropping
box_hr3 <- boxplot(hr_f$hr, main="boxplot of female heartrate before dropping", ylab="heartrate",col=c("coral1"))

## drop Obs with problematic heart rate
hr_f <- hr_f %>% mutate(hr = ifelse(hr >= 200, NA, hr)) %>% filter(is.na(hr) == FALSE)
hr_f <- hr_f %>% mutate(hr = ifelse(hr <= 30, NA, hr)) %>% filter(is.na(hr) == FALSE)

# Boxplot of female heartrate after dropping
box_hr4 <- boxplot(hr_f$hr, main="boxplot of female heartrate after dropping", ylab="heartrate",col=c("coral1"))

# bind weight
data_wgt <- wgt08_18 %>% bind_rows(wgt11_18,wgt14_18)

#separate male and female
wgt_m <- subset(data_wgt,a1=="1",select=c(id, a1, weight))
wgt_f <- subset(data_wgt,a1=="2",select=c(id, a1, weight))
 
# Boxplot of male weight before dropping
box_wgt1 <- boxplot(wgt_m$weight, main="boxplot of male weight before dropping", ylab="weight", col=c("coral1"))

# drop abnormal values of weight
wgt_m <- wgt_m %>% mutate(weight = ifelse(weight < 35, NA, weight)) %>% filter(is.na(weight) == FALSE)
wgt_m <- wgt_m %>% mutate(weight = ifelse(weight > 90, NA, weight)) %>% filter(is.na(weight) == FALSE)

# Boxplot of male weight after dropping
box_wgt2 <- boxplot(wgt_m$weight, main="boxplot of male weight after dropping", ylab="weight", col=c("coral1"))

# Boxplot of female weight before dropping
box_wgt3 <- boxplot(wgt_f$weight, main="boxplot of female weight before dropping", ylab="weight", col=c("coral1"))

# drop abnormal values of weight
wgt_f <- wgt_f %>% mutate(weight = ifelse(weight < 25, NA, weight)) %>% filter(is.na(weight) == FALSE)
wgt_f <- wgt_f %>% mutate(weight = ifelse(weight > 80, NA, weight)) %>% filter(is.na(weight) == FALSE)

# Boxplot of female weight after dropping
box_wgt4 <- boxplot(wgt_f$weight, main="boxplot of female weight after dropping", ylab="weight", col=c("coral1"))

# bind height
data_hgt <- hgt08_18 %>% bind_rows(hgt11_18,hgt14_18)

#separate male and female
hgt_m <- subset(data_hgt,a1=="1",select=c(id, a1, height))
hgt_f <- subset(data_hgt,a1=="2",select=c(id, a1, height))

# Boxplot of male height before dropping 
box_hgt1 <- boxplot(hgt_m$height, main="boxplot of male height before dropping", ylab="height",col=c("coral1"))

# drop abnormal values of height
hgt_m <- hgt_m %>% mutate(height = ifelse(height < 135, NA, height)) %>% filter(is.na(height) == FALSE)
hgt_m <- hgt_m %>% mutate(height = ifelse(height > 186, NA, height)) %>% filter(is.na(height) == FALSE)

# Boxplot of male height after dropping 
box_hgt2 <- boxplot(hgt_m$height, main="boxplot of male height after dropping", ylab="height",col=c("coral1"))

# Boxplot of female height before dropping 
box_hgt3 <- boxplot(hgt_f$height, main="boxplot of female height before dropping", ylab="height",col=c("coral1"))

# drop abnormal values of height
hgt_f <- hgt_f %>% mutate(height = ifelse(height < 120, NA, height)) %>% filter(is.na(height) == FALSE)
hgt_f <- hgt_f %>% mutate(height = ifelse(height > 178, NA, height)) %>% filter(is.na(height) == FALSE)

# Boxplot of female height after dropping 
box_hgt4 <- boxplot(hgt_f$height, main="boxplot of female height after dropping", ylab="height",col=c("coral1"))

# calculate bmi
data_bmi<-left_join(data_hgt, data_wgt,by = "id") %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id,bmi)

# calculate bmi*hr
data_bmihr <-left_join(data_bmi,data_hr,by="id") %>%
    mutate(bmihr =(bmi*hr))

# calculate male bmi
bmi_m<-left_join(hgt_m, wgt_m,by = "id") %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id,bmi)

# calculate male bmi*hr
bmihr_m <-left_join(bmi_m,hr_m,by="id") %>%
    mutate(bmihr =(bmi*hr))

# calculate female bmi
bmi_f<-left_join(hgt_f, wgt_f,by = "id") %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id,bmi)

# calculate female bmi*hr
bmihr_f <-left_join(bmi_f,hr_f,by="id") %>%
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

## drop Obs with problematic male bmi
bmihr_m <- bmihr_m %>% mutate(bmihr = ifelse(bmi > 60, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
bmihr_m <- bmihr_m %>% mutate(bmihr = ifelse(bmi < 10, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
summary(bmihr_m$bmi)
## drop Obs with NA bmi*hr
data_m <- bmihr_m %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)

## drop Obs with problematic female bmi
bmihr_f <- bmihr_f %>% mutate(bmihr = ifelse(bmi > 60, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
bmihr_f <- bmihr_f %>% mutate(bmihr = ifelse(bmi < 10, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
summary(bmihr_f$bmi)
## drop Obs with NA bmi*hr
data_f <- bmihr_f %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)

# merge with survival time and status
data_surtime <- read_dta("E:/Jenny/Downloads/Recoded Data/idnum_survival18.dta")
data_surtime <- data_surtime[, c(1, 3:5)]
data <- data %>% left_join(data_surtime, by = "id")
## recode bmi*hr to quartile
summary(data_bmihr$bmihr)

data_bmihr <- data_bmihr %>% mutate(bmihr_quartile = case_when(bmihr <= 1294.2 ~ "Q1",
                                                               1294.2< bmihr & bmihr <= 1478.7 ~ "Q2",
                                                               1478.7 < bmihr & bmihr <= 1698.2 ~ "Q3",
                                                               bmihr > 1698.2 ~ "Q4")) %>%
    mutate(bmihr_quartile = factor(bmihr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))

#------------------------------------------------------------------------------#
################################### Fit K-M ####################################
#------------------------------------------------------------------------------#
##bmi*hr
fit <- survfit(Surv(survival_bas18,censor18) ~ bmihr_quartile, data = data)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/Figure 1. BMIHR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI * HR",
           #legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only bmi
summary(data$bmi)
data <- data %>% mutate(bmi_quartile = case_when(bmi <= 18 ~ "Q1",
                                                18 < bmi & bmi <= 20 ~ "Q2",
                                                20 < bmi & bmi <= 22 ~ "Q3",
                                                22 < bmi & bmi <= 24 ~ "Q4",
                                                bmi > 24 ~ "Q5")) %>%
    mutate(bmi_quartile = factor(bmi_quartile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
fit <- survfit(Surv(survival_bas18,censor18) ~ bmi_quartile, data = data)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/Figure 2. BMI K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI",
           legend.labs = c("Q1", "Q2", "Q3", "Q4", "Q5"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only heart rate
summary(data$hr)
data <- data %>% mutate(hr_quartile = case_when(hr <= 60 ~ "Q1",
                                                60 < hr & hr <= 70 ~ "Q2",
                                                70 < hr & hr <= 80 ~ "Q3",
                                                hr > 80 ~ "Q4")) %>%
    mutate(hr_quartile = factor(hr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_quartile, data = data)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/Figure 3. HR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "HR",
           legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# export dataset
saveRDS(data, "E:/Jenny/Downloads/Recoded Data/clhls_bmi_hr_pilot.rds")
write_dta(data, "E:/Jenny/Downloads/Recoded Data/clhls_bmi_hr_pilot.dta")

surv_m <- left_join(data_m, data_surtime,by = "id")
# only male bmi
summary(surv_m$bmi)
surv_m <- surv_m %>% mutate(bmi_quartile = case_when(bmi <= 18 ~ "Q1",
                                                 18 < bmi & bmi <= 20 ~ "Q2",
                                                 20 < bmi & bmi <= 22 ~ "Q3",
                                                 22 < bmi & bmi <= 24 ~ "Q4",
                                                 bmi > 24 ~ "Q5")) %>%
    mutate(bmi_quartile = factor(bmi_quartile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
fit <- survfit(Surv(survival_bas18,censor18) ~ bmi_quartile, data = surv_m)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/male BMI K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = surv_m,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI",
           legend.labs = c("Q1", "Q2", "Q3", "Q4", "Q5"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only male heart rate
summary(data_m$hr)
surv_m <- surv_m %>% mutate(hr_quartile = case_when(hr <= 60 ~ "Q1",
                                                60 < hr & hr <= 70 ~ "Q2",
                                                70 < hr & hr <= 80 ~ "Q3",
                                                hr > 80 ~ "Q4")) %>%
    mutate(hr_quartile = factor(hr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_quartile, data = surv_m)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/male HR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = surv_m,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "HR",
           legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()


surv_f <- left_join(data_f, data_surtime,by = "id")
# only female bmi
summary(surv_f$bmi)
surv_f <- surv_f %>% mutate(bmi_quartile = case_when(bmi <= 18 ~ "Q1",
                                                     18 < bmi & bmi <= 20 ~ "Q2",
                                                     20 < bmi & bmi <= 22 ~ "Q3",
                                                     22 < bmi & bmi <= 24 ~ "Q4",
                                                     bmi > 24 ~ "Q5")) %>%
    mutate(bmi_quartile = factor(bmi_quartile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")))
fit <- survfit(Surv(survival_bas18,censor18) ~ bmi_quartile, data = surv_f)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/female BMI K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = surv_f,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "BMI",
           legend.labs = c("Q1", "Q2", "Q3", "Q4", "Q5"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()

# only female heart rate
summary(data_f$hr)
surv_f <- surv_f %>% mutate(hr_quartile = case_when(hr <= 60 ~ "Q1",
                                                    60 < hr & hr <= 70 ~ "Q2",
                                                    70 < hr & hr <= 80 ~ "Q3",
                                                    hr > 80 ~ "Q4")) %>%
    mutate(hr_quartile = factor(hr_quartile, levels = c("Q1", "Q2", "Q3", "Q4")))
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_quartile, data = surv_f)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/female HR K-M survival curve.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = surv_f,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "HR",
           legend.labs = c("Q1", "Q2", "Q3", "Q4"), 
           risk.table = TRUE,
           fontsize = 6,
           risk.table.title = "Number at risk (Deaths)",
           font.main = c(15, "bold", "black"),
           font.x = c(15, "bold", "black"),
           font.y = c(15, "bold", "black"),
           font.tickslab = c(15, "bold", "black"),
           font.legend = c(15, "bold", "black"),
           tables.theme = theme_survminer(font.main = c(15, "bold", "black"),
                                          font.x = c(15, "bold", "black"),
                                          font.y = c(15, "bold", "black"),
                                          font.tickslab = c(15, "bold")))
dev.off()
