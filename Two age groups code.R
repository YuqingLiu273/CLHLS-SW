#retrieve from local library
library(tidyverse)
library(data.table)
library(haven)
library(sjlabelled)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(DiagrammeR)
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
###################################### Bind dataset & Drop  #############################
# --------------------------------------------------------------------------------------#
# bind hr
data_hr <- hr08_18 %>% bind_rows(hr11_18,hr14_18)
# change hr=999 -> NA
data_hr <- data_hr %>% mutate(hr = ifelse(hr == 888 | hr == 999, NA, hr))
## drop Obs with problematic heart rate
data_hr <- data_hr %>% mutate(hr = ifelse(hr >= 200, NA, hr)) %>% filter(is.na(hr) == FALSE)
data_hr <- data_hr %>% mutate(hr = ifelse(hr <= 30, NA, hr)) %>% filter(is.na(hr) == FALSE)

# bind weight
data_wgt <- wgt08_18 %>% bind_rows(wgt11_18,wgt14_18)
# drop abnormal values of weight
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight < 30, NA, weight)) %>% filter(is.na(weight) == FALSE)
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight > 120, NA, weight)) %>% filter(is.na(weight) == FALSE)

# bind height
data_hgt <- hgt08_18 %>% bind_rows(hgt11_18,hgt14_18)
# drop abnormal values of height
data_hgt <- data_hgt %>% mutate(height = ifelse(height < 100, NA, height)) %>% filter(is.na(height) == FALSE)


# ------------------------------------------------------------------------------------- #
################################## Calculate BMI & BMI*HR  ##############################
# --------------------------------------------------------------------------------------#
# calculate bmi
data_bmi<-left_join(data_hgt, data_wgt, by = c("id"="id", "trueage"="trueage")) %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id, trueage, bmi)

# calculate bmi*hr
data_bmihr <-left_join(data_bmi,data_hr,by = c("id"="id", "trueage"="trueage")) %>%
    mutate(bmihr =(bmi*hr))

# ------------------------------------------------------------------------------------- #
######################### Clean Abnormal Obs for BMI*HR #################################
# --------------------------------------------------------------------------------------#
## drop Obs with problematic bmi
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi > 60, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi < 10, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
summary(data_bmihr$bmi)
## drop Obs with NA bmi*hr
data <- data_bmihr %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)

# ------------------------------------------------------------------------------------- #
############################### Divide two age groups  ##################################
# --------------------------------------------------------------------------------------#
#separate age
data1 <- data %>% filter(data$trueage <= 79) %>% filter(is.na(trueage) == FALSE)
data2 <- data %>% filter(data$trueage > 79) %>% filter(is.na(trueage) == FALSE)

# ------------------------------------------------------------------------------------- #
############################ Recode bmi to one-unit groups  #############################
# --------------------------------------------------------------------------------------#
#for younger old
data1_bmi_01 <- data1 %>% filter(data1$bmi < 16) %>% filter(is.na(bmi) == FALSE)
data1_bmi_02 <- data1 %>% filter(data1$bmi >= 16 & data1$bmi < 17) %>% filter(is.na(bmi) == FALSE)
data1_bmi_03 <- data1 %>% filter(data1$bmi >= 17 & data1$bmi < 18) %>% filter(is.na(bmi) == FALSE)
data1_bmi_04 <- data1 %>% filter(data1$bmi >= 18 & data1$bmi < 19) %>% filter(is.na(bmi) == FALSE)
data1_bmi_05 <- data1 %>% filter(data1$bmi >= 19 & data1$bmi < 20) %>% filter(is.na(bmi) == FALSE)
data1_bmi_06 <- data1 %>% filter(data1$bmi >= 20 & data1$bmi < 21) %>% filter(is.na(bmi) == FALSE)
data1_bmi_07 <- data1 %>% filter(data1$bmi >= 21 & data1$bmi < 22) %>% filter(is.na(bmi) == FALSE)
data1_bmi_08 <- data1 %>% filter(data1$bmi >= 22 & data1$bmi < 23) %>% filter(is.na(bmi) == FALSE)
data1_bmi_09 <- data1 %>% filter(data1$bmi >= 23 & data1$bmi < 24) %>% filter(is.na(bmi) == FALSE)
data1_bmi_10 <- data1 %>% filter(data1$bmi >= 24 & data1$bmi < 25) %>% filter(is.na(bmi) == FALSE)
data1_bmi_11 <- data1 %>% filter(data1$bmi >= 25 & data1$bmi < 26) %>% filter(is.na(bmi) == FALSE)
data1_bmi_12 <- data1 %>% filter(data1$bmi >= 26) %>% filter(is.na(bmi) == FALSE)

#for older old
data2_bmi_01 <- data2 %>% filter(data2$bmi < 16) %>% filter(is.na(bmi) == FALSE)
data2_bmi_02 <- data2 %>% filter(data2$bmi >= 16 & data2$bmi < 17) %>% filter(is.na(bmi) == FALSE)
data2_bmi_03 <- data2 %>% filter(data2$bmi >= 17 & data2$bmi < 18) %>% filter(is.na(bmi) == FALSE)
data2_bmi_04 <- data2 %>% filter(data2$bmi >= 18 & data2$bmi < 19) %>% filter(is.na(bmi) == FALSE)
data2_bmi_05 <- data2 %>% filter(data2$bmi >= 19 & data2$bmi < 20) %>% filter(is.na(bmi) == FALSE)
data2_bmi_06 <- data2 %>% filter(data2$bmi >= 20 & data2$bmi < 21) %>% filter(is.na(bmi) == FALSE)
data2_bmi_07 <- data2 %>% filter(data2$bmi >= 21 & data2$bmi < 22) %>% filter(is.na(bmi) == FALSE)
data2_bmi_08 <- data2 %>% filter(data2$bmi >= 22 & data2$bmi < 23) %>% filter(is.na(bmi) == FALSE)
data2_bmi_09 <- data2 %>% filter(data2$bmi >= 23 & data2$bmi < 24) %>% filter(is.na(bmi) == FALSE)
data2_bmi_10 <- data2 %>% filter(data2$bmi >= 24 & data2$bmi < 25) %>% filter(is.na(bmi) == FALSE)
data2_bmi_11 <- data2 %>% filter(data2$bmi >= 25 & data2$bmi < 26) %>% filter(is.na(bmi) == FALSE)
data2_bmi_12 <- data2 %>% filter(data2$bmi >= 26) %>% filter(is.na(bmi) == FALSE)


# merge with survival time and status
data_surtime <- read_dta("idnum_survival18.dta")
data_surtime <- data_surtime[, c(1, 3:5)]
data1 <- data1 %>% left_join(data_surtime, by = "id")
data2 <- data2 %>% left_join(data_surtime, by = "id")

#---------------------------------------------------------------------------------------#
################################# recode hr to subgroups#################################
#---------------------------------------------------------------------------------------#
#for younger old
data1_hr_01 <- data1%>% filter(data1$hr< 60)%>% filter(is.na(hr)==FALSE)
data1_hr_02 <- data1%>% filter(data1$hr>=60 & data1$hr< 65)%>% filter(is.na(hr)==FALSE)
data1_hr_03 <- data1%>% filter(data1$hr>=65 & data1$hr< 70)%>% filter(is.na(hr)==FALSE)
data1_hr_04 <- data1%>% filter(data1$hr>=70 & data1$hr< 75)%>% filter(is.na(hr)==FALSE)
data1_hr_05 <- data1%>% filter(data1$hr>=75 & data1$hr< 80)%>% filter(is.na(hr)==FALSE)
data1_hr_06 <- data1%>% filter(data1$hr>=80)%>% filter(is.na(hr)==FALSE)

#for older old
data2_hr_01 <- data2%>% filter(data2$hr< 60)%>% filter(is.na(hr)==FALSE)
data2_hr_02 <- data2%>% filter(data2$hr>=60 & data2$hr< 65)%>% filter(is.na(hr)==FALSE)
data2_hr_03 <- data2%>% filter(data2$hr>=65 & data2$hr< 70)%>% filter(is.na(hr)==FALSE)
data2_hr_04 <- data2%>% filter(data2$hr>=70 & data2$hr< 75)%>% filter(is.na(hr)==FALSE)
data2_hr_05 <- data2%>% filter(data2$hr>=75 & data2$hr< 80)%>% filter(is.na(hr)==FALSE)
data2_hr_06 <- data2%>% filter(data2$hr>=80)%>% filter(is.na(hr)==FALSE)

# ------------------------------------------------------------------------------------- #
###################################### Run COX Regression Model #################################
# --------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
################################### Set bmi levels ####################################
#------------------------------------------------------------------------------#

data1 <- data1 %>% mutate(bmi_levels = case_when(bmi < 16 ~ "G1",
                                                 16 <= bmi & bmi < 17 ~ "G2",
                                                 17 <= bmi & bmi < 18 ~ "G3",
                                                 18 <= bmi & bmi < 19 ~ "G4",
                                                 19 <= bmi & bmi < 20 ~ "G5",
                                                 20 <= bmi & bmi < 21 ~ "G6",
                                                 21 <= bmi & bmi < 22 ~ "G7",
                                                 22 <= bmi & bmi < 23 ~ "G8",
                                                 23 <= bmi & bmi < 24 ~ "G9",
                                                 24 <= bmi & bmi < 25 ~ "G10",
                                                 25 <= bmi & bmi < 26 ~ "G11",
                                                 bmi >= 26 ~ "G12")) %>%
    mutate(bmi_levels = factor(bmi_levels, levels = c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12")))

data2 <- data2 %>% mutate(bmi_levels = case_when(bmi < 16 ~ "G1",
                                                 16 <= bmi & bmi < 17 ~ "G2",
                                                 17 <= bmi & bmi < 18 ~ "G3",
                                                 18 <= bmi & bmi < 19 ~ "G4",
                                                 19 <= bmi & bmi < 20 ~ "G5",
                                                 20 <= bmi & bmi < 21 ~ "G6",
                                                 21 <= bmi & bmi < 22 ~ "G7",
                                                 22 <= bmi & bmi < 23 ~ "G8",
                                                 23 <= bmi & bmi < 24 ~ "G9",
                                                 24 <= bmi & bmi < 25 ~ "G10",
                                                 25 <= bmi & bmi < 26 ~ "G11",
                                                 bmi >= 26 ~ "G12")) %>%
    mutate(bmi_levels = factor(bmi_levels, levels = c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12")))


# Fit Cox
data1$bmi_levels <- factor(data1$bmi_levels, levels = c("G6","G1","G2","G3","G4","G5","G7","G8","G9","G10","G11","G12"))
# or using Gregor's comment:
data1$bmi_levels = relevel(data1$bmi_levels, ref = "G6")
cox.mod_data1_bmi<-coxph(Surv(data1$trueage, data1$censor18)~bmi_levels , data  = data1)
summary(cox.mod_data1_bmi)

# Fit Cox
data2$bmi_levels <- factor(data2$bmi_levels, levels = c("G6","G1","G2","G3","G4","G5","G7","G8","G9","G10","G11","G12"))
# or using Gregor's comment:
data2$bmi_levels = relevel(data2$bmi_levels, ref = "G6")
cox.mod_data2_bmi<-coxph(Surv(data2$trueage, data2$censor18)~bmi_levels , data  = data2)
summary(cox.mod_data2_bmi)

#ggforest figure
ggforest(cox.mod_data1_bmi,  #coxph得到的Cox回归结果
         data = data1,  #数据集
         main = 'Hazard ratio of data1',  #标题
         cpositions = c(0.05, 0.15, 0.35),  #前三列距离
         fontsize = 1, #字体大小
         refLabel = 'reference', #相对变量的数值标签，也可改为1
         noDigits = 3 #保留HR值以及95%CI的小数位数
        )

ggforest(cox.mod_data2_bmi,  #coxph得到的Cox回归结果
         data = data2,  #数据集
         main = 'Hazard ratio of data2',  #标题
         cpositions = c(0.05, 0.15, 0.35),  #前三列距离
         fontsize = 1, #字体大小
         refLabel = 'reference', #相对变量的数值标签，也可改为1
         noDigits = 3 #保留HR值以及95%CI的小数位数
        )
#------------------------------------------------------------------------------#
################################### Set hr levels ####################################
#------------------------------------------------------------------------------#
data1 <- data1 %>% mutate(hr_levels = case_when(hr < 60 ~ "G1",
                                                60 <= hr & hr < 65 ~ "G2",
                                                65 <= hr & hr < 70 ~ "G3",
                                                70 <= hr & hr < 75 ~ "G4",
                                                75 <= hr & hr < 80 ~ "G5",
                                                hr >= 80 ~ "G6")) %>%
    mutate(hr_levels = factor(hr_levels, levels = c("G1", "G2", "G3", "G4", "G5", "G6")))

data2 <- data2 %>% mutate(hr_levels = case_when(hr < 60 ~ "G1",
                                                60 <= hr & hr < 65 ~ "G2",
                                                65 <= hr & hr < 70 ~ "G3",
                                                70 <= hr & hr < 75 ~ "G4",
                                                75 <= hr & hr < 80 ~ "G5",
                                                hr >= 80 ~ "G6")) %>%
    mutate(hr_levels = factor(hr_levels, levels = c("G1", "G2", "G3", "G4", "G5", "G6")))
#------------------------------------------------------------------------------#
################################### Fit K-M ####################################
#------------------------------------------------------------------------------#
##hr survplot
#younger old
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_levels, data = data1)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/Figure 1. HR K-M survival curve for younger olds.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data1,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           ylim = c(0.65,1),
           legend.title = "Heart rate",
           legend.labs = c("G1", "G2", "G3", "G4", "G5", "G6"), 
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

#older old
fit <- survfit(Surv(survival_bas18,censor18) ~ hr_levels, data = data2)
print(summary(fit)$table)
tiff("E:/Jenny/Downloads/Recoded Data/Figure 1. HR K-M survival curve for elder olds.tiff", 
     width = 8000, height = 7230, pointsize = 12, res = 600)
ggsurvplot(fit, 
           data = data2,
           palette = "jco",
           linetype = 1,
           censor.size = 0.5, # change the width of the curve by this command
           xlab = "Duration of follow-up (years)",
           ylab = "Probability of survival",
           break.x.by = 2,
           xlim = c(0,14),
           legend.title = "Heart rate",
           legend.labs = c("G1", "G2", "G3", "G4", "G5", "G6"), 
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


