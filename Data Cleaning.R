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
hr11_18 <- dat11_18 %>% mutate(hr = g7) %>% select(id, hr)
hr14_18 <- dat14_18 %>% mutate(hr = g7) %>% select(id, hr)
hr08_18 <- dat08_18 %>% mutate(hr = (g71 + g72)/2) %>% select(id, hr)
# ------------------------------------------------------------------------------------- #
###################################### Clean Weight #################################
# --------------------------------------------------------------------------------------#
wgt08_18 <- dat08_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, weight)
wgt11_18 <- dat11_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, weight)
wgt14_18 <- dat14_18 %>%
    mutate(weight = g101) %>%
    mutate(weight = ifelse(weight == 888 | weight == 999, NA, weight)) %>%
    select(id, weight)
# ------------------------------------------------------------------------------------- #
###################################### Clean Height #################################
# --------------------------------------------------------------------------------------#
hgt08_18 <- dat08_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, height)
hgt11_18 <- dat11_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, height)
hgt14_18 <- dat14_18 %>%
    mutate(height = g1021) %>%
    mutate(height = ifelse(height == 888 | height == 999, NA, height)) %>%
    mutate_at(vars(height), funs(setattr(., "label", "directly measured height of the interviewee"))) %>%
    select(id, height)
# ------------------------------------------------------------------------------------- #
###################################### Bind dataset & Drop  #############################
# --------------------------------------------------------------------------------------#
# bind hr
data_hr <- hr08_18 %>% bind_rows(hr11_18,hr14_18)
# change hr=999 -> NA
data_hr <- data_hr %>% mutate(hr = ifelse(hr == 888 | hr == 999, NA, hr))
# Boxplot of heartrate before dropping
box_hr1 <- boxplot(data_hr$hr, main="boxplot of heartrate before dropping", ylab="heartrate",col=c("coral1"))

## drop Obs with problematic heart rate
data_hr <- data_hr %>% mutate(hr = ifelse(hr >= 200, NA, hr)) %>% filter(is.na(hr) == FALSE)

# Boxplot of heartrate after dropping
box_hr2 <- boxplot(data_hr$hr, main="boxplot of heartrate after dropping", ylab="heartrate",col=c("coral1"))

# bind weight
data_wgt <- wgt08_18 %>% bind_rows(wgt11_18,wgt14_18)
 
# Boxplot of weight before dropping
box_wgt1 <- boxplot(data_wgt$weight, main="boxplot of weight before dropping", ylab="weight", col=c("coral1"))

# drop abnormal values of weight
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight < 30, NA, weight)) %>% filter(is.na(weight) == FALSE)
data_wgt <- data_wgt %>% mutate(weight = ifelse(weight > 120, NA, weight)) %>% filter(is.na(weight) == FALSE)
summary(data_wgt$weight)
hist(data_wgt$weight)

# Boxplot of weight after dropping
box_wgt2 <- boxplot(data_wgt$weight, main="boxplot of weight after dropping", ylab="weight", col=c("coral1"))

# bind height
data_hgt <- hgt08_18 %>% bind_rows(hgt11_18,hgt14_18)

# Boxplot of height before dropping 
box_hgt1 <- boxplot(data_hgt$height, main="boxplot of height before dropping", ylab="height",col=c("coral1"))

# drop abnormal values of height
data_hgt <- data_hgt %>% mutate(height = ifelse(height < 100, NA, height)) %>% filter(is.na(height) == FALSE)

# Boxplot of height after dropping 
box_hgt2 <- boxplot(data_hgt$height, main="boxplot of height after dropping", ylab="height",col=c("coral1"))

# calculate bmi
data_bmi<-left_join(data_hgt, data_wgt,by = "id") %>%
    mutate(bmi =((weight/(height/100)^2)))%>%
    select(id,bmi)

# calculate bmi*hr
data_bmihr <-left_join(data_bmi,data_hr,by="id") %>%
    mutate(bmihr =(bmi*hr))

# ------------------------------------------------------------------------------------- #
############################# Clean Abnormal Obs ########################################
# --------------------------------------------------------------------------------------#
## drop Obs with problematic heart rate
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(hr >= 200, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)

## drop Obs with problematic bmi
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi > 60, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
data_bmihr <- data_bmihr %>% mutate(bmihr = ifelse(bmi < 10, NA, bmihr)) %>% filter(is.na(bmihr) == FALSE)
summary(data_bmihr$bmi)
## drop Obs with NA bmi*hr
data <- data_bmihr %>% mutate(bmihr = bmi * hr) %>% filter(is.na(bmihr) == FALSE)


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

install.packages("DiagrammeR")
create_graph()
library(DiagrammeR)
graph <- create_graph()
class(graph)
get_node_df(graph)
get_edge_df(graph)
is_graph_directed(graph)

###
# Create a graph with nodes but no edges
###

install.packages("DiagrammeR")
library(DiagrammeR)
install.packages("create_nodes")

# Create an NDF
nodes <-
    create_nodes(
        nodes = 1:4,
        label = FALSE,
        type = "lower",
        style = "filled",
        color = "aqua",
        shape = c("circle", "circle",
                  "rectangle", "rectangle"),
        data = c(3.5, 2.6, 9.4, 2.7))

# Examine the NDF
nodes
#>   nodes  type label  style color     shape data
#> 1     1 lower       filled  aqua    circle  3.5
#> 2     2 lower       filled  aqua    circle  2.6
#> 3     3 lower       filled  aqua rectangle  9.4
#> 4     4 lower       filled  aqua rectangle  2.7

# Create the graph and include the
# `nodes` NDF
graph <- create_graph(nodes_df = nodes)

# Examine the NDF within the graph object
get_node_df(graph)
#>   nodes  type label  style color     shape data
#> 1     1 lower       filled  aqua    circle  3.5
#> 2     2 lower       filled  aqua    circle  2.6
#> 3     3 lower       filled  aqua rectangle  9.4
#> 4     4 lower       filled  aqua rectangle  2.7

# It's the same NDF (outside and inside the graph)
all(nodes == graph$nodes_df)
#> [1] TRUE




