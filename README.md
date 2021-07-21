# CLHLS-SW
#install packages
install.packages("data.table")
data.table::update.dev.pkg()
install.packages("sjlabelled")

#retrieve from local library
library(tidyverse)
library(data.table)
library(haven)
library(sjlabelled)
library(survival)
library(survminer)
library(ggplot2)

#require
require(tidyverse)
require(data.table)
require(haven)
require(sjlabelled)
require(survival)
require(survminer)
require(ggplot2)



#Read data from STATA file
library(haven)
X2_00_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat00_18.dta")
View(X2_00_18wave)
X2_02_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat02_18.dta")
View(X2_02_18wave)
X2_05_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat05_18.dta")
View(X2_05_18wave)
X2_08_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat08_18.dta")
View(X2_08_18wave)
X2_11_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat11_18.dta")
View(X2_11_18wave)
X2_14_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat14_18.dta")
View(X2_14_18wave)
X2_98_18wave <- read_stata("C:/Users/admin/OneDrive/Desktop/Recoded Data/dat98_18.dta")
View(X2_98_18wave)

