###This file sets up all the quantities to be calculated for the asymptotic normality test###
###and the numerical approximation/parametric bootstrap######################################
rm(list=ls())
library(striprtf)
library(tikzDevice)
library(xtable)
library(tidyverse)
library(ggplot2)
library(texreg)
library(here)
source(here("codes","functions.R"))
setwd(here("Results"))
###########################################################
#######------------------DATA--------------------##########
#######The data needs to be supplied in table form#########
#######
data<-data.frame(data) ###read in your data##### 
######################The way the codes are written below, the data needs to be supplied in the following
####form: One row corresponds to one department############
####The columns correspond to: 
##"FE": number of female department members
## "M": number of male department members
## "Size": Total department size (FE+M)
## "Share": Total female share (FE/Size) on the department level
## "Code": discipline code/identifier 
names(data)<-c("FE","M","Size","Share","Code")
means<-data %>% 
  group_by(Code)%>% 
  dplyr::summarize(average = mean(Share))

##############Generate the bootstrapped observations B*b(z) for a vector $z=0,...,1$ and b=1,..,reps
z=0:10
for(i in 1:length(f_z))
{
  boot_fun(z[i],data,means,seed=10,reps=100)
  ####This function stores .csv files with the path uc"z".csv where one row correspond to B*b(z)
}

###runs the joint test for all z and saves the results in "table_tests_all_new"
f<-0:10
for(i in 1:length(f))
{
  esti_fisher(f[i],means)
}


######Calculate the leave one out shares of women (where own department is not considered in the analysis)

alt_means<-data  %>% group_by(Code) %>%
  mutate(special_mean = (sum(Share) - Share)/(n()-1))

code=unique(alt_means$Code)
mefe<-c()
dev<-c()




