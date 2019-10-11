rm(list=ls())
library("tidyverse")
getwd()
tidyverse_update()
install.packages ("tidyr")

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

# For later plotting
install.packages("Hmisc")
library(Hmisc)

#Chapter 13, problem 20####

#A. We could use a Welch's T-test because as determined in the summary statistics, 
#the variances differ. We could also transform (because there are enough samples) 
#the data and proceed with a two-sample t-test (parametric test) if the ratio 
#of the sd become less than 3

salmoncolor<-read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

ggplot(salmoncolor) +
  geom_histogram(aes(skinColor), binwidth = .1)+
  facet_wrap(~species)

ggplot(salmoncolor) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(salmoncolor)+
  geom_qq(aes(sample = skinColor, color = species))

summ_skinColor<-salmoncolor%>%
  group_by(species)%>%
  summarise(mean_skinColor=mean(skinColor),
          median_skinColor=median(skinColor),
          IQR_skinColor=IQR(skinColor),
          sd_skinColor=sd(skinColor),
          var_skinColor=var(skinColor))

#data is relatively normally distributed, but does not meet assumption of 
# Two sample t-test that the variences must be equal, always try and transform before welch's test (if data values are all positive)

skinColor_ratio <-(max(summ_skinColor$sd_skinColor))/(min(summ_skinColor$sd_skinColor))
#ratio is greater than 3 for variance, so log transform data

salmoncolor<-mutate(salmoncolor, logskinColor = log(skinColor))
summ_logskinColor<-salmoncolor%>%
  group_by(species)%>%
  summarise(mean_logskinColor=mean(logskinColor),
            median_skinColor=median(skinColor),
            IQR_logskinColor=IQR(logskinColor),
            sd_logskinColor=sd(logskinColor),
            var_logskinColor=var(logskinColor))

logskinColor_ratio <-(max(summ_logskinColor$sd_logskinColor))/(min(summ_logskinColor$sd_logskinColor))
#ratio is less than 3, so it is acceptable to procede with t-test

t.test(skinColor ~ species, data = salmoncolor, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#B. There is a significant difference between the two means (p<0.05),it's way less actually
#This confirms the alternative hypothesis that there is a true difference in the mean of 
#the kokanee and sockeyeskincolor

#(two-sample, two-sided t-test, t=10.297, df=33, p<0.001)

#Chapter 13, problem 25####

biomass<-read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

ggplot(biomass) +
  geom_histogram(aes(biomassChange), binwidth = 1)

ggplot(biomass) +
  geom_boxplot(aes(y = biomassChange))

ggplot(biomass)+
  geom_qq(aes(sample = biomassChange))

summ_biomassChange<-biomass%>%
  summarise(mean_biomassChange=mean(biomassChange),
            median_biomassChange=median(biomassChange),
            IQR_biomassChange=IQR(biomassChange),
            sd_biomassChange=sd(biomassChange),
            var_biomassChange=var(biomassChange))
# data shows non-normal trend (best seen in curve of qqplot, trying a sign test)

SignTest(biomass$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
#sign test, tests the null hypothesis that the median=0 because the values are either less than (negative) or greater than (positive)
#(two-sided, sign test, S=21, number of differences= 36, p= 0.405)
#the high p-value shows that there is no significant change in biomass of rainforest areas following clear-cutting

#also tried a parametric test (t-test because data wasn't too far off of normal, but cannot transform because of negative values in data)
t.test(biomass$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)
# we are only given data for the difference in biomass, so performed a one-sample/two-sided t-test
#paired t-testsince we are only given the differences the one-sample t-test was the equivalent way of doing a paired t-test

#(two-sided, paired t-test, t=-0.85279, df=35, p= 0.3996)



#Chapter 13, problem 26####
zebrafinch<-read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

ggplot(zebrafinch) +
  geom_histogram(aes(preference), binwidth = 5)

ggplot(zebrafinch) +
  geom_boxplot(aes(y = preference))

ggplot(zebrafinch)+
  geom_qq(aes(sample = preference))

summ_preference<-zebrafinch%>%
  summarise(mean_preference=mean(preference),
            median_preference=median(preference),
            IQR_preference=IQR(preference),
            sd_bpreference=sd(preference),
            var_preference=var(preference))
#the data is relatively normally dist (as seen best in boxplot/qqplot)
#because the obs are of independent female finch and the null would be mean preference = 0
#the one-sample/ two-sided t-tests

t.test(zebrafinch$preference, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#females preferred the males with the extra dietary careteoids, because p<0.05 (actual value:0.0003259)

# (two-sided,one-sample t-test,t= 5.6198, df=9, p= 0.0003259)

#Review Problem 2, 16####
fishdata<-read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")

ggplot(fishdata) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 20)+
  facet_wrap(~genotype)

ggplot(fishdata) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))

ggplot(fishdata)+
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))

summ_secondsAggressiveActivity<-fishdata%>%
  group_by(genotype)%>%
  summarise(mean_secondsAggressiveActivity=mean(secondsAggressiveActivity),
            median_secondsAggressiveActivity=median(secondsAggressiveActivity),
            IQR_secondsAggressiveActivity=IQR(secondsAggressiveActivity),
            sd_secondsAggressiveActivity=sd(secondsAggressiveActivity),
            var_secondsAggressiveActivity=var(secondsAggressiveActivity))

secondsAggressiveActivity_ratio <-(max(summ_secondsAggressiveActivity$sd_secondsAggressiveActivity))/(min(summ_secondsAggressiveActivity$sd_secondsAggressiveActivity))

#A.estimate the magnitude of the effect of the mutation (difference between means)
# the confidence interval calcualted was 25.93287 is less than or equal than and 110.26713 is greater than or equal to the mean
#this means that we are not very confident in the magnitude of the effect of the mutation (it can be anywhere between these two values), but we do at leaset know that it is a positive value

#B. the p value = 0.003142, meanining that we are pretty confident that the effect is not zero (there is a difference between the mutant zebrafish and the wildtype)
#performing a two-sample, two-sided t-test because data is relatively normally dist, sd ratio is less than 3, and the two sample groups were independent

t.test(secondsAggressiveActivity~ genotype, data = fishdata, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#(two-sided, two-sample t-test, t=3.3802, df=19, p=0.003142)
