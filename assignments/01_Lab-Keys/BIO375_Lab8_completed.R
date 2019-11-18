#### Lab 8: 1-way ANOVA, continued #### 
# For this lab you will use the datasets described in Chapter 15 of your book but you will 
# answer the slightly modified questions that I provide below

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Problem 15-22 ####
# Complete parts a, b, c, d

sticks <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                   col_types = cols(specimen = col_factor()))


# check for normality using plots
ggplot(sticks) +
  geom_histogram(aes(headwidth), binwidth = .011)
ggplot(sticks)+
  geom_qq(aes(sample = headwidth, color = ""))
ggplot(sticks) +
  geom_boxplot(aes(x = "", y = headwidth))

#random effects ANOVA
model01 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = sticks)

model01_varcomp <- VarCorr(model01)

model01_varcomp
#between group variance 0.0002459167
#within group variance(MSerror): 0.0001660000

#variation due to specimen
varAmong  <- as.numeric( model01_varcomp[1,1] )

#measurement error
varWithin <- as.numeric( model01_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
#repeatability= 0.5970059


#### Problem 15-23 ####
# Complete parts a and c only
#a: You need to run a planned multiple comparison test
#c: Skipped

pine <-read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

ggplot(pine) +
  geom_histogram(aes(conemass), binwidth = 1)+
  facet_wrap(~habitat)
ggplot(pine)+
  geom_qq(aes(sample = conemass, color = habitat))
ggplot(pine) +
  geom_boxplot(aes(x = habitat, y = conemass))

#multiple planned comparisons
model02 <- lm(conemass~habitat, data = pine)

planned <- glht(model02, linfct = 
                  mcp(habitat = c("island.absent - island.present = 0")))
confint(planned)
summary(planned)

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

malaria <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
#Step 1
#normal one way fixed effects ANOVA
#checks for normality
ggplot(malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)
ggplot(malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))
ggplot(malaria) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))


#Step 2

model03 <- lm(logSporozoiteNumbers~treatmentGroup, data = malaria)

#Step 3
summ_sporo <- malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_SporozoiteNum = mean(logSporozoiteNumbers),
            sd_SporozoiteNum = sd(logSporozoiteNumbers),
            n_SporozoiteNum = n())
ratio_sporo <-(max(summ_sporo$sd_SporozoiteNum))/(min(summ_sporo$sd_SporozoiteNum))

autoplot(model03)

#step 4
anova(model03)

#Tukey

tukey <- glht(model03, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)


#### Problem 15-30 and/or 15-31 (same data in both problems) ####

# Use the data to perform the correct test.  
# Please show code for all steps in your process.
crab <-read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))
summary(crab)
crab <- crab %>%
  slice(-85)

#Step 1
#normal one way fixed effects ANOVA
#checks for normality
ggplot(crab) +
  geom_histogram(aes(bodyTemperature), binwidth = .1)+
  facet_wrap(~crabType)
ggplot(crab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))
ggplot(crab) +
  geom_boxplot(aes(x = crabType, y = bodyTemperature))

#Step 2
model04 <- lm(bodyTemperature~crabType, data = crab)

#Step 3
summ_temp <- crab %>%
  group_by(crabType) %>% 
  summarise(mean_temp = mean(bodyTemperature),
            sd_temp = sd(bodyTemperature),
            n_crab = n())
ratio_crab <-(max(summ_temp$sd_temp))/(min(summ_temp$sd_temp))

autoplot(model04)

#Step 4
anova(model04)






