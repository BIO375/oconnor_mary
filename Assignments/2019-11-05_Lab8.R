#### Lab 8 ####

rm(list = ls())
getwd()

install.packages("hms")

library("ggfortify")
library("multcomp")
library("nlme")

library("tidyverse")
tidyverse_update()

#### Problem 15-22 ####
# parts A,B, C, D

sticks<- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                  col_types = cols(specimen = col_factor(levels = c("1", 
                                                                    "2", "3", "4", "5", "6", "7", "8", 
                                                                    "9", "10", "11", "12", "13", "14", 
                                                                    "15", "16", "17", "18", "19", "20", 
                                                                    "21", "22", "23", "24", "25"))))
model22 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = sticks)

model22_varcomp <- VarCorr(model22)
model22_varcomp

# A. variance within groups (headwith): 0.0001660000

# B. variance among groups (specimen): 0.0002459167

varAmong  <- as.numeric( model22_varcomp[1,1] )
varWithin <- as.numeric( model22_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

#C. The repeatability of the headwidth measurements = 0.5970059 = 59.7%

#D. Femur length repeatability = 0.75 (75%), head width repeatability = 0.597 (59.7%)
# Femur length has higher repeatability, which means that head width is more affected 
# by measurement error.


#### Problem 15-23 ####
# part A  only

PineCones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv",col_types = cols(
  habitat = col_factor() ))

#A. Because they identified the main comparison of interest to be the comparison between 
# islands with and without squirrels before the experiment, this type of comparison is a 
# planned comparison between 2 means.

model23 <- lm(conemass~habitat, data = PineCones)

# Performing a comparison between 2 means
planned <- glht(model23, linfct = 
                  mcp(habitat = c("island.present-island.absent = 0")))
confint(planned)
summary(planned)

# (Planned comparison between 2 means, df=14, t=-8.596, p<0.0001)
# This means that there is a significant difference between the mean cone size of the Island with 
# squirrels absent and the island with squirrels present.

#### Problem 15-26 ####
# use data to perform the correct test

#read in data making chartacters = factors, then plot data
malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", 
                    col_types = cols(treatmentGroup = col_factor(levels = c("Control", 
                                                                            "WT", "Scorpine"))))

ggplot(malaria) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))
ggplot(malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)
ggplot(malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

# The boxplots, histograms, and qq-plots show relatively normal distribution for all of the 
# treatment groups.

#contstuct anova
model26 <- lm(logSporozoiteNumbers~treatmentGroup, data = malaria)

#recheck assumptions
summ_logSporozoiteNumbers <- malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

# The ratio is less than 3 indicating to proceed with parametric test because of homogeneity of
# variance and normality from the previous plots.

autoplot(model26)

#interpret results

anova(model26)

# (1-Way Fixed ANOVA, df,groups=2, df,error=40, F= 21.361, p<0.0001)
# This indicates that there are differences in the number of sporozoites per group, but it
# is not specific as to which groups differed.

#summary of model results
summary(model26)

# unplanned comparisons using Tukey's HSD
tukey <- glht(model26, linfct = mcp(treatmentGroup = "Tukey"))

summary(tukey)

#Tukey's HSD showed that the scorpine and control and the scorpine and wildtype differed
# in their number of sporozoites (both, *** 0.001). I used this because the data met all 
# of the assumptions: normality, equal variance, random variables, and being unplanned.

#### Problem 15-30 ####
# use data to perform the correct test

#read in data making chartacters = factors, then plot data
crab <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", 
                 col_types = cols(crabType = col_factor(levels = c("female", 
                                                                   "intact male", "male minor removed", 
                                                                   "male major removed"))))
crab<- slice(crab, -85)

ggplot(crab) +
  geom_boxplot(aes(x = crabType, y = bodyTemperature))
ggplot(crab) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.25)+
  facet_wrap(~crabType)
ggplot(crab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

# The boxplots do show a group with a slight left skew and the qq-plot is not quite best fit with
# a linear line. However, the data is close enough to normally distributed that I am going to 
# treat it as meeting the assumption of normality.

model30 <- lm(bodyTemperature~crabType, data = crab)

summ_bodyTemperature <- crab %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
ratio <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))

# The ratio less than 3 and the data was at least relatively normal,
# so I am proceeding with parametric test because it meets the assumptions of normality, 
# homogeneity of variance, and random sampling.

autoplot(model30)

anova(model30)

#(1-Way Fixed ANOVA, df,groups=3, df,error=80, F= 20.312, p<0.0001)
# This indicates that there is differences in the rates of heat gain per crab type, but it
# is not specific as to which types differed.

summary(model30)

tukey <- glht(model30, linfct = mcp(crabType = "Tukey"))

summary(tukey)
# Tukey's HSD showed significant differences between the following groups: 
#intact male - female(*** 0.001),
# male minor removed - female (*** 0.001), 
# male major removed- female(*** 0.001), and 
# male major removed - male minor removed(* 0.05)

#### 26/26 code runs without breaking ####
