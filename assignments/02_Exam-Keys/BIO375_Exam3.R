#### For Exam 3 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()


#### Problem 1 ####
malaria <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
ggplot(data = malaria) +
  geom_point(mapping = aes(x = treatmentGroup, y = logSporozoiteNumbers),
            pch = 1, size = 2)+
  theme_classic()+
  labs( x = "Treatment Group", y = "ln(Sporozoite Number)")

model01 <- lm(logSporozoiteNumbers~treatmentGroup, data = malaria)

summ_sporo <- malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_SporozoiteNum = mean(logSporozoiteNumbers),
            sd_SporozoiteNum = sd(logSporozoiteNumbers),
            n_SporozoiteNum = n())
malaria <- malaria %>%
  mutate(spor_resid = resid(model01))


### Problem 9 ####
bacteria <- read_csv("datasets/exams/bacteria.csv")
model02 <- lm(DENSITY~TREATMNT, data = bacteria)
summary(model02)

### Problem 10 ####
aphids <- read_csv("datasets/exams/aphids.csv")
#random effects ANOVA
model03 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)

model03_varcomp <- VarCorr(model03)

model03_varcomp
#between group variance 0.4048
#within group variance(MSerror): 0.0838

#variation due to gall number
varAmong  <- as.numeric( model03_varcomp[1,1] )

#measurement error
varWithin <- as.numeric( model03_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
#repeatability= 0.8285

### Problem 11 ####
glucose <- read_csv("datasets/exams/glucose.csv")
ggplot(data = glucose, aes(x = blood_glucose, y = HbA1c))+
  geom_point()
ggplot(data = glucose)+
  geom_boxplot(aes("", blood_glucose))
ggplot(data = glucose)+
  geom_boxplot(aes("", HbA1c))
# Blood glucose boxplot is very symmetrical, HbA1c has one outlier, 
# but the whiskers are relatively equal and the median more or less
# centered in the box.  Assumtion of bivariate normality met.

glucCor <- cor.test(~ blood_glucose + HbA1c, data = glucose,
                     method = "pearson")
glucCor


r <- glucCor$estimate
SE <-sqrt((1-r^2)/(6))
t <- r/SE

### Problem 12 ####
drivers <- read_csv("datasets/exams/DriverVision.csv")
ggplot(data = drivers) +
  geom_point(mapping = aes(x = Age, y = Distance ))
model04<-lm(Distance ~ Age, data = drivers)
autoplot(model04)
ggplot(data = drivers)+
  geom_point(mapping = aes(x = Age, y = resid(model04)))

summary(model04)



