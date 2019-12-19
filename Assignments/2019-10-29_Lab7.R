#### Lab 7: Introduction to 1-way Anova####

rm(list = ls())

getwd()

#Already installed necessary packages at the beginning of lab below is the code necessary to execute
# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")

library("tidyverse")
tidyverse_update()

#### Question 1 ####

Jaffe<-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))

#Step 1: plot data

head(Jaffe)
summary(Jaffe)

ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = Aldrin))
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 0.5)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = HCB))
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.5)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

#boxplots, histogram, qqplot for Aldrin show signs of non-normality (best seen in exponential curve of qq plot)
# data for HCB appears to be more normally distributed

# Step 2: Use the function lm() to fit a model, specify equation & data (e.g., y ~ x, data = data)

#Aldrin
model01Aldrin <- lm(Aldrin~Depth, data = Jaffe)

#HCB
model01HCB <- lm(HCB~Depth, data = Jaffe)

# Step 3: Check Assumptions

#Aldrin
summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

#HCB
summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

# sd ratio for Aldrin was greater than 3 indicating that we must try to transform data
# sd ratio for HCB less than 3 so it's good to go

Jaffe<-mutate(Jaffe, log10_Aldrin = log10(Aldrin))

summ_log10_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_log10_Aldrin = mean(log10_Aldrin),
            sd_log10_Aldrin = sd(log10_Aldrin),
            n_log10_Aldrin = n())
ratio <-(max(summ_log10_Aldrin$sd_log10_Aldrin))/(min(summ_log10_Aldrin$sd_log10_Aldrin))

#ratio is now less than 3, plotting the mutated data to check for normality

ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = log10_Aldrin))
ggplot(Jaffe) +
  geom_histogram(aes(log10_Aldrin), binwidth = 0.1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = log10_Aldrin, color = Depth))

# plots show signs of normality to procede with all the assumptions met for the transformed Aldrin Data

# Using the function lm() to fit a model to transformed data
model02Aldrin <- lm(log10_Aldrin~Depth, data = Jaffe)

#Step 4: Check assumptions using residuals plot

# Aldrin non-transformed
autoplot(model01Aldrin)

# Aldrin transformed
autoplot(model02Aldrin)

#HCB
autoplot(model01HCB)

#transformed Aldrin and HCB appear to meet assumptions for parametric tests

# Step 5: use the functions anova() and summary() to interpret statistical results for data that meets assumptions

#If assumptions are not met, try data transformation and/or a non-parametric or robust version of the test
# I already transformed Aldrin data to meet assumptions, see work above

# Is there a difference in Aldrin concentration at different depths?
#non-transformed Aldrin
anova(model01Aldrin)

#transformed Aldrin
anova(model02Aldrin)

# summary of model results
#non-transformed Aldrin
summary(model01Aldrin)

#transformed Aldrin
summary(model02Aldrin)

# Is there a difference in HCB concentrations at different depths?

anova(model01HCB)

# according to the 1-way ANOVA tests the only data with a significant difference is the log transformed Aldrin data

#Step 6: Unplanned Comparisons (Tukey's HSD)
# log transformed Aldrin to see which pairs are significantly different

tukey <- glht(model02Aldrin, linfct = mcp(Depth = "Tukey"))
summary(tukey)

#only a significant difference between Bottom and Surface in this data

#### 10/10 code runs without breaking ####