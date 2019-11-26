#### Exam 3 Practice ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute

install.packages("broom")

library(broom)

library(multcomp)

library(nlme)

# Load tidyverse
library("tidyverse")

# Check for updates
tidyverse_update()

install.packages(c("haven", "rvest"))

# reading in the data
library(readr)

#### 1 ####

peake <- read_csv("datasets/demos/peake.csv")
head(peake)

ggplot(data = peake) +
  geom_point(mapping = aes(x = AREA, y = SPECIES),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "AREA", y = "SPECIES")

ggplot(data = peake)+
  geom_histogram(aes(SPECIES), binwidth = 2)
ggplot(data = peake)+
  geom_boxplot(aes("", SPECIES))
ggplot(data = peake)+
  geom_qq(aes(sample = SPECIES))

#Correlation - not necessary for this
peakeCor <- cor.test(~ AREA + SPECIES, data = peake,
                      method = "pearson")
peakeCor

model01 <- lm(SPECIES ~ AREA, data = peake)

autoplot(model01, smooth.colour = NA)

peake_plus <- augment(model01)
ggplot(data =peake_plus)+
  geom_point(aes(x = AREA, y= .resid))

#transform the data