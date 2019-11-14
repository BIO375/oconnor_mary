#### Lab 9 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")

library("multcomp")

library("nlme")

# Load tidyverse
library("tidyverse")

# Check for updates
tidyverse_update()

install.packages(c("haven", "rvest"))

#### Correlation ####

# reading in the data
library(readr)
fowler <- read_csv("datasets/demos/fowler.csv")
head(fowler)

# Check for the assumption of bivariate normality using a basic scatter 
# plot
ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")
# The scatter plot shows no evidence of non-linearity and the cloud of
# points is elliptical in shape.  

#Last way to check for bivariate normality is to see whether x and y separately 
#are normally distributed.The sample size here is 10, so histogram is not useful.
# Look for normality in boxplots and qq-plots.

# y ~ YIELD
ggplot(data = fowler)+
  geom_boxplot(aes("", YIELD))
ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

# x ~ FERTILIZER
ggplot(data = fowler)+
  geom_boxplot(aes("", FERTILIZER))
ggplot(data = fowler)+
  geom_qq(aes(sample = FERTILIZER))

# Both fertilizer and yield are distributed normally.
# The median is located in the center of the boxplots for both, and the
# whisker lengths are relatively equal on either side of the box. 
# The Q-Q plots are both linear. A correlation will work,The parametric 
# (i.e., assuming bivariate normality) version of a correlation is named after some statistician
# named Pearson.  

fowlerCor <- cor.test(~ FERTILIZER + YIELD, data = fowler,
                     method = "pearson")
fowlerCor

# Based on this result, there is a significant positive association 
# between type of fertilizer used and grass yielded. (cor= 0.9600003)

# Because this data are intentionally manipulated (fertilizer manipulated to affect grass yield),
# we would realistically just proceed with a linear regression.

#### Linear Regression ####

# The assumptions for a linear regression are difficult to test directly,
# so we mostly diagnose departures using residuals plots.  

#In order to plot residuals, we first need to fit a model. y~x

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

# Use autoplot to give a residual by predicted plot in the upper left panel

autoplot(model01, smooth.colour = NA)

# Create a residual by x plot

# I am using the augment function

fowler_plus <- augment(model01)
ggplot(data = fowler_plus)+
  geom_point(aes(x = FERTILIZER, y= .resid))

# The normal Q-Q of the residuals looks like the residuals are normal (not much deviation), 
# which indicates that the data meets the assumptions of y being normally distributed for 
# each value of x.

# The residuals vs. fitted plot (also known as the residuals vs. predicted) indicates no fan 
# shape (actually there is no shape at all). This suggests that the data meets the 2nd 
# assumption of homogeneity of variance.

# Proceed with linear regression

# statistical results
summary(model01)

# p-value of interest is found in the row "FERTILIZER", the intercept
# and slope are found under the column header "Estimate".

# Larger loads of fertilizer yield significantly more grass.
#(Linear regression: YIELD = 51.933 + 0.81139(FERITLIZER);
# df = 1, 8, F=94.04, P<0.0001)
#fertilizer load size explained more than 90% of the variability in grass yield (R2 = 0.9216).

# Adding a regression line to our plot to give an idea about how confident we
# are in our estimate of that regression line
# Generate confidence bands
# The narrower the band, the more confident we are in our estimate of the line.

# Create confidence bands by adding in a layer : geom_smooth(method = "lm", level=0.95)

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")

# All of the data points fall within or nearly withing the added layer, therefore 
# we are confident in the estimate of our line (~95%). This confirms that the data meets
# the fourth assumption: there is a linear relationship between the predictor (x) and 
# response (y) variables.


