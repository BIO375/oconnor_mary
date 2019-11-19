#### Exam 3 ####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggfortify")

library("ggfortify")

install.packages("broom")

library("broom")

library("multcomp")

library("nlme")

library("tidyverse")

tidyverse_update()

install.packages(c("haven", "rvest"))

library("haven")

library("rvest")

library("readr")

#### question 10 ####

aphids <- read_csv("datasets/exams/aphids.csv")

model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)

model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )

varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability

#### question 11 ####

glucose <- read_csv("datasets/exams/glucose.csv")

# Correlation

# Check for the assumption of bivariate normality using a basic scatter 
# plot
ggplot(data = glucose) +
  geom_point(mapping = aes(x = blood_glucose, y = HbA1c),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "Blood Glucose Levels", y = "Hb1Ac Values")

# The scatter plot shows no evidence of non-linearity and the cloud of
# points is elliptical in shape. 

ggplot(data = glucose)+
  geom_histogram(aes(blood_glucose), binwidth = .5)
ggplot(data = glucose)+
  geom_histogram(aes(HbA1c), binwidth = .5)

# sample size is probably too small to determine normality from histograms

ggplot(data = glucose)+
  geom_boxplot(aes("", blood_glucose))
ggplot(data = glucose)+
  geom_boxplot(aes("", HbA1c))
ggplot(data = glucose)+
  geom_qq(aes(sample = blood_glucose))
ggplot(data = glucose)+
  geom_qq(aes(sample = HbA1c))

# boxplots/ qq-plots indicate normality so proceeding with parametric correlation test


glucoseCor <- cor.test(~ blood_glucose + HbA1c, data = glucose,
                     method = "pearson")
glucoseCor
r <- glucoseCor$estimate
r

#### question 12 ####
DriverVision <- read_csv("datasets/exams/DriverVision.csv")
# Linear Regression

model01 <- lm(Distance ~ Age, data = DriverVision)

# Use autoplot to give a residual by predicted plot in the upper left panel

autoplot(model01, smooth.colour = NA)

# Create a residual by x plot

DriverVision_plus <- augment(model01)
ggplot(data = DriverVision_plus)+
  geom_point(aes(x =Age , y= .resid))

# The normal Q-Q of the residuals looks like the residuals are normal (not much deviation), 
# which indicates that the data meets the assumptions of y being normally distributed for 
# each value of x.

# The residuals vs. fitted plot (also known as the residuals vs. predicted) indicates no fan 
# shape (actually there is no shape at all). This suggests that the data meets the 2nd 
# assumption of homogeneity of variance.

# Proceed with linear regression

# statistical results

summary(model01)

# As driver age increases, long distance vision ability decreases.
#(Linear regression: distance = 576.68 - 3.0068(Age);
# df = 1, 28, F=50.21, P<0.0001)
#driver age explained more than 60% of the variability in long distance vision (R2 = 0.642).

ggplot(data = DriverVision, aes(x = Age, y = Distance)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Age", y = "Distance")

# All of the data points fall within or nearly withing the added layer, therefore 
# we are confident in the estimate of our line (~95%). This confirms that the data meets
# the fourth assumption: there is a linear relationship between the predictor (x) and 
# response (y) variables.
