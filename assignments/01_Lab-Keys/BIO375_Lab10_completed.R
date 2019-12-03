#### Lab 10: Chi-squared and friends KEY #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# install.packages("ggmosaic")
library("ggmosaic")
# install.packages("epitools")
library("epitools")
# install.packages("tibble")
library("tibble")
# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

### Exact Binomial test ####
# For the Bergen op Zoom beetle population 
# I don't trust my mental math these days, so I get R to do arithmetic for me.
49+41 # n = total number of trials
# Success = Female
model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01



### Chi-squared goodness of fit ####

# Mendelian flowers example

df <- tibble(color = 
               c("yellow",
             "green")
             )
df
flower <- df %>% 
  mutate(count = c(84, 16)) 


# flower has a single categorical variable, color, that has 2 levels, yellow and green

flower <- add_column(flower, expected= c(75,25)) %>%
  mutate(expected_p = expected/100)

# All the expected values are greater than 5 so we meet the assumptions of the chi-sq goodness of fit test

model02 <-chisq.test(x = flower$count, p = flower$expected_p)
model02

### Chi-squared contingency analysis ####
# Willow beetle sex ratios in Europe example

tab01 <- matrix(c(30, 17, 41, 49), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Location" = c("Belgium", "Holland"),
                      "Sex" = c("Female", "Male"))
as.matrix(tab01)
model03 <- chisq.test(tab01, correct = FALSE)
model03
