
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

crab <- read_csv("chap15q27FiddlerCrabFans.csv")

# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# Question A --------------------------------------------------------------

t.test(formula = species ~ location, data = fish_long)

# Mean difference

Mean_difference = 16.41667 - 14.58333

Mean_difference # The mean difference is 1.83334

# 95% confidence intervall of Mean length difference

Upper_limit = -4.587031

Lower_limit = 8.253697


# Question B --------------------------------------------------------------

"The p value 0.5596 is greater than  0.05, therefore the null hypothesis will not be rejected"


# Question C --------------------------------------------------------------

"The assumption was that the data was normally distributed and that teh hump shaped were closed enough 
to be considered normally shaped"

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

"For Question A, the assumption made was that the presence of tributaries did not affect the local
number of electric fish in the main rivers (Also the Null hypothesis). For part B, the assumption 
was that the data was normally distributed and that the peaks/umps of the two locations 
would be close enough to generate an overall normal distribution.
Based on the graph,the distribution does not appear to be normal and therefore
does not meet teh assumption made about the normality."

# Question  D -------------------------------------------------------------

crab

crab %>% 
  filter(!is.na(crabType)) %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 10, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  )  + 
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4", "yellow")) +
  theme_minimal() +
  facet_wrap(~crabType,, ncol=1)

# Question E --------------------------------------------------------------


# ANOVA test

aov_body_temperature_crabType <-
  aov(bodyTemperature ~ crabType, data = crab)
aov_body_temperature_crabType

summary(aov_body_temperature_crabType) # summary of the ANOVA test


"The null hypothesis is that the mean body temparature would be the same in all four crab groups and,
teh alternative hypothesis is that the mean body temperature would be different in at least one of 
thefour groups.
Based on the graph, a difference in body temparature can be observed and females appeared to have a 
higher body temperature than that of the males in general."

"Based on the result of the ANOVA test, the p-value obtained is 7e-10 which would be lower than any
most likely alpha chosen, and thus will result un the rejection of the null hypothesis and come to 
the conclusion that the mean body temperature would be different in at least one of the four
crab species"






