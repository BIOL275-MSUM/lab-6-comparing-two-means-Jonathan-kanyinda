
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

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
    bins = 15, 
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



