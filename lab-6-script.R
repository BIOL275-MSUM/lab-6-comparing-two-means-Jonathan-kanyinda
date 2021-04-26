
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

# Estimation of the mean and addition of the 95% interval around the estimates

fish_long_summary <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

# Scatter plot of the data with added confidence intervalls 

fish_long %>% 
  ggplot(aes(x = location, y = species)) +
  geom_jitter(aes(color = location), 
              shape = 16, alpha = 0.3, width = 0.4) +
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                data = fish_long_summary, width = .1) +
  geom_point(aes(y = mean), 
             data = fish_long_summary) +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal() +
  guides(color = "none")

"Based on the graph, we can see that the confidednce intervals overlap, therefore the assumptions
were met"


