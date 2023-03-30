library(readxl)
library(tidyverse)
library(pander)

setwd("~/Documents/Data Science/Reef Renewal Bonaire")
carl3 <- read_excel("Carls Hill Monitoring Database.xlsx", 
                    sheet = 2, 
                    range = 'A2:Y86',
                    col_names = c("date", "duration", "species", "geno", "cluster", 
                                  "n_outplanted","n_present", "n_lost", "health_0",
                                  "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                  "perc_dead_tissue", "disease", "ciliate", "bottom_contact", "fireworm",
                                  "snail", "cliona", "competition", "unknown", "none", "n_stressors"))

### Calculating coral losses by geno
carl_losses <- carl3 %>%
    group_by(species, geno) %>%
    summarize(num_outplanted = sum(n_outplanted),
              num_survived = sum(n_present),
              mortality_rate = ((num_outplanted - num_survived) / num_outplanted) * 100) %>%
    arrange(desc(mortality_rate))
print(carl_losses, n = 22)

carl_losses_formatted <- carl_losses
carl_losses_formatted$mortality_rate <- paste0(round(carl_losses$mortality_rate, 2), "%")
carl_losses_formatted %>% pander()

### Calculating weighted coral health by geno
carl_health <- carl3 %>%
  select(geno, n_present, health_0, health_q1, health_q2, health_q3, health_q4, health_100) %>%
  rowwise() %>%
  mutate(Health0 = sum(health_0, na.rm = TRUE),
         Health25 = sum(health_q1 * 0.125, na.rm = TRUE),
         Health50 = sum(health_q2 * 0.375, na.rm = TRUE),
         Health75 = sum(health_q3 * 0.625, na.rm = TRUE),
         Health99 = sum(health_q4 * 0.875, na.rm = TRUE),
         Health100 = sum(health_100, na.rm = TRUE),
         weighted_health = (sum(Health0 + Health25 + Health50 + Health75 + 
                           Health99 + Health100, na.rm = TRUE) / n_present) * 100) 

carl_health_agg <- carl_health %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of net losses and weighted health by geno
loss_health_table <- carl_losses %>%
  inner_join(carl_health_agg, by = "geno") %>%
  select(species, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 
print(loss_health_table, n = 22)

    
### boxplot of tissue health by geno
ggplot(carl_health, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 5)) +
  labs(x = "Apal Genotype", 
       y = "Percent of Healthy Tissue",
       title = "Carl's Hill",
       subtitle = "3-Month Survey",
       caption = "Survey Date: 2022-04-27")

### boxplot of tissue decay by geno
ggplot(carl3, aes(x = factor(geno), y = (perc_dead_tissue * 100))) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(-5, 100)) +
  labs(x = "Apal Genotype", 
       y = "Percent of Tissue Decay",
       title = "Carl's Hill",
       subtitle = "3-Month Survey",
       caption =  "Survey Date: 2022-04-27")

